package com.faacets.qalg.indup
package algebra

sealed trait Validity
sealed trait Valid extends Validity
sealed trait Invalid extends Validity

final class RawOffset[+V <: Validity](val o: Long) extends AnyVal {
  @inline final def isNull = o == -1L
  @inline final def nonNull = !isNull
  // name-based extractor
  @inline final def isEmpty = o == -1L
  @inline final def get: RawOffset[Valid] = this.asInstanceOf[RawOffset[Valid]]
}

object Offset {
  @inline final def unapply[V <: Validity](raw: RawOffset[V]): RawOffset[V] = raw
  @inline final def apply(o: Long): Offset = new RawOffset[Valid](o)
}

trait OffsetAliases {
  type OptOffset = RawOffset[Validity]
  type Offset = RawOffset[Valid]
  type NoOffset = RawOffset[Invalid]
  final def NoOffset: NoOffset = new RawOffset[Invalid](-1L)
}
