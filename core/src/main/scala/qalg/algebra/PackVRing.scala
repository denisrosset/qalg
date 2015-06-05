package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackVRing[V, @sp(Double, Long) A] extends Any {
  implicit def V: VecInRing[V, A]
  type MutM
  type MutV
  def toMutV(v: V): MutV = MutMutV.copy(unsafeToMutV(v))
  def fromMutV(v: MutV): V = unsafeFromMutV(MutMutV.copy(v))
  def unsafeToMutV(v: V): MutV
  def unsafeFromMutV(v: MutV): V
  implicit def MutMutM: MatMutable[MutM, A]
  implicit def MutMutV: VecMutable[MutV, A]
  implicit def MutM: MatInRing[MutM, A]
  implicit def MutV: VecInRing[MutV, A]
  implicit def MutProduct: MatVecProduct[MutM, MutV]
  implicit def MutSlicer: MatSlicer[MutM, MutV]
}
