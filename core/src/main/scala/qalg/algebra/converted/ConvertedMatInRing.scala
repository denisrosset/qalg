package com.faacets.qalg
package algebra
package converted

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import spire.syntax.ring._
import util._

trait ConvertedMatInRing[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMatBuilder[M, A, J]
    with MatInRing[M, A] {
  def source: MatInRing[M, J]

  override def zero: M = source.zero
  override def id: M = source.id
  override def plus(x: M, y: M): M = source.plus(x, y)
  override def minus(x: M, y: M): M = source.minus(x, y)
  override def times(x: M, y: M): M = source.times(x, y)
  override def negate(m: M): M = source.negate(m)
  override def timesl(a: A, m: M): M = source.timesl(aToJ(a), m)
}
