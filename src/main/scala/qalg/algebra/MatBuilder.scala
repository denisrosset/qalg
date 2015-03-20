package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatBuilder[M, @sp(Double, Long) A] extends Any with Mat[M, A] { self =>
  implicit def scalar: AdditiveMonoid[A]

  def from(m: FunM[A]): M

  def apply(m: M, rows: At1, cols: At1): M = from(view(m, rows, cols))
  def apply(m: M, rows: ::.type, cols: ::.type): M = m
  def apply(m: M, rows: ::.type, cols: At1): M = from(view(m, rows, cols))
  def apply(m: M, rows: At1, cols: ::.type): M = from(view(m, rows, cols))

  def t(m: M): M = from(viewT(m))
}

object MatBuilder {
  def apply[M, @sp(Double, Long) A](implicit M: MatBuilder[M, A]): MatBuilder[M, A] = M
}
