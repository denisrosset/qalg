package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatBuilder[MA, @sp(Double, Long) A] extends Any with Mat[MA, A] { self =>
  implicit def scalar: AdditiveMonoid[A]

  def from(m: FunM[A]): MA

  def apply(m: MA, rows: At1, cols: At1): MA = from(view(m, rows, cols))
  def apply(m: MA, rows: ::.type, cols: ::.type): MA = m
  def apply(m: MA, rows: ::.type, cols: At1): MA = from(view(m, rows, cols))
  def apply(m: MA, rows: At1, cols: ::.type): MA = from(view(m, rows, cols))

  def t(m: MA): MA = from(viewT(m))
}

object MatBuilder {
  def apply[MA, @sp(Double, Long) A](implicit MB: MatBuilder[MA, A]): MatBuilder[MA, A] = MB
}
