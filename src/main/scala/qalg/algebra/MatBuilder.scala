package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatBuilder[M, @sp(Double, Long) A] extends Any with Mat[M, A] { self =>
  implicit def scalar: AdditiveMonoid[A]

  def fromFunM(m: FunM[A]): M

  def apply(m: M, rows: At1, cols: At1): M = fromFunM(view(m, rows, cols))
  def apply(m: M, rows: ::.type, cols: ::.type): M = m
  def apply(m: M, rows: ::.type, cols: At1): M = fromFunM(view(m, rows, cols))
  def apply(m: M, rows: At1, cols: ::.type): M = fromFunM(view(m, rows, cols))

  def t(m: M): M = fromFunM(viewT(m))

  def build(nRows: Int, nCols: Int, elements: A*): M = fromFunM(new FunM[A] {
    def nR = nRows
    def nC = nCols
    def f(r: Int, c: Int): A = elements(r * nCols + c)
  })
}

object MatBuilder {
  def apply[M, @sp(Double, Long) A](implicit M: MatBuilder[M, A]): MatBuilder[M, A] = M
}

trait ConvertedMatBuilder[M, @sp(Double, Long) A, J] extends Any
    with ConvertedMat[M, A, J]
    with MatBuilder[M, A] {
  def source: MatBuilder[M, J]

  def fromFunM(m: FunM[A]): M = source.fromFunM(new FunM[J] {
    def nR: Int = m.nR
    def nC: Int = m.nC
    def f(r: Int, c: Int): J = aToJ(m.f(r, c))
  })

  def from[M1](m: M1)(implicit M1: Mat[M1, A]): M = fromFunM(M1.view(m, ::, ::))

  override def apply(m: M, rows: At1, cols: At1): M = source(m, rows, cols)
  override def apply(m: M, rows: ::.type, cols: ::.type): M = source(m, rows, cols)
  override def apply(m: M, rows: ::.type, cols: At1): M = source(m, rows, cols)
  override def apply(m: M, rows: At1, cols: ::.type): M = source(m, rows, cols)

  override def t(m: M): M = source.t(m)
}
