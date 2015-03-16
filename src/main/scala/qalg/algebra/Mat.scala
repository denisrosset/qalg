package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait Mat[MA, @sp(Double, Long) A] extends Any with Eq[MA] { self =>
  implicit def eqA: Eq[A]
  def size(m: MA): IntInt = IntInt(nRows(m), nCols(m))
  def nRows(m: MA): Int
  def nCols(m: MA): Int
  def apply(m: MA, r: Int, c: Int): A
    def eqv(x: MA, y: MA): Boolean =
    (nRows(x) == nRows(y)) && (nCols(x) == nCols(y)) && {
      val nR = nRows(x)
      val nC = nCols(x)
      var r = 0
      var c = nC
      while (r < nR && c == nC) {
        c = 0
        while (c < nC && apply(x, r, c) === apply(y, r, c)) {
          c += 1
        }
        r += 1
      }
      r == nR
    }
  def vec[VA](m: MA, rows: ::.type, c: Int)(implicit VA: VecBuilder[VA, A]): VA =
    VA.from(new FunV[A] {
      def len: Int = self.nRows(m)
      def f(r: Int): A = self.apply(m, r, c)
    })
  def vec[VA](m: MA, rows: At1, c: Int)(implicit VA: VecBuilder[VA, A]): VA =
    VA.from(new FunV[A] {
      def len: Int = rows.length
      def f(r: Int): A = self.apply(m, rows(r), c)
    })
  def vec[VA](m: MA, r: Int, cols: ::.type)(implicit VA: VecBuilder[VA, A]): VA =
    VA.from(new FunV[A] {
      def len: Int = self.nCols(m)
      def f(c: Int): A = self.apply(m, r, c)
    })
  def vec[VA](m: MA, r: Int, cols: At1)(implicit VA: VecBuilder[VA, A]): VA =
    VA.from(new FunV[A] {
      def len: Int = cols.length
      def f(c: Int): A = self.apply(m, r, cols(c))
    })
}

trait MatBuilder[MA, @sp(Double, Long) A] extends Any with Mat[MA, A] { self =>
  implicit def scalar: AdditiveMonoid[A]

  def from(m: FunM[A]): MA

  def apply(m: MA, rows: At1, cols: At1): MA = from(new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, rows(r), cols(c))
  })
  def apply(m: MA, rows: ::.type, cols: ::.type): MA = m
  def apply(m: MA, rows: ::.type, cols: At1): MA = from(new FunM[A] {
    def nR: Int = self.nRows(m)
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, r, cols(c))
  })
  def apply(m: MA, rows: At1, cols: ::.type): MA = from(new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = self.nCols(m)
    def f(r: Int, c: Int): A = self.apply(m, rows(r), c)
  })
  def t(m: MA): MA = from(new FunM[A] {
    def nR: Int = self.nCols(m)
    def nC: Int = self.nRows(m)
    def f(r: Int, c: Int): A = self.apply(m, c, r)
  })
}

object MatBuilder {
  def apply[MA, @sp(Double, Long) A](implicit MB: MatBuilder[MA, A]): MatBuilder[MA, A] = MB
}

trait MatMutable[MA, @sp(Double, Long) A] extends Any { self =>
  def update(m: MA, r: Int, c: Int, a: A): Unit
}

trait MatInRing[MA, @sp(Double, Long) A] extends Any with MatBuilder[MA, A] with Module[MA, A] with MultiplicativeSemigroup[MA] {
  implicit def scalar: Ring[A]
  def zero: MA = from(FunM.empty[A])
}

trait MatInField[MA, @sp(Double, Long) A] extends Any with MatInRing[MA, A] with VectorSpace[MA, A] {
  implicit def scalar: Field[A]
}
