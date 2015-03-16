package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.additiveMonoid._
import spire.syntax.cfor._

import algebra._

trait FunM[@sp(Double, Long) A] extends Any {
  override def toString = (0 until nR).map(r => (0 until nC).map(c => f(r, c)).mkString("[", " ", "]")).mkString("\n") // TODO: better presentation
  /** Returns the next non-zero matrix element in column `c`, after row `r`,
    * or -1 if there is no such element.
    */
  def nextNZInCol(r: Int, c: Int)(implicit am: AdditiveMonoid[A], eq: Eq[A]): Int = {
    cforRange(r + 1 until nR) { newR =>
      if (!f(newR, c).isZero) return newR
    }
    -1
  }
  /** Returns the next non-zero matrix element in row `r`, after column `c`,
    * or -1 if there is no such element.
    */
  def nextNZInRow(r: Int, c: Int)(implicit am: AdditiveMonoid[A], eq: Eq[A]): Int = {
    cforRange(c + 1 until nC) { newC =>
      if (!f(r, newC).isZero) return newC
    }
    -1
  }
  def f(r: Int, c: Int): A
  def nR: Int
  def nC: Int
}

object FunM {
  def empty[@sp(Double, Long) A]: FunM[A] = new FunM[A] {
    def nR = 0
    def nC = 0
    def f(r: Int, c: Int) = sys.error("Cannot get element of empty matrix.")
  }
  def fill[@sp(Double, Long) A](nRows: Int, nCols: Int)(a: A): FunM[A] = new FunM[A] {
    def nR = nRows
    def nC = nCols
    def f(r: Int, c: Int) = a
  }
  def fillDiag[@sp(Double, Long) A](nRows: Int, nCols: Int)(diag: A, nonDiag: A): FunM[A] = new FunM[A] {
    def nR = nRows
    def nC = nCols
    def f(r: Int, c: Int) = if (r == c) diag else nonDiag
  }
}

class FunMat[@sp(Double, Long) A](implicit val scalar: AdditiveMonoid[A], val eqA: Eq[A]) extends MatBuilder[FunM[A], A] {
  def nRows(m: FunM[A]): Int = m.nR
  def nCols(m: FunM[A]): Int = m.nC
  def apply(m: FunM[A], r: Int, c: Int): A = m.f(r, c)
  def from(m: FunM[A]): FunM[A] = m
}
