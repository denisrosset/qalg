package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait Mat[M, @sp(Double, Long) A] extends Any with Lin[M, A] with Index2[M, A] { self =>
  def sameShape(x: M, y: M): Boolean = (nRows(x) == nRows(y)) && (nCols(x) == nCols(y))
  def linearLength(m: M): Int = nRows(m) * nCols(m)
  def linearApply(m: M, k: Int): A = {
    val nC = nCols(m)
    val r = k / nC
    val c = k % nC
    apply(m, r, c)
  }
  def size(m: M): IntInt = IntInt(nRows(m), nCols(m))
  def nRows(m: M): Int
  def nCols(m: M): Int
  def view(m: M, genRows: At1, genCols: At1): FunM[A] = {
    val rows = genRows.forRowsOf(m)(self)
    val cols = genCols.forColsOf(m)(self)
    new FunM[A] {
      def nR: Int = rows.length
      def nC: Int = cols.length
      def f(r: Int, c: Int): A = self.apply(m, rows(r), cols(c))
    }
  }
  def view(m: M, genRows: At1, c: Int): FunV[A] = {
    val rows = genRows.forRowsOf(m)(self)
    new FunV[A] {
      def len: Int = rows.length
      def f(r: Int): A = self.apply(m, rows(r), c)
    }
  }
  def view(m: M, r: Int, genCols: At1): FunV[A] = {
    val cols = genCols.forColsOf(m)(self)
    new FunV[A] {
      def len: Int = cols.length
      def f(c: Int): A = self.apply(m, r, cols(c))
    }
  }
  def viewT(m: M): FunM[A] = new FunM[A] {
    def nR: Int = self.nCols(m)
    def nC: Int = self.nRows(m)
    def f(r: Int, c: Int): A = self.apply(m, c, r)
  }
}

object Mat {
  def apply[M, @sp(Double, Long) A](implicit M: Mat[M, A]): Mat[M, A] = M
  implicit def fromPack[M, @sp(Double, Long) A](implicit ev: PackMR[M, A]): Mat[M, A] = ev.M
}
