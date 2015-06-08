package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait Mat[M, @sp(Double, Long) A] extends Any with Lin[M, A] { self =>
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
  def apply(m: M, r: Int, c: Int): A
  def view(m: M, rows: At1, cols: At1): FunM[A] = new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, rows(r), cols(c))
  }
  def view(m: M, rows: ::.type, cols: ::.type): FunM[A] = new FunM[A] {
    def nR: Int = self.nRows(m)
    def nC: Int = self.nCols(m)
    def f(r: Int, c: Int): A = self.apply(m, r, c)
  }
  def view(m: M, rows: ::.type, cols: At1): FunM[A] = new FunM[A] {
    def nR: Int = self.nRows(m)
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, r, cols(c))
  }
  def view(m: M, rows: At1, cols: ::.type): FunM[A] = new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = self.nCols(m)
    def f(r: Int, c: Int): A = self.apply(m, rows(r), c)
  }
  def view(m: M, rows: ::.type, c: Int): FunV[A] = new FunV[A] {
    def len: Int = self.nRows(m)
    def f(r: Int): A = self.apply(m, r, c)
  }
  def view(m: M, rows: At1, c: Int): FunV[A] =new FunV[A] {
    def len: Int = rows.length
    def f(r: Int): A = self.apply(m, rows(r), c)
  }
  def view(m: M, r: Int, cols: ::.type): FunV[A] = new FunV[A] {
    def len: Int = self.nCols(m)
    def f(c: Int): A = self.apply(m, r, c)
  }
  def view(m: M, r: Int, cols: At1): FunV[A] = new FunV[A] {
    def len: Int = cols.length
    def f(c: Int): A = self.apply(m, r, cols(c))
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
