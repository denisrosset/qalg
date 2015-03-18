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

  def size(m: MA): IntInt = IntInt(nRows(m), nCols(m))
  def nRows(m: MA): Int
  def nCols(m: MA): Int
  def apply(m: MA, r: Int, c: Int): A
  def view(m: MA, rows: At1, cols: At1): FunM[A] = new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, rows(r), cols(c))
  }
  def view(m: MA, rows: ::.type, cols: ::.type): FunM[A] = new FunM[A] {
    def nR: Int = self.nRows(m)
    def nC: Int = self.nCols(m)
    def f(r: Int, c: Int): A = self.apply(m, r, c)
  }
  def view(m: MA, rows: ::.type, cols: At1): FunM[A] = new FunM[A] {
    def nR: Int = self.nRows(m)
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, r, cols(c))
  }
  def view(m: MA, rows: At1, cols: ::.type): FunM[A] = new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = self.nCols(m)
    def f(r: Int, c: Int): A = self.apply(m, rows(r), c)
  }
  def view(m: MA, rows: ::.type, c: Int): FunV[A] = new FunV[A] {
    def len: Int = self.nRows(m)
    def f(r: Int): A = self.apply(m, r, c)
  }
  def view(m: MA, rows: At1, c: Int): FunV[A] =new FunV[A] {
    def len: Int = rows.length
    def f(r: Int): A = self.apply(m, rows(r), c)
  }
  def view(m: MA, r: Int, cols: ::.type): FunV[A] = new FunV[A] {
    def len: Int = self.nCols(m)
    def f(c: Int): A = self.apply(m, r, c)
  }
  def view(m: MA, r: Int, cols: At1): FunV[A] = new FunV[A] {
    def len: Int = cols.length
    def f(c: Int): A = self.apply(m, r, cols(c))
  }
  def viewT(m: MA): FunM[A] = new FunM[A] {
    def nR: Int = self.nCols(m)
    def nC: Int = self.nRows(m)
    def f(r: Int, c: Int): A = self.apply(m, c, r)
  }
}

object Mat {
  def apply[MA, @sp(Double, Long) A](implicit M: Mat[MA, A]): Mat[MA, A] = M
}
