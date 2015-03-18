package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait Vec[VA, @sp(Double, Long) A] extends Any with Eq[VA] { self =>
  implicit def eqA: Eq[A]

  def eqv(x: VA, y: VA): Boolean =
    (length(x) == length(y)) && {
      var i = 0
      val n = length(x)
      while (i < n && apply(x, i) === apply(y, i)) {
        i += 1
      }
      i == n
    }

  def length(v: VA): Int
  def apply(v: VA, k: Int): A
  def toIndexedSeq(v: VA): IndexedSeq[A] = new IndexedSeq[A] {
    def length: Int = self.length(v)
    def apply(k: Int): A = self.apply(v, k)
  }
  def rowMat[MA](v: VA)(implicit M: MatBuilder[MA, A]): MA = M.from(new FunM[A] {
    def nR: Int = 1
    def nC: Int = self.length(v)
    def f(r: Int, c: Int): A = self.apply(v, c)
  })
  def colMat[MA](v: VA)(implicit M: MatBuilder[MA, A]): MA = M.from(new FunM[A] {
    def nR: Int = self.length(v)
    def nC: Int = 1
    def f(r: Int, c: Int): A = self.apply(v, r)
  })
}
