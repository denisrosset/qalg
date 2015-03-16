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
  def length(v: VA): Int
  def apply(v: VA, k: Int): A
  def eqv(x: VA, y: VA): Boolean =
    (length(x) == length(y)) && {
      var i = 0
      val n = length(x)
      while (i < n && apply(x, i) === apply(y, i)) {
        i += 1
      }
      i == n
    }
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

trait VecBuilder[VA, @sp(Double, Long) A] extends Any with Vec[VA, A] { self =>
  implicit def scalar: AdditiveMonoid[A]
  def from(v: FunV[A]): VA
  def apply(v: VA, at: At1): VA = from(new FunV[A] {
    def len: Int = at.length
    def f(k: Int): A = self.apply(v, at(k))
  })
}

trait VecMutable[VA, @sp(Double, Long) A] extends Any { self =>
  def update(v: VA, k: Int, a: A): Unit
}

trait VecInRing[VA, @sp(Double, Long) A] extends Any with VecBuilder[VA, A] with Module[VA, A] {
  implicit def scalar: Ring[A]
  def zero: VA = from(FunV.empty[A])
}

trait VecInField[VA, @sp(Double, Long) A] extends Any with VecInRing[VA, A] with VectorSpace[VA, A] {
  implicit def scalar: Field[A]
}
