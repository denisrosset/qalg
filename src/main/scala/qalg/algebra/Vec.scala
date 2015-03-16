package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.cfor._
import util._

trait Vec[VA, @sp(Double, Long) A] extends Any { self =>
  def length(v: VA): Int
  def apply(v: VA, k: Int): A
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
  implicit def eqA: Eq[A]
  def from(v: FunV[A]): VA
  def apply(v: VA, at: At1): VA = from(new FunV[A] {
    def len: Int = at.length
    def f(k: Int): A = self.apply(v, at(k))
  })
}

trait VecMutable[VA, @sp(Double, Long) A] extends Any { self =>
  def update(v: VA, k: Int, a: A): Unit
}

trait VecModule[VA, @sp(Double, Long) A] extends Any with VecBuilder[VA, A] with Module[VA, A] {
  implicit def scalar: Ring[A]
}

trait VecVectorSpace[VA, @sp(Double, Long) A] extends Any with VecModule[VA, A] with VectorSpace[VA, A] {
  implicit def scalar: Field[A]
}
