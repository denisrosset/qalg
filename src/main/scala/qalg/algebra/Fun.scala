package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.additiveMonoid._
import spire.syntax.cfor._

import algebra._

trait FunV[@sp(Double, Long) A] extends Any {
  override def toString = (0 until len).map(f(_)).mkString("(", ",", ")")
  def f(k: Int): A
  def len: Int
  /** Returns the next non-zero element after index `k`, or -1 if there is no such element. */
  def nextNZ(k: Int)(implicit am: AdditiveMonoid[A], eq: Eq[A]): Int = {
    cforRange(k + 1 until len) { newK =>
      if (!f(newK).isZero) return newK
    }
    -1
  }
}

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

class FunAlgebra[@sp(Double, Long) A](implicit val AdditiveMonoidA: AdditiveMonoid[A], val EqA: Eq[A])
    extends BuildableMatVec[FunM, FunV, A] {
  implicit def FunMA: FunAlgebra[A] = this
  implicit def FunVA: FunAlgebra[A] = this
  def length(v: FunV[A]): Int = v.len
  def nRows(m: FunM[A]): Int = m.nR
  def nCols(m: FunM[A]): Int = m.nC
  def apply(v: FunV[A], k: Int): A = v.f(k)
  def apply(m: FunM[A], r: Int, c: Int): A = m.f(r, c)
  def fromMat[M1[_], V1[_]](m1: M1[A])(implicit M1: Mat[M1, V1, A]): FunM[A] = m1 match {
    case funM: FunM[A] => funM
    case _ => new FunM[A] {
      def nR: Int = M1.nRows(m1)
      def nC: Int = M1.nCols(m1)
      def f(r: Int, c: Int): A = M1.apply(m1, r, c)
    }
  }
  def fromVec[V1[_]](v1: V1[A])(implicit V1: Vec[V1, A]): FunV[A] = v1 match {
    case funV: FunV[A] => funV
    case _ => new FunV[A] {
      def len: Int = V1.length(v1)
      def f(k: Int): A = V1.apply(v1, k)
    }
  }
}
