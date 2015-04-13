package com.faacets.qalg
package std

import scala.{specialized => sp}
import scala.reflect.{classTag, ClassTag}

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.std.long._
import spire.syntax.ring._

import algebra._
import syntax.vec._

trait SeqVecBuilder[@sp(Double, Long) A] extends Any
    with VecBuilder[Seq[A], A] {
  implicit def classTagA: ClassTag[A]
  type V = Seq[A]
  def V: Vec[V, A] = this
  def apply(v: V, k: Int): A = v(k)
  def length(v: V): Int = v.length
  def tabulate(n: Int)(f: Int => A): V = Seq.tabulate(n)(f)
}

trait SeqVecInRing[@sp(Double, Long) A] extends Any
    with SeqVecBuilder[A]
    with VecInRing[Seq[A], A] {
  override def negate(v: Seq[A]): Seq[A] = v.map(-_)
  override def plus(x: Seq[A], y: Seq[A]): Seq[A] = (x zip y).map { case (xx, yy) => xx + yy }
  override def minus(x: Seq[A], y: Seq[A]): Seq[A] = (x zip y).map { case (xx, yy) => xx - yy }
  override def timesl(r: A, v: Seq[A]): Seq[A] = v.map(r * _)
}

trait SeqVecInField[@sp(Double, Long) A] extends Any
    with SeqVecInRing[A]
    with VecInField[Seq[A], A]

trait SeqInstances0 {
  implicit def SeqVecBuilder[A: ClassTag: Eq] = new SeqVecBuilder[A] {
    def eqA = Eq[A]
    def classTagA = implicitly[ClassTag[A]]
  }
}

trait SeqInstances extends SeqInstances0 {
  implicit val SeqDouble = new SeqVecInField[Double] {
    def classTagA = classTag[Double]
    def eqA = Eq[Double]
    def scalar = Field[Double]
  }
  implicit val SeqRational = new SeqVecInField[Rational] {
    def classTagA = classTag[Rational]
    def eqA = Eq[Rational]
    def scalar = Field[Rational]
  }
  implicit val SeqLong = new SeqVecInRing[Long] {
    def classTagA = classTag[Long]
    def eqA = Eq[Long]
    def scalar = Ring[Long]
  }
}
