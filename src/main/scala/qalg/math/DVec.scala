package com.faacets.qalg
package math

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.syntax.field._

import algebra._

final class DVec[@sp(Double, Long) A: ClassTag](val len: Int, val array: Array[A]) {
  override def toString = math.MatrixPrinting.print(1, len, (r: Int, c: Int) => array(c).toString)
}

trait DVecVec[@sp(Double, Long) A] extends Any
    with VecBuilder[DVec[A], A]
    with VecMutable[DVec[A], A] {
  implicit def classTag: ClassTag[A]
  def length(v: DVec[A]): Int = v.len
  def apply(v: DVec[A], k: Int): A = v.array(k)
  def update(v: DVec[A], k: Int, a: A): Unit = { v.array(k) = a }
  def fromArray(a: Array[A]): DVec[A] = new DVec(a.length, a)
}

trait DVecVecInRing[@sp(Double, Long) A] extends Any with DVecVec[A] with VecInRing[DVec[A], A] {
  def from(v: FunV[A]): DVec[A] = new DVec(v.len, Array.tabulate(v.len)(k => v.f(k)))
  def negate(v: DVec[A]): DVec[A] = fromArray(std.ArraySupport.negate(v.array))
  def plus(x: DVec[A], y: DVec[A]): DVec[A] =
    fromArray(std.ArraySupport.plus(x.array, y.array))
  override def minus(x: DVec[A], y: DVec[A]): DVec[A] =
    fromArray(std.ArraySupport.minus(x.array, y.array))
  def timesl(x: A, y: DVec[A]): DVec[A] =
    fromArray(std.ArraySupport.timesl(x, y.array))
}

trait DVecVecInField[@sp(Double, Long) A] extends Any with DVecVecInRing[A] with VecInField[DVec[A], A]

object DVec {
  implicit val DVecDouble = new DVecVecInField[Double] {
    def classTag = scala.reflect.classTag[Double]
    def scalar = Field[Double]
    def eqA = Eq[Double]
  }
  implicit val DVecRational = new DVecVecInField[Rational] {
    def classTag = scala.reflect.classTag[Rational]
    def scalar = Field[Rational]
    def eqA = Eq[Rational]
  }
}
