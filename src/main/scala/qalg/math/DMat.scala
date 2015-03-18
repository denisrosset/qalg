package com.faacets.qalg
package math

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.math.Rational
import spire.std.double._
import spire.syntax.field._

import algebra._

/** Dense matrix stored in row-major order. */
final class DMat[@sp(Double, Long) A: ClassTag](val nR: Int, val nC: Int, val array: Array[A]) {
  override def toString = math.MatrixPrinting.print(nR, nC, (r: Int, c: Int) => array(r * nC + c).toString)
}

trait DMatMat[@sp(Double, Long) A] extends Any
    with MatBuilder[DMat[A], A]
    with MatMutable[DMat[A], A] {
  implicit def classTag: ClassTag[A]
  def apply(m: DMat[A], r: Int, c: Int): A = m.array(r * m.nC + c)
  def update(m: DMat[A], r: Int, c: Int, a: A): Unit = { m.array(r * m.nC + c) = a }
  def nRows(m: DMat[A]): Int = m.nR
  def nCols(m: DMat[A]): Int = m.nC
}

/*
  def kron(x: MA, y: MA): MA = {
     val res = mutable.QMatrix.zeros(a.rows * b.rows, a.cols * b.cols)
     // TODO cfor
     for (r <- 0 until a.rows; c <- 0 until a.cols; av = a(r, c))
     res((r * b.rows) until ((r+1) * b.rows), (c * b.cols) until ((c+1) * b.cols)) = ms.timesl(av, b)
     a.factory.unsafeBuild(res)
     }
    ???
  }
 */

trait DMatMatInRing[@sp(Double, Long) A] extends Any with DMatMat[A] with MatInRing[DMat[A], A] {
  def from(m: FunM[A]): DMat[A] = {
    val nR = m.nR
    val nC = m.nC
    val array = new Array[A](nR * nC)
    var i = 0
    var r = 0
    while (r < nR) {
      var c = 0
      while (c < nC) {
        array(i) = m.f(r, c)
        c += 1
        i += 1
      }
      r += 1
    }
    new DMat(nR, nC, array)
  }
  override def negate(m: DMat[A]): DMat[A] = new DMat(m.nR, m.nC, std.ArraySupport.negate(m.array))
  override def plus(x: DMat[A], y: DMat[A]): DMat[A] = {
    require(x.nR == y.nR && x.nC == y.nC)
    new DMat(x.nR, x.nC, std.ArraySupport.plus(x.array, y.array))
  }
  override def minus(x: DMat[A], y: DMat[A]): DMat[A] = {
    require(x.nR == y.nR && x.nC == y.nC)
    new DMat(x.nR, x.nC, std.ArraySupport.minus(x.array, y.array))
  }
  override def timesl(x: A, y: DMat[A]): DMat[A] =
    new DMat(y.nR, y.nC, std.ArraySupport.timesl(x, y.array))
  override def times(x: DMat[A], y: DMat[A]): DMat[A] = {
    val xR = x.nR
    val yR = y.nR
    val xC = x.nC
    val yC = y.nC
    val xa = x.array
    val ya = y.array
    val nR = xR
    val nC = yC
    val nK = xC
    require(yR == nK)
    val array = new Array[A](nR * nC)
    var r = 0
    while (r < nR) {
      var c = 0
      while (c < nC) {
        var acc = xa(r * xC)*ya(c) // x(r)(0)*y(0)(c)
        var k = 1
        while (k < nK) {
          acc += xa(r * xC + k)*ya(k * yC + c) // x(r)(k)*y(k)(c)
          k += 1
        }
        array(r * nC + c) = acc
        c += 1
      }
      r += 1
    }
    new DMat(nR, nC, array)
  }
}

trait DMatMatInField[@sp(Double, Long) A] extends Any with DMatMatInRing[A] with MatInField[DMat[A], A]

/*
trait DMatVecBuilder[@sp(Double, Long) A] extends Any with MatVecBuilder[DMat[A], DVec[A], A] {

 }*/

object DMat {
  implicit val DMatDouble = new DMatMatInField[Double] {
    def classTag = scala.reflect.classTag[Double]
    def scalar = Field[Double]
    def eqA = Eq[Double]
  }
  implicit val DMatRational = new DMatMatInField[Rational] {
    def classTag = scala.reflect.classTag[Rational]
    def scalar = Field[Rational]
    def eqA = Eq[Rational]
  }
  /*
  implicit val DMatVecBuilderDouble = new DMatVecBuilder[Double] {
    def MA = DMatDouble
    def VA = DVec.DVecDouble
    def apply(m: DMat[Double], r: Int, cols: At1): DVec[Double] = VA.from(new FunV[Double] {
      def len: Int = cols.length
      def f(c: Int): Double = MA.apply(m, r, cols(c))
    })
  }*/
}
