package com.faacets

import scala.annotation.tailrec
import scala.math.abs
import spire.math.{Rational, SafeLong, lcm}
import language.implicitConversions

package object alg {
  // factorizes integer n
  def factor(n: Int): List[Int] = {
    // finds a divisor for n greater or equal to k
    @tailrec def ldf(k : Int, n : Int) : Int = {
      if (n % k == 0) k
      else if ((k*k) > n) n // divisor must be <= sqrt(n)
      else ldf((k+1), n) // try next divisor
    }

    n match {
      case 1 => Nil;
      case _ => {
        val p = ldf(2, n) // find a divisor, starts with 2
        p :: factor(n / p)
      }
    }
  }

  def asSafeLongs(array: Array[Rational]): Array[SafeLong] = {
    val factor = array.map(_.denominator).fold(BigInt(1))(lcm)
    array.map(rat => SafeLong(rat.numerator * (factor / rat.denominator)))
  }

  def asCoprimeSafeLongs(array: Array[Rational]): Array[SafeLong] = {
    val safeLongArray = asSafeLongs(array)
    var a = SafeLong.zero
    for (v <- safeLongArray) {
      if (a == 0)
        a = v.abs
      else {
        var b = v.abs
        while (b != 0) {
          val t = b
          b = a % t
          a = t
        }
      }
    }
    if (a == 0)
      safeLongArray
    else
      safeLongArray.map(_ / a)
  }

  def parseCoefficients(S: String): Array[Int] = S.split("\\s+").map(_.toInt)

  ///////////////////////////////////////
  // vvv Matlab-like mathematical functions
  /**
   * Returns the Kronecker product of the two matrices a and b,
   * usually denoted a ⊗ b.
   */
  def kronAB[M <: alg.QMatrixLike[M, _]](a: alg.QMatrix with M, b: alg.QMatrix with M): alg.QMatrix with M = {
    val res = alg.mutable.QMatrix.zeros(a.rows * b.rows, a.cols * b.cols)
    for (r <- 0 until a.rows; c <- 0 until a.cols; av = a(r, c))
      res((r * b.rows) until ((r+1) * b.rows), (c * b.cols) until ((c+1) * b.cols)) = b * av
    a.factory.unsafeBuild(res)
  }

  def kron[M <: alg.QMatrixLike[M, _]](matrices: alg.QMatrix with M*): alg.QMatrix with M = matrices.tail.foldLeft(matrices.head)(kronAB[M])

  def reverseKron[M <: alg.QMatrixLike[M, _]](matrices: alg.QMatrix with M*): alg.QMatrix with M = kron[M](matrices.reverse:_*)

  def kronAB[V <: alg.QVectorLike[V, _]](a: alg.QVector with V, b: alg.QVector with V): alg.QVector with V = {
    val res = alg.mutable.QVector.zeros(a.length * b.length)
    for (i <- 0 until a.length; av = a(i))
      res(i * b.length until (i+1) * b.length) = b * av
    a.factory.unsafeBuild(res)
  }

  def kron[V <: alg.QVectorLike[V, _]](vectors: alg.QVector with V*): alg.QVector with V = vectors.tail.foldLeft(vectors.head)(kronAB[V])

  def reverseKron[V <: alg.QVectorLike[V, _]](vectors: alg.QVector with V*): alg.QVector with V = kron[V](vectors.reverse:_*)

  // ^^^ Matlab-like mathematical functions
  /////////////////////////////////////////
}
