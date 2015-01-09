package com.faacets

import scala.annotation.tailrec
import scala.math.abs
import spire.algebra.{Monoid, VectorSpace}
import spire.math.{Rational, SafeLong, lcm}
import language.implicitConversions
import spire.implicits._

package object qalg {

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

  def kron[M <: QMatrixBase[M, _]](matrices: M*)(implicit ev: Monoid[M]): M =
    matrices.tail.foldLeft(matrices.head)(_ |+| _)

  def reverseKron[M <: QMatrixBase[M, _]](matrices: M*)(implicit ev: Monoid[M]): M = 
    kron[M](matrices.reverse:_*)

  def vecKron[V <: QVectorBase[V, _]](vectors: V*)(implicit ev: Monoid[V]): V = 
    vectors.tail.foldLeft(vectors.head)(_ |+| _)

  def vecReverseKron[V <: QVectorBase[V, _]](vectors: V*)(implicit ev: Monoid[V]): V = 
    vecKron[V](vectors.reverse:_*)
}
