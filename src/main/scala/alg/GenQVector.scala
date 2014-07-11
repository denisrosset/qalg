package com.faacets
package alg

import spire.algebra.Order
import spire.math.Rational
import net.alasc.Index

/** Base class for mutable or immutable vectors. */
abstract class GenQVector extends GenQTensor {
  /** Length of this vector. */
  def length: Int

  /** Returns the i-th element of this vector, zero-based. */
  def apply(i: Int): Rational

  override def toString = MatrixPrinting.print(immutable.QMatrix(1, length, unsafeToArray))
}

trait GenQVectorInstances {
  implicit object GenQVectorIndex extends Index[Rational, GenQVector] {
    def indexElement(v: GenQVector, i: Int) = v(i)
    def indexLength(v: GenQVector) = v.length
  }

  implicit object GenQVectorOrder extends Order[GenQVector] {
    def compare(u: GenQVector, v: GenQVector): Int = {
      assert(u.length == v.length)
      for(i <- 0 until u.length) {
        val compareValue = Rational.RationalAlgebra.compare(u(i), v(i))
        if (compareValue != 0)
          return compareValue
      }
      0
    }
  }
}
