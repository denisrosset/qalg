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