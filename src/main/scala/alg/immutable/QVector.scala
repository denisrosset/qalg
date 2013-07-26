package com.faacets
package alg
package immutable

import spire.math.Rational

abstract class QVector extends alg.QVector with alg.QVectorLike[alg.immutable.QVector, alg.immutable.QMatrix] with Cloneable {
  // vvv Abstract methods

  /** Returns a clone of this vector. Immutable, so no need to copy. */
  override def clone: alg.immutable.QVector = copy

  def copy: alg.immutable.QVector

  def factory: VectorFactory[alg.immutable.QVector] = alg.immutable.QVector

  def matrixFactory: MatrixFactory[alg.immutable.QMatrix] = alg.immutable.QMatrix

  // ^^^ Abstract methods
  ////////////////////////
}

class RationalVector(val data: Array[Rational]) extends alg.immutable.QVector {
  override def length = data.length
  def apply(k: Int) = data(k)
  def copy: RationalVector = this
  def toArray = data.clone
  def usingIntegers = new RationalVector(alg.asSafeLongs(data).map(Rational(_)))
  def usingCoprimeIntegers = new RationalVector(alg.asCoprimeSafeLongs(data).map(Rational(_)))
  def elements = data.toIndexedSeq
  override def hashCode: Int = scala.util.hashing.MurmurHash3.arrayHash(data)
  def toMutable = alg.mutable.QVector(this)
  def toImmutable = this
}

object QVector extends VectorFactory[alg.immutable.QVector] {
  def build(data: Array[Rational]) = new RationalVector(data.clone)
  def build(vec: alg.QVector) = new RationalVector(vec.elements.toArray)

  protected[alg] def unsafeBuild(data: Array[Rational]) = new RationalVector(data)
  protected[alg] def unsafeBuild(vec: alg.QVector) = new RationalVector(vec.elements.toArray)
}
