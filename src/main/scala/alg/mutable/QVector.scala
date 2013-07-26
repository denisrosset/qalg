package com.faacets
package alg
package mutable

import spire.math.Rational

abstract class QVector extends alg.QVector with alg.QVectorLike[alg.mutable.QVector, alg.mutable.QMatrix] with Cloneable {
  ///////////////////////
  // vvv Abstract methods

  /** Updates the i-th element (zero-based) with the given value. */
  def update(i: Int, v: Rational): Unit

  /** Returns a clone of this vector. Performs a deep copy. */
  override def clone: alg.mutable.QVector = copy

  def copy: alg.mutable.QVector

  def factory: VectorFactory[alg.mutable.QVector] = alg.mutable.QVector

  def matrixFactory: MatrixFactory[alg.mutable.QMatrix] = alg.mutable.QMatrix

  // ^^^ Abstract methods
  ////////////////////////
  // vvv Update with a scalar

  def update(all: ::.type, v: Rational) { update(0 until length, v) }
  def update(is: Seq[Int], v: Rational) { for(i <- is) update(i, v) }

  // ^^^ Update with a scalar
  ///////////////////////////
  // vvv Update with a matrix

  def update(all: ::.type, m: alg.QMatrix) { update(0 until length, m) }
  def update(is: Seq[Int], m: alg.QMatrix) {
    require(is.length == m.length && (m.rows == 1 || m.cols == 1))
    for ((dest, src) <- is.view.zipWithIndex) update(dest, m(src))
  }

  // ^^^ Update with a matrix
  ///////////////////////////
  // vvv Update with a vector

  def update(all: ::.type, v: alg.QVector) { update(0 until length, v) }
  def update(is: Seq[Int], v: alg.QVector) {
    require(is.length == v.length)
    for ((dest, src) <- is.view.zipWithIndex) update(dest, v(src))
  }

  // ^^^ Update with a vector
  ////////////////////////////
  // vvv Collection-like methods

  def updateElements(f: Rational => Rational) { for (i <- 0 until length) this(i) = f(this(i)) }

  def updateWithIndex(f: (Int, Rational) => Rational) { for (i <- 0 until length) this(i) = f(i, this(i)) }

  // ^^^ Collection-like methods
  //////////////////////////
  // vvv Arithmetic update

  def +=(rhs: alg.QVector) { updateWithIndex( (i: Int, v: Rational) => v + rhs(i) ) }
  def -=(rhs: alg.QVector) { updateWithIndex( (i: Int, v: Rational) => v - rhs(i) ) }

  def *=[R : RationalMaker](rhs: R) {
    updateElements((rat: Rational) => rat * implicitly[RationalMaker[R]].toRational(rhs))
  }
  def /=[R : RationalMaker](rhs: R) {
    updateElements((rat: Rational) => rat / implicitly[RationalMaker[R]].toRational(rhs))
  }
  def :+=[R : RationalMaker](rhs: R) {
    updateElements((rat: Rational) => rat + implicitly[RationalMaker[R]].toRational(rhs))
  }
  def :-=[R : RationalMaker](rhs: R) {
    updateElements((rat: Rational) => rat - implicitly[RationalMaker[R]].toRational(rhs))
  }

  // ^^^ Arithmetic update
  ////////////////////////
}

class RationalVector(val data: Array[Rational]) extends alg.mutable.QVector {
  override def length = data.length
  def apply(k: Int) = data(k)
  def update(k: Int, v: Rational) { data(k) = v }
  def copy: RationalVector = new RationalVector(data.clone)
  def toArray = data.clone
  def usingIntegers = new RationalVector(alg.asSafeLongs(data).map(Rational(_)))
  def usingCoprimeIntegers = new RationalVector(alg.asCoprimeSafeLongs(data).map(Rational(_)))
  def elements = data.toIndexedSeq
  def toMutable = alg.mutable.QVector(this)
  def toImmutable = alg.immutable.QVector(this)
}

object QVector extends VectorFactory[alg.mutable.QVector] {
  def build(data: Array[Rational]) = new RationalVector(data.clone)
  def build(vec: alg.QVector) = new RationalVector(vec.elements.toArray)

  protected[alg] def unsafeBuild(data: Array[Rational]) = new RationalVector(data)
  protected[alg] def unsafeBuild(vec: alg.QVector) = new RationalVector(vec.elements.toArray)
}
