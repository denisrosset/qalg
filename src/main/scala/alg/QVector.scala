package com.faacets
package alg

import spire.math.{Rational, SafeLong, lcm, gcd}
import spire.syntax.cfor._
import net.alasc.{Dom, FiniteElement, PermElementLike, Action}


/** Base class for mutable or immutable vectors. */
abstract class QVector {
  ///////////////////////
  // vvv Abstract methods

  /** Length of this vector. */
  def length: Int

  /** Enumeration of this vector elements. */
  def elements: IndexedSeq[Rational]

  /** Returns the i-th element of this vector, zero-based. */
  def apply(i: Int): Rational

  // ^^^ Abstract methods
  ////////////////////////////
  // vvv Standard Scala/Java methods

  override def toString: String = alg.QMatrix((1, length), elements:_*).toString

  override def hashCode: Int = sys.error("Object.hashCode not implemented for mutable vectors.")

  override def equals(other: Any) =
    other match {
      case that: alg.QVector =>
        (that canEqual this) &&
        length == that.length &&
        elements.sameElements(that.elements)

      case _ => false
    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[alg.QVector]

  // vvv Standard Scala/Java methods
  /////////////////////////////
  // vvv Mathematical methods

  def commonFactor = {
    val lcmDenominator = elements.map(_.denominator).fold(BigInt(1))(lcm)
    val gcdNumerator = elements.map(_.numerator).fold(BigInt(0))(gcd)
    Rational(gcdNumerator, lcmDenominator)
  }

  def integerCoefficients: (Array[BigInt], BigInt) = {
    val commonDenominator = elements.map(_.denominator).fold(BigInt(1))(lcm)
    val coeffsArray = elements.map( r => r.numerator * (commonDenominator/r.denominator) ).toArray
    (coeffsArray, commonDenominator)
  }

  def longCoefficients: (Array[Long], Long) = {
    val (coeffsArray, commonDenominator) = integerCoefficients
    if (coeffsArray.exists(!_.isValidLong))
      throw new IllegalArgumentException("Coefficients do not fit into a long.")
    if (!commonDenominator.isValidLong)
      throw new IllegalArgumentException("Denominator does not fit into a long.")
    (coeffsArray.map(_.longValue), commonDenominator.longValue)
  }

  // ^^^ Mathematical methods
  /////////////////////
  // vvv Conversion

  def toMutable: mutable.QVector

  def toImmutable: immutable.QVector

  // ^^^ Conversion
}

/** Base trait for mutable or immutable vectors. */
trait QVectorLike[V <: alg.QVectorLike[V, M], M <: alg.QMatrixLike[M, V]] extends alg.QVector {
  lhs: V =>
  ///////////////////////
  // vvv Abstract methods

  /** Returns a copy of this QVector. */
  def copy: V

  /** Returns a builder for this kind of QVector. */
  def factory: VectorFactory[V]

  /** Returns a builder for matrices of the same kind as this QVector. */
  def matrixFactory: MatrixFactory[M]

  // ^^^ Abstract methods
  //////////////////////
  // vvv Apply variants

  def apply(all: ::.type): V = copy
  def apply(is: Seq[Int]): V = factory.tabulate(is.length)( i => this(i) )

  // ^^^ Apply variants
  /////////////////////////
  // vvv Collection-like methods

  def mapElements(f: Rational => Rational): V = factory.tabulate(length)(i => f(this(i)))

  def mapWithIndex(f: (Int, Rational) => Rational): V = {
    val data = new Array[Rational](length)
    var index = 0
    cfor(0)(_ < length, _ + 1) { index =>
      data(index) = f(index, this(index))
    }
    factory.build(data)
  }

  // ^^^ Collection-like methods
  //////////////////////////
  // vvv Scalar arithmetic

  def :+[R : RationalMaker](rhs: R): V =
    mapElements((rat: Rational) => rat + implicitly[RationalMaker[R]].toRational(rhs))

  def :-[R : RationalMaker](rhs: R): V =
    mapElements((rat: Rational) => rat - implicitly[RationalMaker[R]].toRational(rhs))

  def *[R : RationalMaker](rhs: R): V =
    mapElements((rat: Rational) => rat * implicitly[RationalMaker[R]].toRational(rhs))

  def /[R : RationalMaker](rhs: R): V =
    mapElements((rat: Rational) => rat / implicitly[RationalMaker[R]].toRational(rhs))

  // ^^^ Scalar arithmetic
  ////////////////////////
  // vvv Vector arithmetic

  def canonicalizeCoefficients: (V, Rational) = {
    val cf = commonFactor
    if (cf == Rational.zero) // cf can be zero if ALL coefficients are zero
      (this, cf)
    else
      (this/cf, cf)
  }

  def uncanonicalizeCoefficients(cf: Rational): V = this*cf

  def unary_-(): V = mapElements( (rat: Rational) => -rat )

  def +(rhs: alg.QVector): V = factory.tabulate(length)(i => lhs(i) + rhs(i))

  def -(rhs: alg.QVector): V = factory.tabulate(length)(i => lhs(i) - rhs(i))

  def dot(rhs: alg.QVector): Rational = {
    require(length == rhs.length)
    var s = this(0)*rhs(0)
    cfor(1)(_ < length, _ + 1) { i =>
      s += this(i)*rhs(i)
    }
    s
  }

  // ^^^ Vector arithmetic
  //////////////////////////
  // vvv Action of permutations

  import Dom.ZeroBased._

  def permutedBy(p: PermElementLike): V = {
    val newData = new Array[Rational](length)
    for (i <- 0 until length)
      newData(p.image(i)._0) = this(i)
    factory.build(newData)
  }

  def permutedByInverseOf(pinv: PermElementLike): V = factory.tabulate(length)(i => apply(pinv.image(i)._0))

  def permutedBy[F <: FiniteElement[F]](f: F, action: Action[F]): V = {
    val newData = new Array[Rational](length)
    for (i <- 0 until length)
      newData(action(f, i)._0) = this(i)
    factory.build(newData)
  }

  def permutedByInverseOf[F <: FiniteElement[F]](finv: F, action: Action[F]): V = factory.tabulate(length)(i => apply(action(finv, i)._0))


  // ^^^ Action of permutations
  ///////////////////////
  // vvv Conversions

  /** Returns a row QMatrix containing a copy of this vector elements. */
  def toRowQMatrix: M = matrixFactory.tabulate(1, length)( (r,c) => this(c) )

  /** Returns a column QMatrix containing a copy of this vector elements. */
  def toColQMatrix: M = matrixFactory.tabulate(length, 1)( (r,c) => this(r) )

  /** Transpose of that vector, i.e. vector as row matrix. */
  def t: M = toRowQMatrix

  def usingIntegers: V
  def usingCoprimeIntegers: V
}

abstract class VectorFactory[V <: alg.QVector] {
  ///////////////////////
  // vvv Abstract methods

  /** Constructs a vector from a data array. */
  def build(data: Array[Rational]): V

  /** Constructs a copy of a vector. */
  def build(vec: alg.QVector): V

  protected[alg] def unsafeBuild(vec: alg.QVector): V

  protected[alg] def unsafeBuild(data: Array[Rational]): V

  // ^^^ Abstract methods
  /////////////////////////
  // vvv Constructor variants

// TODO: change immutable.QVector implementation to an array of BigInt/Long/Int and
// denominator to avoid this hack
  val smallIntegerCache = collection.mutable.Map.empty[Int, Rational]

  def apply(intArray: Array[Int]) = {
    val coeffArray = intArray.map {
      case i if i >= -300 && i <= 300 && smallIntegerCache.isDefinedAt(i) =>
        smallIntegerCache(i)
      case i if i >= -300 && i <= 300 =>
        val r = Rational(i)
        smallIntegerCache(i) = r
        r
      case i => Rational(i)
    }
    unsafeBuild(coeffArray)
  }
  def apply[R : RationalMaker](data: R*): V =
    build(data.map(r => implicitly[RationalMaker[R]].toRational(r)).toArray)

  def apply(data: Array[Rational]): V = build(data)

  def apply(mat: alg.QMatrix): V = tabulate(mat.rows*mat.cols)( i => mat(i) )

  def apply(vec: alg.QVector): V = build(vec)

  // ^^^ Constructor variants
  ////////////////////////////
  // vvv Collection-like methods

  def fill(length: Int)(v: Rational): V =
    build(Array.fill(length)(v))

  def tabulate(length: Int)(f: Int => Rational): V = {
    val data = new Array[Rational](length)
    cfor(0)(_ < length, _ + 1) { index =>
      data(index) = f(index)
    }
    build(data)
  }

  // ^^^ Collection-like methods
  /////////////////////////////////
  // vvv Matlab-like vector constructors

  def zeros(length: Int): V = fill(length)(Rational.zero)
  def ones(length: Int): V = fill(length)(Rational.one)

  // ^^^ Matlab-like vector constructors
  ///////////////////////////////////////

}

object QVector extends VectorFactory[alg.QVector] {
  def build(vec: alg.QVector) = alg.mutable.QVector.build(vec)
  def build(data: Array[Rational]) = alg.mutable.QVector.build(data)
  protected[alg] def unsafeBuild(vec: alg.QVector) = vec
  protected[alg] def unsafeBuild(data: Array[Rational]) = alg.mutable.QVector.unsafeBuild(data)
}
