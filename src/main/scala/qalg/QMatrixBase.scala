package com.faacets
package qalg

import spire.algebra.{Field, Monoid, Ring, VectorSpace}
import spire.math.Rational
import spire.syntax.cfor._
import spire.syntax.innerProductSpace._
import all._

/** Base trait for mutable or immutable matrices. */
abstract class QMatrixBase[M <: QMatrixBase[M, V], V <: QVectorBase[V, M]] extends GenQMatrix with QTensorBase[M] {
  lhs: M =>
  def factory: QMatrixFactory[M]
  def vectorFactory: QVectorFactory[V]

  /** Returns a QVector containing a copy of this matrix elements.
    * 
    * @note The matrix should be one-dimensional. */
  def toQVector: V = {
    require(rows == 1 || cols == 1)
    vectorFactory.tabulate(length)(i => apply(i))
  }

  def rowVectors: IndexedSeq[V] = new IndexedSeq[V] {
    def length = rows
    def apply(r: Int): V = vectorFactory.tabulate(cols)(c => lhs.apply(r, c))
  }

  def colVectors: IndexedSeq[V] = new IndexedSeq[V] {
    def length = cols
    def apply(c: Int): V = vectorFactory.tabulate(rows)(r => lhs.apply(r, c))
  }

  def toMutable: mutable.QMatrix
  def toImmutable: immutable.QMatrix

  def mapElements(f: Rational => Rational): M = 
    factory.tabulate(rows, cols)( (r, c) => f(this(r, c)))

  def mapWithIndices(f: (Int, Int, Rational) => Rational): M = 
    factory.tabulate(rows, cols)( (r, c) => f(r, c, this(r, c)))

  def t: M = factory.tabulate(cols, rows)( (c,r) => this(r,c) )

  def apply(r: ::.type, c: ::.type): M = copy

  def apply(r: Int, all: ::.type): M = apply(r, 0 until cols)

  def apply(all: ::.type, c: Int): M = apply(0 until rows, c)

  def apply(rs: Seq[Int], all: ::.type): M = apply(rs, 0 until cols)

  def apply(all: ::.type, cs: Seq[Int]): M = apply(0 until rows, cs)

  def apply(rs: Seq[Int], c: Int): M = factory.tabulate(rs.length, 1)( (i, j) => this(rs(i), c))

  def apply(r: Int, cs: Seq[Int]): M = factory.tabulate(1, cs.length)( (i, j) => this(r, cs(j)))

  def apply(rs: Seq[Int], cs: Seq[Int]): M = factory.tabulate(rs.length, cs.length)( (i, j) => this(rs(i), cs(j)))

  def *(rhs: V): V = {
    val res = mutable.QVector.zeros(lhs.rows)
    require(lhs.cols == rhs.length)
    cfor(0)(_ < lhs.rows, _ + 1) { r =>
      cfor(0)(_ < lhs.cols, _ + 1) { c =>
        val v = lhs(r, c)
        val w = rhs(c)
        if (v != Rational.zero && w != Rational.zero) {
          res(r) += v * w
        }
      }
    }
    vectorFactory.unsafeBuild(res)
  }

  def circShift(rowShift: Int, colShift: Int) = {
    val res = toMutable
    rowShift.signum match {
      case 1 => { // shift downwards
        val s = rowShift % rows
        if (s != 0) {
          res(0 until s, ::) = this(rows - s until rows, ::)
          res(s until rows, ::) = this(0 until rows - s, ::)
        }
      }
      case -1 => { // shift upwards
        val s = (-rowShift) % rows
        if (s != 0) {
          res(rows - s until rows, ::) = this(0 until s, ::)
          res(0 until rows - s, ::) = this(s until rows, ::)
        }
      }
      case _ => { }
    }
    colShift.signum match {
      case 1 => { // shift to the right
        val s = colShift % cols
        if (s != 0) {
          res(::, 0 until s) = this(::, rows - s until rows)
          res(::, s until rows) = this(::, 0 until rows - s)
        }
      }
      case -1 => { // shift to the left
        val s = (-colShift) % cols
        if (s != 0) {
          res(::, rows - s until rows) = this(::, 0 until s)
          res(::, 0 until rows - s) = this(::, s until rows)
        }
      }
      case _ => { }
    }
    factory.unsafeBuild(res)
  }

  def isIntegerMatrix: Boolean = {
    cfor(0)(_ < rows, _ + 1) { r =>
      cfor(0)(_ < cols, _ + 1) { c =>
        if (this(r, c).denominator != 1)
          return false
      }
    }
    true
  }

  def isIdentity: Boolean = {
    if (rows != cols) return false
    cfor(0)(_ < rows, _ + 1) { r =>
      cfor(0)(_ < cols, _ + 1) { c =>
        if ((r == c && this(r,c) != Rational.one) || (r != c && this(r,c) != Rational.zero))
          return false
      }
    }
    return true
  }

  def solve(b: V): V = {
    val (lu, order, flag) = luDecomposition
    var n = rows
    // rearrange the elements of the b vector, hold them into x
    val x = mutable.QVector.tabulate(n)( i => b(order(i)) )
    if( (0 until n).exists( i => lu(i,i) == 0 ) )
      throw new IllegalArgumentException("Coefficient matrix is singular")
    // do forward substitution, replacing x vector
    x(0) /= lu(0,0)
    for (i <- 1 until n) {
      var sum = Rational.zero
      for (j <- 0 until i) sum += lu(i,j) * x(j)
      x(i) = (x(i) - sum)/lu(i,i)
    }
    // now get the solution vector, x(n-1) is already done
    for (i <- n - 2 to 0 by -1) {
      var sum = Rational.zero
      for (j <- i + 1 until n) sum += lu(i,j) * x(j)
      x(i) -= sum
    }
    vectorFactory.unsafeBuild(x)
  }

  def rank = rref._2.length

  /** Return reduced row-echelon form of matrix.
    */
  def rref: (M, List[Int]) = {
    val a = toMutable
    var used = List.empty[Int]
    var r = 0
    for (c <- 0 until cols) {
      if (r < rows) {
        val (m, pivot) = (r until rows).map( i => (a(i,c).abs, i) ).max
        if (m != 0) { // if m is zero, skip the column c
          used = c :: used // keep track of bound variables

          // swap current row and pivot row
          val tmp = a(pivot, c until cols)
          a(pivot, c until cols) = a(r, c until cols)
          a(r, c until cols) = tmp
          // normalize pivot row
          val f = a(r, c)
          for (c1 <- c until cols)
            a(r, c1) = a(r, c1) / f
          // eliminate current column
          for (ridx <- 0 until rows) {
            if (ridx != r)
              a(ridx, c until cols) = a(ridx, c until cols) - (a(r, c until cols) :* a(ridx, c))
          }
          r += 1
        }
      }
    }
    (factory.unsafeBuild(a), used)
  }

  // assumes matrix non singular
  def luDecomposition: (M, Array[Int], Int) = {
    require(rows == cols)
    var flag = 1 // changes sign with each row interchange
    val n = rows
    val order: Array[Int] = Array(0 until n:_*) // establish initial ordering in order vector
    val a = toMutable
    /* Find pivot element
     * 
     * The function pivot finds the largest element for a pivot in "jcol"
     *   of Matrix "a", performs interchanges of the appropriate
     *   rows in "a", and also interchanges the corresponding elements in
     *   the order vector.
     *
     *  using    a      -  n by n Matrix of coefficients
     *  using    order  - integer vector to hold row ordering
     *  @param    jcol   - column of "a" being searched for pivot element
     *
     */
    def pivot(jcol: Int): Boolean = {
      var ipvt = jcol
      var big = a(ipvt, ipvt).abs
      // Find biggest element on or below diagonal. This will be the pivot row.
      for (i <- ipvt + 1 until n) {
        val anext = a(i, jcol).abs
        if (anext > big) {
          big = anext
          ipvt = i
        }
      }
      if (big == 0) throw new IllegalArgumentException("LU decomposition is implemented for now only for non-singular matrices.")
      assert(big != 0) // otherwise Matrix is singular
      // Interchange pivot row (ipvt) with current row (jcol).
      if (ipvt == jcol) false
      else {
        val row = a(jcol, ::)
        a(jcol, ::) = a(ipvt, ::)
        a(ipvt, ::) = row
        val tmp: Int = order(jcol)
        order(jcol) = order(ipvt)
        order(ipvt) = tmp
        true
      }
    }

    /* do pivoting for first column and check for singularity */
    if (pivot(0)) flag = -flag
    val diag0 = Rational.one/a(0,0)
    for (i <- 1 until n)
      a(0,i) *= diag0

    //  Now complete the computing of L and U elements.
    //  The general plan is to compute a column of L's, then
    //  call pivot to interchange rows, and then compute
    //  a row of U's.

    var nm1 = n - 1

    for (j <- 1 until nm1) {
      /* column of L's */
      for (i <- j until n) {
	var sum = Rational.zero
        for (k <- 0 until j)
          sum += a(i,k) * a(k,j)
        a(i,j) -= sum
      }
      /* pivot, and check for singularity */
      if (pivot(j)) flag = -flag
      /* row of U's */
      val diag = Rational.one/a(j,j)
      for (k <- j + 1 until n) {
        var sum = Rational.zero
        for (i <- 0 until j)
          sum += a(j,i) * a(i,k)
        a(j,k) = (a(j,k) - sum) * diag
      }
    }

    /* still need to get last element in L Matrix */

    var suml = Rational.zero
    for (k <- 0 until nm1)
      suml += a(nm1,k) * a(k,nm1)
    a(nm1,nm1) -= suml
    (factory.unsafeBuild(a), order, flag)
  }

  def isTimesIdentity: Boolean = {
    if (rows != cols) return false
    val ct = this(0,0)
    cfor(0)(_ < rows, _ + 1) { r =>
      cfor(0)(_ < cols, _ + 1) { c =>
        if ((r == c && this(r,c) != ct) || (r != c && this(r,c) != Rational.zero))
          return false
      }
    }
    return true
  }

  /** Performs the Gram-Schmidt process to orthogonalize the rows of matrix
    * M. */
  def integerOrthogonalized: M = {
    val res = mutable.QMatrix(this)
    for (i <- 0 until rows-1) {
      val v = res(i, ::).toQVector
      for (j <- i+1 until rows) {
        implicit val vs: spire.algebra.InnerProductSpace[mutable.QVector, Rational] =
          mutableQVectorInnerProductSpace
        val r = res(j, ::).toQVector
        res(j, ::) = ( ((v dot v) *: r) - ((v dot r) *: v) ).withPrimes._1
      }
    }
    factory.unsafeBuild(res)
  }
}

class QMatrixAlgebra[M <: QMatrixBase[M, _]](factory: QMatrixFactory[M]) extends Ring[M] with VectorSpace[M, Rational] {
  implicit def scalar = Rational.RationalAlgebra
  def negate(m: M): M = factory.tabulate(m.rows, m.cols)(-m(_, _))
  def zero: M = factory.fill(0, 0)(Rational.zero)
  def one: M = factory.fill(0, 0)(Rational.zero)
  def plus(x: M, y: M): M = {
    require(x.sameDimensions(y))
    factory.tabulate(x.rows, x.cols)( (r, c) => x(r, c) + y(r, c))
  }
  override def minus(x: M, y: M): M = {
    require(x.sameDimensions(y))
    factory.tabulate(x.rows, x.cols)( (r, c) => x(r, c) - y(r, c))
  }
  def timesl(s: Rational, m: M): M = factory.tabulate(m.rows, m.cols)( (r, c) => s * m(r, c) )
  def times(lhs: M, rhs: M): M = {
    val res = mutable.QMatrix.zeros(lhs.rows, rhs.cols)
    require(lhs.cols == rhs.rows)
    cfor(0)(_ < lhs.cols, _ + 1) { c =>
      cfor(0)(_ < lhs.rows, _ + 1) { r =>
        val v = lhs(r, c)
        if (v != Rational.zero) {
          cfor(0)(_ < rhs.cols, _ + 1) { j =>
            val w = rhs(c, j)
            if (w != Rational.zero)
              res(r, j) += v * w
          }
        }
      }
    }
    factory.unsafeBuild(res)
  }
}

class QMatrixBaseKron[M <: QMatrixBase[M, _]](factory: QMatrixFactory[M])(implicit ms: VectorSpace[M, Rational]) extends Monoid[M] {
  def id = factory.fill(1, 1)(Rational.one)

  def op(a: M, b: M): M = {
    val res = mutable.QMatrix.zeros(a.rows * b.rows, a.cols * b.cols)
    // TODO cfor
    for (r <- 0 until a.rows; c <- 0 until a.cols; av = a(r, c))
      res((r * b.rows) until ((r+1) * b.rows), (c * b.cols) until ((c+1) * b.cols)) = ms.timesl(av, b)
    a.factory.unsafeBuild(res)
  }
}
