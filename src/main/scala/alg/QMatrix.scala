package com.faacets
package alg

import spire.math.{Rational, lcm, gcd}
import spire.syntax.cfor._

/** Base class for mutable or immutable matrices. */
abstract class QMatrix {
  ///////////////////////
  // vvv Abstract methods

  /** Number of rows. */
  def rows: Int

  /** Number of columns. */
  def cols: Int

  /** Enumeration of this matrix elements in column-major order. */
  def elements: IndexedSeq[Rational]

  /** Retries the i-th element of this matrix, in zero-based column-major order. */
  def apply(i: Int): Rational

  // ^^^ Abstract methods
  ////////////////////////////
  // vvv Standard Scala/Java methods

  override def toString: String = {
    if (rows == 0 || cols == 0)
      return f"$rows%d x $cols%d matrix"

    def colWidth(col: Int) = (0 until rows).map(row => this(row,col).toString.length+1).max
    val colWidths = (0 until cols).map(colWidth)
    val colText = "... " + cols + " total ..."

    def paddedCell(row: Int, col: Int) = {
      val cell = this(row, col).toString
      " " * (colWidths(col) - cell.length) + cell
    }

    val printRow: (Int => String) = if (colWidths.sum > QMatrix.maxToStringWidth && cols > 2) {
      val widthForNandNCols = (colWidths zip colWidths.reverse).take(cols/2).map( Function.tupled( (i,j) => i+j ) ).scanLeft(0)(_+_)
      val maxWidthLeft = QMatrix.maxToStringWidth - colText.length
      val howManyCols = widthForNandNCols.zipWithIndex.find(_._1 > maxWidthLeft).getOrElse((0,2))._2 - 1
      (row: Int) => (List() ++
        (0 until howManyCols).map(col => paddedCell(row, col)) ++
        List(colText) ++
        (cols - howManyCols until cols).map(col => paddedCell(row, col))
      ).mkString
    } else
      (row: Int) => (0 until cols).map(col => paddedCell(row, col)).mkString

    if (rows/2 > QMatrix.maxToStringRows/2 - 1) {
      val howManyRows = List(QMatrix.maxToStringRows/2 - 1, rows/2).min

      (List() ++
        (0 until howManyRows).map(printRow) ++
        List(f"... $rows%d in total ...") ++
        (rows - howManyRows until rows).map(printRow)
      ).mkString("\n")
    } else
      (0 until rows).map(printRow).mkString("\n")
  }

  override def hashCode: Int = sys.error("Object.hashCode not implemented for mutable matrices.")

  override def equals(other: Any) =
    other match {

      case that: alg.QMatrix =>
        (that canEqual this) &&
        rows == that.rows &&
        cols == that.cols &&
        elements.sameElements(that.elements)

      case _ => false

    }

  def canEqual(other: Any): Boolean =
    other.isInstanceOf[alg.QMatrix]

  // ^^^ Standard Java functions
  /////////////////////////////
  // vvv Mathematical methods

  def commonFactor = {
    val lcmDenominator = elements.map(_.denominator).fold(BigInt(1))(lcm)
    val gcdNumerator = elements.map(_.numerator).fold(BigInt(0))(gcd)
    Rational(gcdNumerator, lcmDenominator)
  }

  // ^^^ Mathematical methods
  /////////////////////
  // vvv Conversion

  def toMutable: mutable.QMatrix

  def toImmutable: immutable.QMatrix

  // ^^^ Conversion
  ////////////////////
  // vvv Element indexing

  def length = rows * cols

  def index(r: Int, c: Int): Int = r + c * rows

  def sameDimensions(rhs: alg.QMatrix) = rows == rhs.rows && cols == rhs.cols

  // ^^^ Element indexing
  //////////////////////
  // vvv Apply variants

  def apply(r: Int, c: Int): Rational = apply(index(r, c))

  // ^^^ Apply variants
  /////////////////////
}

/** Base trait for mutable or immutable matrices. */
trait QMatrixLike[M <: alg.QMatrixLike[M, V], V <: alg.QVectorLike[V, M]] extends alg.QMatrix {
  lhs: M =>
  ///////////////////////
  // vvv Abstract methods

  /** Returns a copy of this matrix. */
  def copy: M

  /** Returns a QVector containing a copy of this matrix elements.
    * 
    * @note The matrix should be one-dimensional. */
  def toQVector: V

  /** Returns a factory for this kind of QMatrix. */
  def factory: MatrixFactory[M]

  /** Returns a factory for vectors of the same kind a this QMatrix. */
  def vectorFactory: VectorFactory[V]

  // ^^^ Abstract methods
  ////////////////////////////
  // vvv Element indexing

  def t: M = factory.tabulate(cols, rows)( (c,r) => this(r,c) )

  // ^^^ Element indexing
  //////////////////////
  // vvv Apply variants

  def apply(r: ::.type, c: ::.type): M = copy

  def apply(r: Int, all: ::.type): M = apply(r, 0 until cols)

  def apply(all: ::.type, c: Int): M = apply(0 until rows, c)

  def apply(rs: Seq[Int], all: ::.type): M = apply(rs, 0 until cols)

  def apply(all: ::.type, cs: Seq[Int]): M = apply(0 until rows, cs)

  def apply(rs: Seq[Int], c: Int): M = factory.tabulate(rs.length, 1)( (i, j) => this(rs(i), c))

  def apply(r: Int, cs: Seq[Int]): M = factory.tabulate(1, cs.length)( (i, j) => this(r, cs(j)))

  def apply(rs: Seq[Int], cs: Seq[Int]): M = factory.tabulate(rs.length, cs.length)( (i, j) => this(rs(i), cs(j)))

  // ^^^ Apply variants
  /////////////////////////
  // vvv Collection-like methods

  def mapElements(f: Rational => Rational): M = {
    val data = new Array[Rational](length)
    cfor(0)(_ < length, _ + 1) { index =>
      data(index) = f(this(index))
    }
    factory.unsafeBuild(rows, cols, data)
  }

  def mapWithIndices(f: (Int, Int, Rational) => Rational): M = {
    val data = new Array[Rational](length)
    var index = 0
    cfor(0)(_ < cols, _ + 1) { c =>
      cfor(0)(_ < rows, _ + 1) { r =>
        data(index) = f(r, c, this(index))
        index += 1
      }
    }
    factory.unsafeBuild(rows, cols, data)
  }

  // ^^^ Collection-like methods
  //////////////////////////
  // vvv Scalar arithmetic

  def :+[R : RationalMaker](rhs: R): M =
    mapElements((rat: Rational) => rat + implicitly[RationalMaker[R]].toRational(rhs))

  def :-[R : RationalMaker](rhs: R): M =
    mapElements((rat: Rational) => rat - implicitly[RationalMaker[R]].toRational(rhs))

  def *[R : RationalMaker](rhs: R): M =
    mapElements((rat: Rational) => rat * implicitly[RationalMaker[R]].toRational(rhs))

  def /[R : RationalMaker](rhs: R): M = 
    mapElements((rat: Rational) => rat / implicitly[RationalMaker[R]].toRational(rhs))

  // ^^^ Scalar arithmetic
  ////////////////////////
  // vvv Vector arithmetic

  def *(rhs: alg.QVector): V = {
    val res = alg.mutable.QVector.zeros(lhs.rows)
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

  // ^^^ Vector arithmetic
  ////////////////////////
  // vvv Matrix arithmetic

  def unary_-(): M = mapElements(-_)

  def +(rhs: alg.QMatrix): M = {
    require(sameDimensions(rhs))
    factory.tabulate(rows, cols)( (r,c) => lhs(r,c) + rhs(r,c) )
  }

  def -(rhs: alg.QMatrix): M = {
    require(sameDimensions(rhs))
    factory.tabulate(rows, cols)( (r,c) => lhs(r,c) - rhs(r,c) )
  }

  def *(rhs: alg.QMatrix): M = {
    val res = alg.mutable.QMatrix.zeros(lhs.rows, rhs.cols)
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

  // ^^^ Matrix arithmetic
  ////////////////////////////
  // vvv Helper mathematical methods

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
    val x = alg.mutable.QVector.tabulate(n)( i => b(order(i)) )
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
          a(r, c until cols) /= a(r, c)
          // eliminate current column
          for (ridx <- 0 until rows) {
            if (ridx != r)
              a(ridx, c until cols) = a(ridx, c until cols) - a(r, c until cols) * a(ridx, c)
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

  def usingIntegers: M = factory.build(rows, cols, asSafeLongs(elements.toArray).map(Rational(_)))

  def usingCoprimeIntegers: M = factory.build(rows, cols, asCoprimeSafeLongs(elements.toArray).map(Rational(_)))

  def rowsUsingCoprimeIntegers: M = {
    val res = alg.mutable.QMatrix(this)
    for (r <- 0 until rows) {
      res(r, ::) = this(r, ::).toQVector.usingCoprimeIntegers
    }
    factory.unsafeBuild(res)
  }

  /** Performs the Gram-Schmidt process to orthogonalize the rows of matrix
    * M. */
  def integerOrthogonalized: M = {
    val res = alg.mutable.QMatrix(this)
    for (i <- 0 until rows-1) {
      val v = res(i, ::).toQVector
      for (j <- i+1 until rows) {
        val r = res(j, ::).toQVector
        res(j, ::) = ( (v.dot(v))*r - (v.dot(r))*v ).usingCoprimeIntegers
      }
    }
    factory.unsafeBuild(res)
  }

  def compact = factory.build(rows, cols, elements.map(alg.QVector.cacheRational(_)).toArray)

  // ^^^ Helper mathematical methods
  //////////////////////////////////
}

abstract class MatrixFactory[M <: alg.QMatrix] {
  ///////////////////////
  // vvv Abstract methods

  /** Constructs a matrix from rows/cols and a column-major data array. */
  def build(rows: Int, cols: Int, data: Array[Rational]): M

  /** Constructs a copy of a matrix. */
  def build(mat: alg.QMatrix): M

  protected[alg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]): M

  protected[alg] def unsafeBuild(mat: alg.QMatrix): M

  // ^^^ Abstract methods
  /////////////////////////
  // vvv Constructor variants 

  def colMajor[R : RationalMaker](size: (Int, Int), data: R*): M = build(size._1, size._2, data.map(r => implicitly[RationalMaker[R]].toRational(r)).toArray)

  def rowMajor[R : RationalMaker](size: (Int, Int), transposedData: R*): M = tabulate(size._1, size._2)( (r, c) => implicitly[RationalMaker[R]].toRational(transposedData(c + r * size._2)) )

  def apply[R : RationalMaker](size: (Int, Int), data: R*): M = colMajor(size, data:_*)

  def apply(rows: Int, cols: Int, data: Array[Int]): M = build(rows, cols, data.map(alg.QVector.cacheRational(_)))

  def apply(rows: Int, cols: Int, data: Array[Rational]): M = build(rows, cols, data)

  def apply(rows: Int, cols: Int, vector: alg.QVector): M =
    tabulate(rows, cols)( (r, c) => vector(r + c * rows) )

  def apply(mat: alg.QMatrix): M = build(mat)

  // ^^^ Constructor variants
  ////////////////////////////
  // vvv Collection-like methods

  def fill(rows: Int, cols: Int)(v: Rational): M = 
    build(rows, cols, Array.fill(rows*cols)(v))

  def tabulate(rows: Int, cols: Int)(f: (Int, Int) => Rational): M = {
    val data = new Array[Rational](rows * cols)
    var index = 0
    cfor(0)(_ < cols, _ + 1) { c =>
      cfor(0)(_ < rows, _ + 1) { r =>
        data(index) = f(r, c)
        index += 1
      }
    }
    build(rows, cols, data)
  }

  // ^^^ Collection-like methods
  /////////////////////////////////
  // vvv Matlab-like matrix constructors

  def eye(d: Int): M = tabulate(d, d)( (r, c) => if (r == c) Rational.one else Rational.zero )
  def zeros(rows: Int, cols: Int): M = fill(rows, cols)(Rational.zero)
  def ones(rows: Int, cols: Int): M = fill(rows, cols)(Rational.one)

  // ^^^ Matlab-like matrix constructors
  ///////////////////////////////////////
}

object QMatrix extends MatrixFactory[alg.QMatrix] {
  def build(mat: alg.QMatrix) = alg.mutable.QMatrix(mat)
  def build(rows: Int, cols: Int, data: Array[Rational]) = alg.mutable.QMatrix.build(rows, cols, data)
  protected[alg] def unsafeBuild(mat: alg.QMatrix) = mat
  protected[alg] def unsafeBuild(rows: Int, cols: Int, data: Array[Rational]) = alg.mutable.QMatrix.unsafeBuild(rows, cols, data)

  var maxToStringWidth = 80
  var maxToStringRows = 15
}
