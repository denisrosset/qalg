package com.faacets.qalg

import mutable.{QVector => QVec, QMatrix => QMat}
import org.scalatest.FunSuite
import spire.math.Rational

/** LU examples taken from
  * http://www.johnloomis.org/ece538/notes/Matrix/ludcmp.html
  */
class LinearAlgebraSuite extends FunSuite {
  test("Example 1") {
    val a = QMat.rowMajor( (3,3),
       4, -2, 1,
      -3, -1, 4,
       1, -1, 3)
    val b = QVec(15, 8, 13)
    assert(a.solve(b) === QVec(2, -2, 3))
  }
  test("Example 2") {
    val a = QMat.rowMajor( (4,4),
      6, 1,-6,-5,
      2, 2, 3, 2,
      4,-3, 0, 1,
      0, 2, 0, 1)
    val b = QVec(6, -2, -7, 0)
    assert(a.solve(b) === QVec(Rational(-1,2), Rational(1), Rational(1,3), Rational(-2)))
  }
  test("Set of equations for problem 2-5 (page 201)") {
    val a = QMat.rowMajor( (4,4),
      2,1,1,-2,
      4,0,2,1,
      3,2,2,0,
      1,3,2,0)
    val b = QVec(0,8,7,3)
    assert(a.solve(b) === QVec(Rational(35,11),Rational(26,11),Rational(-40,11),Rational(28,11)))
  }
  test("Set of equations for problem 2-11") {
    val a = QMat.rowMajor( (4,4),
      3,2,-1,-4,
      1,-1,3,-1,
      2,1,-3,0,
      0,-1,8,-5)
    val b = QVec(10,-4,16,3)
    intercept[IllegalArgumentException] {
      a.solve(b)
    }
  }
  test("Rank of matrix (Wikipedia)") {
    val a1 = QMat.rowMajor( (3,3),
      1,2,1,
      -2,-3,1,
      3,5,0)
    assert( a1.rank == 2 )
    val a2 = QMat.rowMajor( (2,4),
      1,1,0,2,
      -1,-1,0,-2
    ).t
    assert( a2.rank == 1)
  }
  // following examples taken from http://code.google.com/p/addi/source/browse/Addi/assets/m/linear-algebra/rref.m?r=190
  test("Reduced row echelon form") {
    val a = QMat.rowMajor( (2,2),
      1,3,
      4,5)
    val (r,k) = a.rref
    assert(r === QMat.eye(2))
    assert(k.sorted === List(0,1))
    val a1 = QMat.rowMajor( (3,2),
      1,3,
      4,5,
      7,9)
    val (r1,k1) = a1.rref
    assert(r1 === QMat.rowMajor( (3,2),
      1,0,
      0,1,
      0,0))
    assert(k1.sorted === List(0,1))
    val a2 = QMat.rowMajor( (3,3),
      1,2,3,
      2,4,6,
      7,2,0)
    val (r2,k2) = a2.rref
    assert(r2 === QMat.rowMajor( (3,3),
      Rational(1), Rational(0), Rational(-1, 2),
      Rational(0), Rational(1), Rational(7,4),
      Rational(0), Rational(0), Rational(0)
    ))
    assert(k2.sorted == List(0,1))
  }
  test("Kron of [1, 2] and [3, 5]") {
    import all._
    import spire.implicits._
    val a = QVec(1, 2)
    val b = QVec(3, 5)
    assert((a |+| b) == QVec(3, 5, 6, 10))
    assert(vecKron(a, b) == QVec(3, 5, 6, 10))
    assert(vecReverseKron(b, a) == QVec(3, 5, 6, 10))
  }
  test("Kron of [1, 2; 3 5] and [1, 0; 0, 1]") {
    import all._
    import spire.implicits._
    val a = QMat.rowMajor((2, 2),
      1, 2,
      3, 5)
    val b = QMat.rowMajor((2, 2),
      1, 0,
      0, 1);
    val k = QMat.rowMajor((4, 4),
      1, 0, 2, 0,
      0, 1, 0, 2,
      3, 0, 5, 0,
      0, 3, 0, 5)
    assert((a |+| b) == k)
    assert(kron(a, b) == k)
    assert(reverseKron(b, a) == k)
  }
}
