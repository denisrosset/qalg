package com.faacets.alg

import mutable.{QVector => QVec, QMatrix => QMat}
import org.scalatest.FunSuite

/** Examples and tests taken from Lipschutz-Lipson 2004.
  * 
  * Full reference:
  * 
  * Lipschutz-Lipson, Schaum's Outline of Theory and Problems of Linear Algebra, 3rd Edition, McGraw-Hill, 2004
  */
class SchaumSuite extends FunSuite {
  test("Example 1.2") {
    val u = QVec(2, 4, -5)
    val v = QVec(1, -6, 9)
    assert(u + v === QVec(3, -2, 4))
    assert(7*u === QVec(14,28,-35))
    assert(-v === QVec(-1, 6, -9))
    assert(3*u - 5*v === QVec(1, 42, -60))
  }
  test("Example 1.3a") {
    val u = QVec(1,-2,3)
    val v = QVec(4,5,-1)
    val w = QVec(2,7,4)
    assert(u.dot(v) === -9)
    assert(u.dot(w) === 0)
    assert(v.dot(w) === 39)
  }
  test("Example 1.3b") {
    val u = QVec(2,3,-4)
    val v = QVec(3,-1,-2)
    assert(u.dot(v) === 11)
  }
  test("Example 2.2") {
    val a = QMat.colMajor((2, 3),
      1, -2, 3,
      0, 4, 5)
    val b = QMat.colMajor((2, 3),
      4, 6, 8,
      1, -3, -7)
    assert(a + b === QMat.colMajor((2, 3),
      5, 4, 11,
      1, 1, -2))
    assert(3*a === QMat.colMajor((2, 3),
      3, -6, 9,
      0, 12, 15))
    assert(2*a - 3*b === QMat.colMajor((2, 3),
      -10, -22, -18,
      -3, 17, 31))
  }
  test("Example 2.4") {
    assert(QMat((1,3), 7, -4, 5)*QMat((3,1),3,2,-1) === QMat((1,1),8))
    assert(QMat((1,4), 6,-1,8,3)*QMat((4,1), 4,-9,-2,5) === QMat((1,1),32))
  }
  test("Example 2.5") {
    assert(QMat.rowMajor((2, 2), 1, 3, 2, -1) * QMat.rowMajor((2, 3), 2, 0, -4, 5, -2, 6) === QMat.rowMajor((2, 3), 17, -6, 14, -1, 2, -14))
  }
  test("Exercice 2.8a") {
    assert(QMat.rowMajor((2, 2), 1, 6, -3, 5) * QVec(2, -7) === QVec(-40, -41))
  }
}
