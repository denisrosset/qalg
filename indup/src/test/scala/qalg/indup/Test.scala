package com.faacets.qalg
package indup

import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.Checkers

import algebra._
import syntax.indup.all._

object Instances {
  implicit object AA extends ArrayArrayIntAlgebra
  implicit object A extends ArrayIntAlgebra
}

import Instances._

abstract class Test[M, V](m9: => M)(implicit UM: Update2[M, Int], M: IndexAt2To1[M, M, V, Int], V: IndexAt1[V, V, Int]) extends FunSuite {
  test("Update") {
    val a = m9
    a(0,0) = 0
    assert(a(0,0) === 0)
  }
  test("Row slice") {
    assert(m9(0, ::) === Array(1,2,3))
    assert(m9(1, At(0,2)) === Array(4,6))
    assert(m9(2, At(0)) === Array(7))
  }
  test("Col slice") {
    assert(m9(::, 0) === Array(1,4,7))
    assert(m9(At(0,2), 1) === Array(2,8))
    assert(m9(At(1), 2) === Array(6))
  }
  test("Row+col slice") {
    assert(m9(At(0,2), At(0,2)) === Array(Array(1,3),Array(7,9)))
    assert(m9(::, At(0,2)) === Array(Array(1,3),Array(4,6),Array(7,9)))
    assert(m9(At(0,2), ::) === Array(Array(1,2,3),Array(7,8,9)))
  }
  test("toIndexedSeq") {
    assert(m9(0,::).toIndexedSeq === Seq(1,2,3))
  }
  test("Size") {
    assert(m9.size0 === 3)
    assert(m9.size1 === 3)
    assert(m9.size === IntInt(3, 3))
  }
}

class Test1 extends Test[Array[Array[Int]], Array[Int]](Array(Array(1,2,3),Array(4,5,6),Array(7,8,9)))
