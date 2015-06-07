package com.faacets.qalg

import org.scalatest.{FunSuite, NonImplicitAssertions}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
import spire.std.long._

import algebra._
import algos._
import math._
import syntax.all._

class LUSuite[M, V](val pack: AlgMVField[M, V, Rational]) extends FunSuite with NonImplicitAssertions {
  import pack._
  test("Linear solver") {
    val mA = M.build(3, 3,
       3,  2, -1,
       2, -2,  4,
      -1, Rational(1, 2), -1)
    val vb = V.build(1, -2, 0)
    val res = V.build(1, -2, -2)
    assert(mA.lu.solveV(vb) === res)
  }
  test("Determinant") {
    val mA = M.build(3,3, -2,2,-3, -1,1,3, 2,0,-1)
    assert(mA.lu.determinant === 18)
  }
}

final class DenseLUSuite extends LUSuite[DenseM[Rational, Immutable], DenseV[Rational, Immutable]](DenseM.rationalPack)
