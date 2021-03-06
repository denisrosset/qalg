package com.faacets.qalg

import org.scalatest.{FunSuite, NonImplicitAssertions}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
import spire.std.long._

import algebra._
import algos._
import math._
import syntax.algos.all._
import Matrix.packs._
import optional.vecProductOrder._

class LUSuite[M, V](implicit val pack: PackField.ForMV[M, V, Rational]) extends FunSuite with NonImplicitAssertions {
  implicit def M = pack.M
  implicit def V = pack.V
  test("Linear solver") {
    val mA = M.rowMajor(3, 3)(
       3,  2, -1,
       2, -2,  4,
      -1, Rational(1, 2), -1)
    val vb = V.build(1, -2, 0)
    val res = V.build(1, -2, -2)
    assert(mA.lu.solveV(vb) === res)
  }
  test("Linear solver 1") {
    val mA = M.rowMajor(3, 3)(
      0, 0,-1,
      1, 0, 0,
      0,-1, 0)
    val vb = V.build(0, 1, 0)
    val res = V.build(1, 0, 0)
    assert(mA.lu.solveV(vb) === res)
  }
  test("Determinant") {
    val mA = M.rowMajor(3,3)(-2,2,-3, -1,1,3, 2,0,-1)
    assert(mA.lu.determinant === 18)
  }
}

final class DenseLUSuite extends LUSuite[Matrix[Rational, Imm], Vector[Rational, Imm]]
