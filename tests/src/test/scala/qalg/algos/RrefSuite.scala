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
import syntax.algos.all._
import Matrix.packs._
import optional.matProductOrder._
import optional.vecProductOrder._

class RrefSuite[M0, V](implicit val pack: PackField.ForMV[M0, V, Rational]) extends FunSuite with NonImplicitAssertions {
  import pack._
  test("1x1 identity matrix") {
    val mA = M.rowMajor(1,1)(1)
    assert(mA.rref.reduced === mA)
  }
  test("Full rank matrix") {
    val mA = M.rowMajor(2,2)(1,3, 4,5)
    val res = eye[M](2)
    val r = mA.rref
    assert(r.reduced === res)
    assert(r.basis.toSet == Set(0,1))
  }
  test("Non square matrix") {
    val mA = M.rowMajor(3,2)(1,3, 4,5, 7,9)
    val res = eye[M](3,2)
    val r = mA.rref
    assert(r.reduced === res)
    assert(r.basis.toSet == Set(0,1))
  }
  test("Non full rank matrix") {
    val mA = M.rowMajor(3,3)(1,2,3, 2,4,6, 7,2,0)
    val res = M.rowMajor(3,3)(1,0,Rational(-1,2), 0,1,Rational(7,4), 0,0,0)
    val r = mA.rref
    assert(r.reduced === res)
    assert(r.basis.toSet == Set(0,1))
  }
}

final class DenseRrefSuite extends RrefSuite[Matrix[Rational, Imm], Vector[Rational, Imm]]
