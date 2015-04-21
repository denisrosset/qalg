package com.faacets.qalg

import org.scalatest.{FunSuite, NonImplicitAssertions}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
import spire.std.long._

import algebra._
import algos._
import math._
import std.arrayarray._
import std.array._
import syntax.all._

class LUSuite extends FunSuite with NonImplicitAssertions {
  test("Linear solver") {
    val mA = DenseM.forRational.build(3, 3,
       3,  2, -1,
       2, -2,  4,
      -1, Rational(1, 2), -1)
    val vb = DenseV.forRational.build(1, -2, 0)
    val res = DenseV.forRational.build(1, -2, -2)
    assert(lu(mA).solve(vb) === res)
  }
}
