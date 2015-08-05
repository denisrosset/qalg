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
import Matrix.packs._
import optional.matProductOrder._

class GramSchmidtSuite(implicit val pack: PackField.ForMV[Matrix[Rational, Imm], Vector[Rational, Imm], Rational]) extends FunSuite with NonImplicitAssertions {
  def M = pack.M
  test("Rational Gram Schmidt") {
    val m = M.rowMajor(2, 3)(
      3, 1, 1,
      2, 2, 1)
    val res = M.rowMajor(2, 3)(
      3, 1, 1,
      Rational(-5, 11), Rational(13, 11), Rational(2, 11))
    assert(m.orthogonalized === res)
  }
  /*
  test("Integer Gram Schmidt") {
    val m = DenseM.forLong.build(2, 3,
      3, 1, 1,
      2, 2, 1)
    val res = DenseM.forLong.build(2, 3,
      3, 1, 1,
      -5, 13, 2)
    assert(euclideanGramSchmidt(m) === res)
  }*/
}
