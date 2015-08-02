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
import optional.vecProductOrder._

class DeterminantSuite[M, V](implicit val pack: PackRing.ForMV[M, V, Rational]) extends FunSuite with NonImplicitAssertions {
  def M = pack.M
  test("Linear solver") {
    val mA = M.rowMajor(3, 3)(
      -2, 2, -3,
      -1, 1, 3,
      2, 0, -1)
    assert(mA.determinant === 18)
  }
}

final class DenseDeterminantSuite extends DeterminantSuite[Matrix[Rational, Imm], Vector[Rational, Imm]]
