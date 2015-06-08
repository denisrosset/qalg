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

class DeterminantSuite[M, V](implicit val pack: AlgMVR[M, V, Long]) extends FunSuite with NonImplicitAssertions {
  import pack._
  test("Linear solver") {
    val mA = M.build(3, 3,
      -2, 2, -3,
      -1, 1, 3,
      2, 0, -1)
    assert(mA.determinant === 18)
  }
}

final class DenseDeterminantSuite extends DeterminantSuite[DenseM[Long, Immutable], DenseV[Long, Immutable]]
