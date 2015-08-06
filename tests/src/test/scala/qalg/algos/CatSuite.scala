package com.faacets.qalg

import org.scalatest.{FunSuite, NonImplicitAssertions}
import spire.math.Rational
import spire.syntax.eq._
import spire.syntax.innerProductSpace._
import spire.std.long._

import algebra._
import algos._
import math._
import optional.matProductOrder._
import optional.vecProductOrder._
import Matrix.packs._
import syntax.all._

class CatSuite extends FunSuite with NonImplicitAssertions {
  type V = Vector[Rational, Imm]
  type M = Matrix[Rational, Imm]
  val V = VecBuild[V, Rational]
  val M = MatBuild[M, Rational]
  val m1 = zeros[M](2, 2)
  val m2 = ones[M](2, 2)
  val v1 = zeros[V](2)
  val v2 = ones[V](2)
  test("horzcat") {
    assert(horzcat(m1, m2) ===
      M.rowMajor(2, 4)(
        0L, 0L, 1L, 1L,
        0L, 0L, 1L, 1L
      ))
  }
  test("vertcat") {
    assert(vertcat(m1, m2) ===
      M.rowMajor(4, 2)(
        0L, 0L,
        0L, 0L,
        1L, 1L,
        1L, 1L
      ))
  }
  test("cat") {
    assert(cat(v1, v2) === V.build(0L, 0L, 1L, 1L))
  }
}
