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

class CatSuite extends FunSuite with NonImplicitAssertions {
  val V = VecBuilder[Array[Long], Long]
  val M = MatBuilder[Array[Array[Long]], Long]
  val m1 = M.zeros(2, 2)
  val m2 = M.ones(2, 2)
  val v1 = V.zeros(2)
  val v2 = V.ones(2)
  test("horzcat") {
    assert(horzcat(m1, m2) === Array(
      Array(0L, 0L, 1L, 1L),
      Array(0L, 0L, 1L, 1L)
    ))
    assert(M.horzcat(m1, m2) === Array(
      Array(0L, 0L, 1L, 1L),
      Array(0L, 0L, 1L, 1L)
    ))
  }
  test("vertcat") {
    assert(vertcat(m1, m2) === Array(
      Array(0L, 0L),
      Array(0L, 0L),
      Array(1L, 1L),
      Array(1L, 1L)
    ))
    assert(M.vertcat(m1, m2) === Array(
      Array(0L, 0L),
      Array(0L, 0L),
      Array(1L, 1L),
      Array(1L, 1L)
    ))
  }
  test("cat") {
    assert(cat(v1, v2) === Array(0L, 0L, 1L, 1L))
    assert(V.cat(v1, v2) === Array(0L, 0L, 1L, 1L))
  }
}
