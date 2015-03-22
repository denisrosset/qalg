package com.faacets.qalg

import org.scalatest.FunSuite
import spire.math.Rational
import spire.syntax.innerProductSpace._

import algebra._
import math._
import syntax.all._

/** Examples and tests taken from Lipschutz-Lipson 2004.
  * 
  * Full reference:
  * 
  * Lipschutz-Lipson, Schaum's Outline of Theory and Problems of Linear Algebra, 3rd Edition, McGraw-Hill, 2004
  */
trait SchaumSuite[M, V] extends FunSuite {
  implicit def M: MatVecInField[M, V, Rational]
  implicit def V: VecInField[V, Rational] = M.V
  test("Example 1.2") {
    val u = V.build(2, 4, -5)
    val v = V.build(1, -6, 9)
    assert(u + v === V.build(3, -2, 4))
     assert(u :* 7 === V.build(14, 28, -35))
     assert(-v === V.build(-1, 6, -9))
     assert((u :* 3) - (v :* 5) === V.build(1, 42, -60))
  }
  test("Example 1.3a") {
    val u = V.build(1, -2, 3)
    val v = V.build(4, 5, -1)
    val w = V.build(2, 7, 4)
    assert(u.dot(v) === -9)
    assert(u.dot(w) === 0)
    assert(v.dot(w) === 39)
  }
  test("Example 1.3b") {
    val u = V.build(2, 3, -4)
    val v = V.build(3, -1, -2)
    assert(u.dot(v) === 11)
  }
  test("Example 2.2") {
    val a = M.build(2, 3,
      1, -2, 3,
      0, 4, 5)
    val b = M.build(2, 3,
      4, 6, 8,
      1, -3, -7)
    assert(a + b === M.build(2, 3,
      5, 4, 11,
      1, 1, -2))
    assert(a:* 3 === M.build(2, 3,
      3, -6, 9,
      0, 12, 15))
    assert((a:* 2) - (b:* 3) === M.build(2, 3,
      -10, -22, -18,
      -3, 17, 31))
  }
  test("Example 2.4") {
    assert(M.build(1,3, 7, -4, 5)*M.build(3,1,3,2,-1) === M.build(1,1,8))
    assert(M.build(1,4, 6,-1,8,3)*M.build(4,1, 4,-9,-2,5) === M.build(1,1,32))
  }
  test("Example 2.5") {
    assert(M.build(2, 2, 1, 3, 2, -1) * M.build(2, 3, 2, 0, -4, 5, -2, 6) === M.build(2, 3, 17, -6, 14, -1, 2, -14))
  }
  test("Exercice 2.8a") {
    assert(M.build(2, 2, 1, 6, -3, 5) ::* V.build(2, -7) === V.build(-40, -41))
  }
}

class DenseSchaumSuite extends SchaumSuite[DenseM[Rational], DenseV[Rational]] {
  def M = MatVecInField[DenseM[Rational], DenseV[Rational], Rational]
}

import std.commons._
import std.commons.converted._

import org.apache.commons.math3.fraction._
import org.apache.commons.math3.linear._

class CommonsBigSchaumSuite extends SchaumSuite[FieldMatrix[BigFraction], FieldVector[BigFraction]] {
  def M = MatVecInField[FieldMatrix[BigFraction], FieldVector[BigFraction], Rational]
}

class CommonsSchaumSuite extends SchaumSuite[FieldMatrix[Fraction], FieldVector[Fraction]] {
  def M = MatVecInField[FieldMatrix[Fraction], FieldVector[Fraction], Rational]
}

import org.jscience.mathematics.number.{Rational => JSRational}
import org.jscience.mathematics.vector.{DenseVector => JSDenseVector, DenseMatrix => JSDenseMatrix}

import std.jScience._
import std.jScience.converted._

class JScienceSchaumSuite extends SchaumSuite[JSDenseMatrix[JSRational], JSDenseVector[JSRational]] {
  def M = MatVecInField[JSDenseMatrix[JSRational], JSDenseVector[JSRational], Rational]
}

import org.jlinalg.{Vector => JLVector, Matrix => JLMatrix}
import org.jlinalg.rational.{Rational => JLRational}

import std.jLinAlg._
import std.jLinAlg.converted._

class JLinAlgSchaumSuite extends SchaumSuite[JLMatrix[JLRational], JLVector[JLRational]] {
  def M = MatVecInField[JLMatrix[JLRational], JLVector[JLRational], Rational]
}
