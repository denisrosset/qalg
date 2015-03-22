package com.faacets.qalg

import org.scalatest.FunSuite
import spire.math.Rational
import spire.syntax.innerProductSpace._

import algebra._
import math._
import syntax.all._

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

class CommonsBigLinearAlgebraSuite extends LinearAlgebraSuite[FieldMatrix[BigFraction], FieldVector[BigFraction]] {
  def M = MatVecInField[FieldMatrix[BigFraction], FieldVector[BigFraction], Rational]
}

class CommonsLinearAlgebraSuite extends LinearAlgebraSuite[FieldMatrix[Fraction], FieldVector[Fraction]] {
  def M = MatVecInField[FieldMatrix[Fraction], FieldVector[Fraction], Rational]
}
