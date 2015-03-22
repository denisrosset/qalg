package com.faacets.qalg

import org.scalatest.FunSuite
import spire.math.Rational
import spire.syntax.innerProductSpace._

import algebra._
import math._
import syntax.all._

import org.jlinalg.{Vector => JLVector, Matrix => JLMatrix}
import org.jlinalg.rational.{Rational => JLRational}

import std.jlinalg._
import std.jlinalg.converted._

class JLinAlgSchaumSuite extends SchaumSuite[JLMatrix[JLRational], JLVector[JLRational]] {
  def M = MatVecInField[JLMatrix[JLRational], JLVector[JLRational], Rational]
}

class JLinAlgLinearAlgebraSuite extends LinearAlgebraSuite[JLMatrix[JLRational], JLVector[JLRational]] {
  def M = MatVecInField[JLMatrix[JLRational], JLVector[JLRational], Rational]
}
