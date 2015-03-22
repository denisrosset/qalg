package com.faacets.qalg

import org.scalatest.FunSuite
import spire.math.Rational
import spire.syntax.innerProductSpace._

import algebra._
import math._
import syntax.all._

import org.jscience.mathematics.number.{Rational => JSRational}
import org.jscience.mathematics.vector.{DenseVector => JSDenseVector, DenseMatrix => JSDenseMatrix}

import std.jscience._
import std.jscience.converted._

class JScienceSchaumSuite extends SchaumSuite[JSDenseMatrix[JSRational], JSDenseVector[JSRational]] {
  def M = MatVecInField[JSDenseMatrix[JSRational], JSDenseVector[JSRational], Rational]
}

class JScienceLinearAlgebraSuite extends LinearAlgebraSuite[JSDenseMatrix[JSRational], JSDenseVector[JSRational]] {
  def M = MatVecInField[JSDenseMatrix[JSRational], JSDenseVector[JSRational], Rational]
}
