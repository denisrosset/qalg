package com.faacets.qalg

import org.scalatest.FunSuite
import spire.math.Rational
import spire.syntax.innerProductSpace._

import algebra._
import math._
import syntax.all._

class DenseSchaumSuite extends SchaumSuite[DenseM[Rational], DenseV[Rational]] {
  def M = MatVecInField[DenseM[Rational], DenseV[Rational], Rational]
}

class DenseLinearAlgebraSuite extends LinearAlgebraSuite[DenseM[Rational], DenseV[Rational]] {
  def M = MatVecInField[DenseM[Rational], DenseV[Rational], Rational]
}
