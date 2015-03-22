package com.faacets.qalg

import spire.math.Rational
import spire.std.any._

package object algebra {
  implicit val FunVecRational: FunVec[Rational] = new FunVec[Rational]
  implicit val FunVecDouble: FunVec[Double] = new FunVec[Double]
  implicit val FunVecLong: FunVec[Long] = new FunVec[Long]
  implicit val FunMatRational: FunMat[Rational] = new FunMat[Rational]
  implicit val FunMatDouble: FunMat[Double] = new FunMat[Double]
  implicit val FunMatLong: FunMat[Long] = new FunMat[Long]
}
