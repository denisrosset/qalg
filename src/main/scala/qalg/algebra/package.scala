package com.faacets.qalg

import spire.math.Rational
import spire.std.any._

package object algebra {
  implicit val FunRationalAlgebra: FunAlgebra[Rational] = new FunAlgebra[Rational]
  implicit val FunDoubleAlgebra: FunAlgebra[Double] = new FunAlgebra[Double]
  implicit val FunLongAlgebra: FunAlgebra[Long] = new FunAlgebra[Long]
}
