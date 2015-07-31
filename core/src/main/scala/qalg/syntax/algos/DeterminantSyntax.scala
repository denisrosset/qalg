package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait DeterminantSyntax {
  implicit class DeterminantOps[M, @sp(Double, Long) A](m: M)(implicit M: Determinant[M, A]) {
    def determinant: A = M.determinant(m)
  }
}
