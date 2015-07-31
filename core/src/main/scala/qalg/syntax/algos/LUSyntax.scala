package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait LUSyntax {
  implicit class LUOps[M, V, @sp(Double) A](m: M)(implicit M: LU[M, V, A]) { // not @sp(Long)
    def lu: LUDecomposition[M, V, A] = M.lu(m)
  }
  implicit class MutableLUOps[M, V, @sp(Double) A](m: M)(implicit M: MutableLU[M, V, A]) { // not @sp(Long)
    def unsafeLU(m: M): LUDecomposition[M, V, A] = M.unsafeLU(m)
  }
}
