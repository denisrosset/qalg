package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait RrefSyntax {
  implicit class RrefOps[M](m: M)(implicit M: Rref[M]) {
    def rref: RrefDecomposition[M] = M.rref(m)
  }
  implicit class MutableRrefOps[M](m: M)(implicit M: MutableRref[M]) {
    def unsafeRref: RrefDecomposition[M] = M.unsafeRref(m)
  }
}
