package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait PrimeSyntax {
  implicit class PrimeOps[L, @sp(Double, Long) A](l: L)(implicit L: Prime[L, A]) {
    def commonFactor: A = L.commonFactor(l)
    def withPrimes: L = L.withPrimes(l)
  }
}
