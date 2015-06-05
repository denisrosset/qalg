package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._

trait AlgMVField[M, V, @sp(Double, Long) A] extends Any with AlgMVEuclideanRing[M, V, A] {
  implicit def MGramSchdmit: GramSchmidt[M]
  implicit def MutMGramSchdmit: GramSchmidt[MutM]

  implicit def MRref: Rref[M]
  implicit def MutMRref: MutableRref[MutM]
}
