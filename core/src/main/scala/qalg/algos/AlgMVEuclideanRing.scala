package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._

trait AlgMVEuclideanRing[M, V, @sp(Double, Long) A] extends Any with AlgMVRing[M, V, A] {
  implicit def MGramSchdmit: GramSchmidt[M]
  implicit def MutMGramSchdmit: GramSchmidt[MutM]
}
