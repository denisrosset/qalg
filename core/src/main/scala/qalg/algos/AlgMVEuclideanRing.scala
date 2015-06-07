package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._

trait AlgMVEuclideanRing[M, V, @sp(Double, Long) A] extends Any with AlgMVRing[M, V, A] with PackMVEuclideanRing[M, V, A] {
  implicit def MGramSchmidt: GramSchmidt[M]
  implicit def MutMGramSchmidt: MutableGramSchmidt[MutM]
}
