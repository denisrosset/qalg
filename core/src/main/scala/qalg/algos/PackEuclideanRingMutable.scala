package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._

import algebra._

trait PackEuclideanRingMutable[@sp(Double, Long) A] extends Any with PackEuclideanRing[A] with PackRingMutable[A] {
  implicit def MGramSchmidt: MutableMGramSchmidt[M]
  implicit def MPrime: MutablePrime[M, A]
  implicit def VPrime: MutablePrime[V, A]
}

object PackEuclideanRingMutable {
  type ForM[M0, A] = PackEuclideanRingMutable[A] { type M = M0 }
  type ForV[V0, A] = PackEuclideanRingMutable[A] { type V = V0 }
  type ForMV[M0, V0, A] = PackEuclideanRingMutable[A] { type M = M0; type V = V0 }
}
