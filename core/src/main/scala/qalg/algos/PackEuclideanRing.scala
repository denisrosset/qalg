package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._

import algebra._

trait PackEuclideanRing[@sp(Double, Long) A] extends Any with PackRing[A] {
  implicit def A: EuclideanRing[A]

  implicit def VPrime: Prime[V, A]

  implicit def MPrime: Prime[M, A]
  implicit def MRank: Rank[M]
  implicit def MGramSchmidt: MGramSchmidt[M]

  implicit def VGramSchmidt: VGramSchmidt[V, A]
}

object PackEuclideanRing {
  type ForM[M0, A] = PackEuclideanRing[A] { type M = M0 }
  type ForV[V0, A] = PackEuclideanRing[A] { type V = V0 }
  type ForMV[M0, V0, A] = PackEuclideanRing[A] { type M = M0; type V = V0 }
}

