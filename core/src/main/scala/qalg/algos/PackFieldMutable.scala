package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._

import algebra._

trait PackFieldMutable[@sp(Double, Long) A] extends Any with PackField[A] with PackEuclideanRingMutable[A] {
  implicit def MRref: MutableRref[M]
  implicit def MLU: MutableLU[M, V, A]
}

object PackFieldMutable {
  type ForM[M0, A] = PackFieldMutable[A] { type M = M0 }
  type ForV[V0, A] = PackFieldMutable[A] { type V = V0 }
  type ForMV[M0, V0, A] = PackFieldMutable[A] { type M = M0; type V = V0 }
}
