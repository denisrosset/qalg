package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._

import algebra._

trait PackField[@sp(Double, Long) A] extends Any with PackEuclideanRing[A] {
  implicit def M: MatField[M, A] with MatSlice[M, V, A]
  implicit def V: VecField[V, A]
  implicit def A: Field[A]

  implicit def MRref: Rref[M]
  implicit def MLU: LU[M, V, A]
}

object PackField {
  type ForM[M0, A] = PackField[A] { type M = M0 }
  type ForV[V0, A] = PackField[A] { type V = V0 }
  type ForMV[M0, V0, A] = PackField[A] { type M = M0; type V = V0 }
}
