package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._

import algebra._

trait PackRingMutable[@sp(Double, Long) A] extends Any with PackRing[A] {
  implicit def MM: MatMut[M, A]
  implicit def VM: VecMut[V, A]

  implicit def MShift: MutableMatShift[M]
  implicit def VShift: MutableVecShift[V]
}

object PackRingMutable {
  type ForM[M0, A] = PackRingMutable[A] { type M = M0 }
  type ForV[V0, A] = PackRingMutable[A] { type V = V0 }
  type ForMV[M0, V0, A] = PackRingMutable[A] { type M = M0; type V = V0 }
}
