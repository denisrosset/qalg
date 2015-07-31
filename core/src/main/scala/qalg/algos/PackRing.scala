package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._

import algebra._

trait PackRing[@sp(Double, Long) A] extends Any {
  type M
  type V

  implicit def M: MatRing[M, A] with MatSlice[M, V, A]
  implicit def V: VecRing[V, A]
  implicit def A: Ring[A]

  implicit def MatVecProduct: MatVecProduct[M, V]

  implicit def VFactory: VecFactory[V]
  implicit def VKron: Kron[V]
  implicit def VCat: VecCat[V, A]
  implicit def VShift: VecShift[V]

  implicit def MFactory: MatFactory[M]
  implicit def MKron: Kron[M]
  implicit def MCat: MatCat[M, A]
  implicit def MShift: MatShift[M]
  implicit def MDeterminant: Determinant[M, A]
  implicit def MTrace: Trace[M, A]
}

object PackRing {
  type ForM[M0, A] = PackRing[A] { type M = M0 }
  type ForV[V0, A] = PackRing[A] { type V = V0 }
  type ForMV[M0, V0, A] = PackRing[A] { type M = M0; type V = V0 }
}
