package com.faacets.qalg
package algebra

import scala.{specialized => sp}

trait PackMRing[M, @sp(Double, Long) A] extends Any {
  implicit def M: MatInRing[M, A]
  type MutM
  type MutV
  def toMutM(m: M): MutM = MutMutM.copy(unsafeToMutM(m))
  def fromMutM(m: MutM): M = unsafeFromMutM(MutMutM.copy(m))
  def unsafeToMutM(m: M): MutM
  def unsafeFromMutM(m: MutM): M
  implicit def MutMutM: MatMutable[MutM, A]
  implicit def MutMutV: VecMutable[MutV, A]
  implicit def MutM: MatInRing[MutM, A]
  implicit def MutV: VecInRing[MutV, A]
  implicit def MutProduct: MatVecProduct[MutM, MutV]
  implicit def MutSlicer: MatSlicer[MutM, MutV]
}
