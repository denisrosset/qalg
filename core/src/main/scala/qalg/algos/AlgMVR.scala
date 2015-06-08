package com.faacets.qalg
package algos

import scala.{specialized => sp}

import scala.reflect.ClassTag

import algebra._

trait AlgMVR[M0, V0, @sp(Double, Long) A] extends Any with PackMVR[M0, V0, A] {
  type M = M0
  type V = V0
  implicit def MFactory: MatFactory[M]
  implicit def VFactory: VecFactory[V]
  implicit def MKron: Kron[M]
  implicit def VKron: Kron[V]
  implicit def MCat: MatCat[M, A]
  implicit def VCat: VecCat[V, A]
  implicit def MShift: MatShift[M]
  implicit def VShift: VecShift[V]
  implicit def MDeterminant: Determinant[M, A]
  implicit def MTrace: Trace[M, A]
}

trait AlgUMVR[M0, V0, @sp(Double, Long) A] extends Any with AlgMVR[M0, V0, A] with PackUM[M0, A] with PackUV[V0, A] {
  implicit def MShift: MutableMatShift[M]
  implicit def VShift: MutableVecShift[V]
}

/*
trait AlgMVRImpl[M0, V0, @sp(Double, Long) A] extends AlgMVR[M0, V0, A] { self =>
  object MKron extends MatKronImpl[M, A] { def M = self.M }
  object VKron extends VecKronImpl[V, A] { def V = self.V }
  object MCat extends MatCatImpl[M, A] { def M1 = self.M }
  object VCat extends VecCatImpl[V, A] { def V1 = self.V }
  object MTrace extends TraceImpl[M, A] { def M = self.M }
}

trait AlgIMVRImpl[M0, V0, @sp(Double, Long) A] extends AlgMVR[M0, V0, A] { self =>
  object MShift = new MatShiftImpl[M, A] { def M: MatInRing[M, A] = self.M }
  object VShift = new VecShiftImpl[V, A] { def V: VecInRing[V, A] = self.V }
}

trait AlgUMVRImpl[M0, V0, @sp(Double, Long) A] extends AlgUMVR[M0, V0, A] with AlgMVRImpl[M0, V0, A] { self =>
  implicit def classTagM: ClassTag[M]

  override lazy val MShift = new MutableMatShiftImpl[M, A] {
    def M: MatInRing[M, A] = self.M
    def MM: MatMutable[M, A] = self.UM
  }
  override lazy val VShift = new MutableVecShiftImpl[V, A] {
    def V: VecInRing[V, A] = self.V
    def VM: VecMutable[V, A] = self.UV
  }
  lazy val MDeterminant: Determinant[M, A] = new DeterminantMutableRingImpl[M, A] {
    def classTagM = self.classTagM
    def M = self.M
    def MF = self.MFactory
    def MM = self.UM
 }
}
 */
