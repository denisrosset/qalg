package com.faacets.qalg
package algos

import scala.{specialized => sp}

import scala.reflect.ClassTag

import algebra._

trait AlgMVRing[M0, V0, @sp(Double, Long) A] extends Any with PackMVRing[M0, V0, A] {
  type M = M0
  type V = V0
  implicit def MFactory: MatFactory[M]
  implicit def VFactory: VecFactory[V]
  implicit def MutMFactory: MatFactory[MutM]
  implicit def MutVFactory: VecFactory[MutV]
  implicit def MKron: Kron[M]
  implicit def VKron: Kron[V]
  implicit def MutMKron: Kron[MutM]
  implicit def MutVKron: Kron[MutV]
  implicit def VCat: VecCat[V, A]
  implicit def MCat: MatCat[M, A]
  implicit def MutVCat: VecCat[MutV, A]
  implicit def MutMCat: MatCat[MutM, A] 
  implicit def VShift: VecShift[V]
  implicit def MutVShift: MutableVecShift[MutV]
  implicit def MShift: MatShift[M]
  implicit def MutMShift: MutableMatShift[MutM]
  implicit def MDeterminant: Determinant[M, A]
  implicit def MutMDeterminant: Determinant[MutM, A]
  implicit def MTrace: Trace[M, A]
  implicit def MutMTrace: Trace[MutM, A]
}

trait AlgMVRingImpl[M, V, @sp(Double, Long) A] extends AlgMVRing[M, V, A] { self =>
  implicit def classTagMutM: ClassTag[MutM]
  implicit object MFactory extends MatFactoryImpl[M, A] { def M = self.M }
  implicit object VFactory extends VecFactoryImpl[V, A] { def V = self.V }
  implicit object MutMFactory extends MatFactoryImpl[MutM, A] { def M = self.MutM }
  implicit object MutVFactory extends VecFactoryImpl[MutV, A] { def V = self.MutV }
  implicit object MKron extends MatKronImpl[M, A] { def M = self.M }
  implicit object VKron extends VecKronImpl[V, A] { def V = self.V }
  implicit object MutMKron extends MatKronImpl[MutM, A] { def M = self.MutM }
  implicit object MutVKron extends VecKronImpl[MutV, A] { def V = self.MutV }
  implicit object MCat extends MatCatImpl[M, A] { def M1 = self.M }
  implicit object VCat extends VecCatImpl[V, A] { def V1 = self.V }
  implicit object MutMCat extends MatCatImpl[MutM, A] { def M1 = self.MutM }
  implicit object MutVCat extends VecCatImpl[MutV, A] { def V1 = self.MutV }
  implicit object MShift extends MatShiftImpl[M, A] { def M = self.M }
  implicit object MutMShift extends MutableMatShiftImpl[MutM, A] {
    def M = self.MutM
    def MM = self.MutMutM
  }
  implicit object VShift extends VecShiftImpl[V, A] { def V = self.V }
  implicit object MutVShift extends MutableVecShiftImpl[MutV, A] {
    def V = self.MutV
    def VM = self.MutMutV
  }
  implicit object MTrace extends TraceImpl[M, A] { def M = self.M }
  implicit object MutMTrace extends TraceImpl[MutM, A] { def M = self.MutM }
  implicit object MutMDeterminant extends DeterminantMutableRingImpl[MutM, A] {
    def classTagM = self.classTagMutM
    def M = self.MutM
    def MF = self.MutMFactory
    def MM = self.MutMutM
  }
  implicit object MDeterminant extends Determinant[M, A] {
    def determinant(m: M): A = MutMDeterminant.determinant(unsafeToMutM(m))
  }
}
