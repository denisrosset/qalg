package com.faacets.qalg
package algos

import scala.{specialized => sp}

import scala.reflect.ClassTag

import algebra._

trait AlgMVR[M0, V0, @sp(Double, Long) A] extends Any with PackMVR[M0, V0, A] with AlgVR[V0, A] {
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

trait AlgUMVR[M0, V0, @sp(Double, Long) A] extends Any with AlgMVR[M0, V0, A] with PackUM[M0, A] with PackUV[V0, A] with AlgUVR[V0, A] {
  implicit def MShift: MutableMatShift[M]
  implicit def VShift: MutableVecShift[V]
}

final class AlgUMVRImpl[M0: ClassTag, V0, A](implicit val M: MatInRing[M0, A], val UM: MatMutable[M0, A], val V: VecInRing[V0, A], val UV: VecMutable[V0, A], val MVProduct: MatVecProduct[M0, V0], val MVSlicer: MatSlicer[M0, V0]) extends AlgUMVR[M0, V0, A] { // no @sp
  val MCat: MatCat[M, A] = new MatCatImpl[M, A]
  implicit val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
  val MKron: Kron[M] = new MatKronImpl[M, A]
  val MShift: MutableMatShift[M] = new MutableMatShiftImpl[M, A]
  val MTrace: Trace[M, A] = new TraceImpl[M, A]
  val MDeterminant: Determinant[M, A] = new DeterminantMahajanVinay[M, A]
  implicit val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
  val VCat: VecCat[V, A] = new VecCatImpl[V, A]
  val VKron: Kron[V] = new VecKronImpl[V, A]
  val VShift: MutableVecShift[V] = new MutableVecShiftImpl[V, A]
}

final class AlgMVRImpl[M0, V0, A, UM](implicit val M: MatInRing[M0, A], val V: VecInRing[V0, A], val MVProduct: MatVecProduct[M0, V0], val MVSlicer: MatSlicer[M0, V0], U: AlgUMVR[UM, _, A], CM: ConvM[M0, UM, A]) extends AlgMVR[M0, V0, A] { // no @sp
  val MCat: MatCat[M, A] = new MatCatImpl[M, A]
  implicit val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
  val MKron: Kron[M] = new MatKronImpl[M, A]
  val MShift: MatShift[M] = new MatShiftImpl[M, A]
  val MTrace: Trace[M, A] = new TraceImpl[M, A]
  val MDeterminant: Determinant[M, A] = new Determinant[M, A] {
    def determinant(m: M): A = U.MDeterminant.determinant(CM.unsafeToUM(m))
  }
  implicit val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
  val VCat: VecCat[V, A] = new VecCatImpl[V, A]
  val VKron: Kron[V] = new VecKronImpl[V, A]
  val VShift: VecShift[V] = new VecShiftImpl[V, A]
}
