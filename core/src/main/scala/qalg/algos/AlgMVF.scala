package com.faacets.qalg
package algos

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._

import algebra._

trait AlgMVF[M, V, @sp(Double, Long) A] extends Any with AlgMVE[M, V, A] with PackMVF[M, V, A] with AlgVF[V, A] {
  implicit def MRref: Rref[M]
  implicit def MLU: LU[M, V, A]
}

trait AlgUMVF[M, V, @sp(Double, Long) A] extends Any with AlgUMVE[M, V, A] with AlgMVF[M, V, A] with PackMVF[M, V, A] with AlgUVF[V, A] {
  implicit def MRref: MutableRref[M]
  implicit def MLU: MutableLU[M, V, A]
}

final class AlgUMVFImpl[M0: ClassTag, V0, @sp(Double) A: Eq: Pivot](implicit val M: MatInField[M0, A], val UM: MatMutable[M0, A], val V: VecInField[V0, A], val UV: VecMutable[V0, A], val MVProduct: MatVecProduct[M0, V0], val MVSlicer: MatSlicer[M0, V0]) extends AlgUMVF[M0, V0, A] { // no @sp
  implicit def A: Field[A] = M.A
  lazy val MCat: MatCat[M, A] = new MatCatImpl[M, A]
  implicit lazy val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
  lazy val MKron: Kron[M] = new MatKronImpl[M, A]
  implicit lazy val MShift: MutableMatShift[M] = new MutableMatShiftImpl[M, A]
  lazy val MTrace: Trace[M, A] = new TraceImpl[M, A]
  lazy val MDeterminant: Determinant[M, A] = new DeterminantMahajanVinay[M, A]
  implicit lazy val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
  lazy val VCat: VecCat[V, A] = new VecCatImpl[V, A]
  lazy val VKron: Kron[V] = new VecKronImpl[V, A]
  implicit lazy val VShift: MutableVecShift[V] = new MutableVecShiftImpl[V, A]
  lazy val MGramSchmidt: MutableGramSchmidt[M] = new GramSchmidtNonNorm[M, A]
  lazy val MPrime: MutablePrime[M, A] = new MatMutablePrimeImpl[M, A]
  lazy val VPrime: MutablePrime[V, A] = new VecMutablePrimeImpl[V, A]
  lazy val MLU: MutableLU[M, V, A] = new MutableLUImpl[M, V, A]
  lazy val MRref: MutableRref[M] = new MutableRrefImpl[M, A]
  lazy val MRank: Rank[M] = new Rank[M] {
    def rank(m: M): Int = MRref.rref(m).basis.length
  }
}

final class AlgMVFImpl[M0, V0, @sp(Double) A: Eq, UM, UV](implicit val M: MatInField[M0, A], val V: VecInField[V0, A], val MVProduct: MatVecProduct[M0, V0], val MVSlicer: MatSlicer[M0, V0], A: EuclideanRing[A], U: AlgUMVF[UM, UV, A], convM: ConvM[M0, UM, A], convV: ConvV[V0, UV, A]) extends AlgMVF[M0, V0, A] { // no @sp
  lazy val MCat: MatCat[M, A] = new MatCatImpl[M, A]
  implicit lazy val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
  lazy val MKron: Kron[M] = new MatKronImpl[M, A]
  lazy val MShift: MatShift[M] = new MatShiftImpl[M, A]
  lazy val MTrace: Trace[M, A] = new TraceImpl[M, A]
  lazy val MDeterminant: Determinant[M, A] = new Determinant[M, A] {
    def determinant(m: M): A = U.MDeterminant.determinant(convM.unsafeToUM(m))
  }
  lazy val MGramSchmidt: GramSchmidt[M] = new GramSchmidt[M] {
    def gramSchmidt(m: M): M = convM.unsafeToIM(U.MGramSchmidt.gramSchmidt(convM.toUM(m)))
  }
  lazy val MLU: LU[M, V, A] = new LU[M, V, A] {
    def lu(m: M): LUDecomposition[M, V, A] = U.MLU.unsafeLU(convM.toUM(m)).map(convM.unsafeToIM, convM.unsafeToUM, convV.unsafeToIV, convV.unsafeToUV)
  }
  lazy val MRref: Rref[M] = new Rref[M] {
    def rref(m: M): RrefDecomposition[M] = U.MRref.unsafeRref(convM.toUM(m)).map(convM.unsafeToIM)
  }
  lazy val MPrime: Prime[M, A] = new PrimeImpl[M, A]
  lazy val VPrime: Prime[V, A] = new PrimeImpl[V, A]
  implicit lazy val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
  lazy val VCat: VecCat[V, A] = new VecCatImpl[V, A]
  lazy val VKron: Kron[V] = new VecKronImpl[V, A]
  lazy val VShift: VecShift[V] = new VecShiftImpl[V, A]
  lazy val MRank: Rank[M] = new Rank[M] {
    def rank(m: M): Int = MRref.rref(m).basis.length
  }
}
