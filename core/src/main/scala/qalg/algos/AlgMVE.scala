package com.faacets.qalg
package algos

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.additiveGroup._

import algebra._

trait AlgMVE[M, V, @sp(Double, Long) A] extends Any with AlgMVR[M, V, A] {
  implicit def MGramSchmidt: MGramSchmidt[M]
  implicit def MPrime: Prime[M, A]
  implicit def VPrime: Prime[V, A]
  implicit def MRank: Rank[M]
}

trait AlgUMVE[M, V, @sp(Double, Long) A] extends Any with AlgUMVR[M, V, A] with AlgMVE[M, V, A] {
  implicit def MGramSchmidt: MutableMGramSchmidt[M]
  implicit def MPrime: MutablePrime[M, A]
  implicit def VPrime: MutablePrime[V, A]
}

final class AlgUMVEImpl[M0: ClassTag, V0, @sp(Long) A: Eq](implicit val M: MatInRing[M0, A], val UM: MatMutable[M0, A], val V: VecInRing[V0, A], val UV: VecMutable[V0, A], val MVProduct: MatVecProduct[M0, V0], val MVSlicer: MatSlicer[M0, V0], A: EuclideanRing[A]) extends AlgUMVE[M0, V0, A] { // no @sp
  implicit lazy val MCat: MatCat[M, A] = new MatCatImpl[M, A]
  implicit lazy val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
  lazy val MKron: Kron[M] = new MatKronImpl[M, A]
  lazy val MShift: MutableMatShift[M] = new MutableMatShiftImpl[M, A]
  lazy val MTrace: Trace[M, A] = new TraceImpl[M, A]
  lazy val MDeterminant: Determinant[M, A] = new DeterminantMahajanVinay[M, A]
  implicit lazy val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
  lazy val VCat: VecCat[V, A] = new VecCatImpl[V, A]
  lazy val VKron: Kron[V] = new VecKronImpl[V, A]
  lazy val VShift: MutableVecShift[V] = new MutableVecShiftImpl[V, A]
  lazy val MGramSchmidt: MutableMGramSchmidt[M] = new GramSchmidtE[M, V, A]
  lazy val VGramSchmidt: VGramSchmidt[V, A] = new GramSchmidtE[M, V, A]
  lazy val MPrime: MutablePrime[M, A] = new MatMutablePrimeImpl[M, A]
  lazy val VPrime: MutablePrime[V, A] = new VecMutablePrimeImpl[V, A]
  lazy val MRank: Rank[M] = new Rank[M] {
    def rank(m: M): Int = {
      val res = MGramSchmidt.orthogonalized(m)
      cforRange(M.nRows(m) - 1 to 0 by -1) { r =>
        cforRange(0 until M.nCols(m)) { c =>
          if (!M.apply(m, r, c).isZero) return r + 1
        }
      }
      0
    }
  }
}

final class AlgMVEImpl[M0, V0, @sp(Long) A: Eq, UM](implicit val M: MatInRing[M0, A], val V: VecInRing[V0, A], val MVProduct: MatVecProduct[M0, V0], val MVSlicer: MatSlicer[M0, V0], A: EuclideanRing[A], U: AlgUMVE[UM, _, A], convM: ConvM[M0, UM, A]) extends AlgMVE[M0, V0, A] { // no @sp
  lazy val MCat: MatCat[M, A] = new MatCatImpl[M, A]
  implicit lazy val MFactory: MatFactory[M] = new MatFactoryImpl[M, A]
  lazy val MKron: Kron[M] = new MatKronImpl[M, A]
  lazy val MShift: MatShift[M] = new MatShiftImpl[M, A]
  lazy val MTrace: Trace[M, A] = new TraceImpl[M, A]
  lazy val MDeterminant: Determinant[M, A] = new Determinant[M, A] {
    def determinant(m: M): A = U.MDeterminant.determinant(convM.unsafeToUM(m))
  }
  lazy val MGramSchmidt: MGramSchmidt[M] = new MGramSchmidt[M] {
    def orthogonalized(m: M): M = convM.unsafeToIM(U.MGramSchmidt.orthogonalized(convM.toUM(m)))
  }
  lazy val MPrime: Prime[M, A] = new PrimeImpl[M, A]
  implicit lazy val VFactory: VecFactory[V] = new VecFactoryImpl[V, A]
  lazy val VCat: VecCat[V, A] = new VecCatImpl[V, A]
  lazy val VKron: Kron[V] = new VecKronImpl[V, A]
  lazy val VShift: VecShift[V] = new VecShiftImpl[V, A]
  lazy val VPrime: Prime[V, A] = new PrimeImpl[V, A]
  lazy val MRank: Rank[M] = new Rank[M] {
    def rank(m: M): Int = {
      val res = MGramSchmidt.orthogonalized(m)
      cforRange(M.nRows(m) - 1 to 0 by -1) { r =>
        cforRange(0 until M.nCols(m)) { c =>
          if (!M.apply(m, r, c).isZero) return r + 1
        }
      }
      0
    }
  }
}
