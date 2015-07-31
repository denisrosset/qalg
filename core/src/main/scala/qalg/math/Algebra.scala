package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._

import indup.algebra._

import algebra._
import algos._

final class PackFM[@sp(Double) A: Eq: ClassTag: Pivot](implicit val M: MatField[Matrix[A, Mut], A] with MatSlice[Matrix[A, Mut], Vector[A, Mut], A], val V: VecField[Vector[A, Mut], A], val MatVecProduct: MatVecProduct[Matrix[A, Mut], Vector[A, Mut]], val MM: MatMut[Matrix[A, Mut], A], val VM: VecMut[Vector[A, Mut], A]) extends PackFieldMutable[A] {
  type M = Matrix[A, Mut]
  type V = Vector[A, Mut]
  implicit val A: Field[A] = M.scalar

  implicit val MFactory = new algos.impl.MatFactoryImpl[M, A]
  implicit val VFactory = new algos.impl.VecFactoryImpl[V, A]
  implicit val MCat = new algos.impl.DenseMatCatImpl[M, A]
  implicit val VCat = new algos.impl.DenseVecCatImpl[V, A]
  implicit val MShift = new algos.impl.DenseMutableMatShiftImpl[M, A]
  implicit val VShift = new algos.impl.DenseMutableVecShiftImpl[V, A]

  val MKron = new algos.impl.DenseMatKronImpl[M, A]
  val VKron = new algos.impl.DenseVecKronImpl[V, A]
  val MTrace = new algos.impl.DenseTraceImpl[M, A]
  val MDeterminant = new algos.impl.DeterminantMahajanVinay[M, A]
  val MLU = new algos.impl.DenseMutableLUImpl[M, V, A]
  val MRref = new algos.impl.DenseMutableRrefImpl[M, A]

  val MGramSchmidt = new algos.impl.DenseGramSchmidtE[M, V, A]
  def VGramSchmidt = MGramSchmidt

  val MRank: Rank[M] = new Rank[M] {
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

  val MPrime = new algos.impl.DenseMatMutablePrimeImpl[M, A]

  val VPrime = new algos.impl.DenseVecMutablePrimeImpl[V, A]
}

final class PackF[@sp(Double) A: Eq: ClassTag: Pivot, IM <: ImmMut](implicit val M: MatField[Matrix[A, IM], A] with MatSlice[Matrix[A, IM], Vector[A, IM], A], val V: VecField[Vector[A, IM], A], val MatVecProduct: MatVecProduct[Matrix[A, IM], Vector[A, IM]], mutablePack: PackFieldMutable.ForMV[Matrix[A, Mut], Vector[A, Mut], A]) extends PackField[A] {
  type M = Matrix[A, IM]
  type V = Vector[A, IM]
  type MM = Matrix[A, Mut]
  type VM = Vector[A, Mut]
  def unsafeToMutable(m: M): MM = m.asInstanceOf[MM]
  def unsafeFromMutable(m: MM): M = m.asInstanceOf[M]
  def unsafeToMutable(v: V): VM = v.asInstanceOf[VM]
  def unsafeFromMutable(v: VM): V  = v.asInstanceOf[V]
  implicit val A: Field[A] = M.scalar

  implicit val MFactory = new algos.impl.MatFactoryImpl[M, A]
  implicit val VFactory = new algos.impl.VecFactoryImpl[V, A]
  implicit val MCat = new algos.impl.DenseMatCatImpl[M, A]
  implicit val VCat = new algos.impl.DenseVecCatImpl[V, A]
  implicit val MShift = new algos.impl.DenseMatShiftImpl[M, A]
  implicit val VShift = new algos.impl.DenseVecShiftImpl[V, A]

  val MKron = new algos.impl.DenseMatKronImpl[M, A]
  val VKron = new algos.impl.DenseVecKronImpl[V, A]
  val MTrace = new algos.impl.DenseTraceImpl[M, A]

  val MDeterminant = new Determinant[M, A] {
    def determinant(m: M): A = mutablePack.MDeterminant.determinant(unsafeToMutable(m))
  }

  val MLU = new LU[M, V, A] {
    def lu(m: M): LUDecomposition[M, V, A] = mutablePack.MLU.lu(unsafeToMutable(m)).map(unsafeFromMutable, unsafeToMutable, unsafeFromMutable, unsafeToMutable)
  }

  val MRref = new Rref[M] {
    def rref(m: M): RrefDecomposition[M] = mutablePack.MRref.rref(unsafeToMutable(m)).map(unsafeFromMutable)
  }

  val MGramSchmidt = new MGramSchmidt[M] {
    def orthogonalized(m: M): M = unsafeFromMutable(mutablePack.MGramSchmidt.orthogonalized(unsafeToMutable(m)))
  }

  val VGramSchmidt = new VGramSchmidt[V, A] {
    def orthogonalBasis(vs: Seq[V]): Seq[V] = mutablePack.VGramSchmidt.orthogonalBasis(vs.map(unsafeToMutable)).map(unsafeFromMutable)
    def orthogonalComplement(vs: Seq[V], d: Int): Seq[V] = mutablePack.VGramSchmidt.orthogonalComplement(vs.map(unsafeToMutable), d).map(unsafeFromMutable)
    def orthogonalized[V1](v: V, other: V1*)(implicit V1: Vec[V1, A]): V = unsafeFromMutable(mutablePack.VGramSchmidt.orthogonalized(unsafeToMutable(v), other: _*))
  }

  val MRank: Rank[M] = new Rank[M] {
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

  val MPrime = new algos.impl.DenseMatPrimeImpl[M, A]

  val VPrime = new algos.impl.DenseVecPrimeImpl[V, A]
}
