package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._

trait AlgMVField[M, V, @sp(Double, Long) A] extends Any with AlgMVEuclideanRing[M, V, A] with PackMVField[M, V, A] {
  implicit def MRref: Rref[M]
  implicit def MutMRref: MutableRref[MutM]
  implicit def MLU: LU[M, V, A]
  implicit def MutMLU: MutableLU[MutM, MutV, A]
}

trait AlgMVFieldImpl[M, V, @sp(Double, Long) A] extends AlgMVField[M, V, A] with AlgMVRingImpl[M, V, A] { self =>
  implicit def pivotA: Pivot[A]

  implicit object MGramSchmidt extends GramSchmidt[M] {
    def gramSchmidt(m: M): M = {
      val res = toMutM(m)
      MutMGramSchmidt.unsafeGramSchmidt(res)
      unsafeFromMutM(res)
    }
  }
  implicit object MutMGramSchmidt extends GramSchmidtNoRoot[MutM, A] {
    def M = self.MutM
    def MM = self.MutMutM
  }
  implicit object MRref extends Rref[M] {
    def rref(m: M): RrefDecomposition[M] =
      MutMRref.unsafeRref(toMutM(m)).map(unsafeFromMutM)
  }
  implicit object MutMRref extends MutableRrefImpl[MutM, A] {
    def M = self.MutM
    def MM = self.MutMutM
    def pivotA = self.pivotA
  }
  implicit object MLU extends LU[M, V, A] {
    def lu(m: M): LUDecomposition[M, V, A] =
      MutMLU.unsafeLU(toMutM(m)).map(unsafeFromMutM, unsafeToMutM, unsafeFromMutV, unsafeToMutV)
  }
  implicit object MutMLU extends MutableLUImpl[MutM, MutV, A] {
    def M = self.MutM
    def MM = self.MutMutM
    def V = self.MutV
    def VM = self.MutMutV
    def MS = self.MutMShift
    def VS = self.MutVShift
    def MF = self.MutMFactory
    def VF = self.MutVFactory
    def pivotA = self.pivotA
  }
  implicit object MDeterminant extends Determinant[M, A] {
    def determinant(m: M) = MLU.lu(m).determinant
  }
  implicit object MutMDeterminant extends Determinant[MutM, A] {
    def determinant(m: MutM) = MutMLU.lu(m).determinant
  }
}
