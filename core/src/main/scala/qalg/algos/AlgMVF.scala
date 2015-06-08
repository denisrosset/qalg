package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._

trait AlgMVF[M, V, @sp(Double, Long) A] extends Any with AlgMVE[M, V, A] with PackMVF[M, V, A] {
  implicit def MRref: Rref[M]
  implicit def MLU: LU[M, V, A]
}

trait AlgUMVF[M, V, @sp(Double, Long) A] extends Any with AlgUMVE[M, V, A] with AlgMVF[M, V, A] with PackMVF[M, V, A] {
  implicit def MRref: MutableRref[M]
  implicit def MLU: MutableLU[M, V, A]
}

trait AlgUMVFImpl[M, V, @sp(Double, Long) A] extends AlgMVF[M, V, A] with AlgUMVRImpl[M, V, A] { self =>
  implicit def pivotA: Pivot[A]

  object MGramSchmidt extends GramSchmidtNoRoot[M, A] {
    def M = self.M
    def MM = self.UM
  }

  object MRref extends MutableRrefImpl[M, A] {
    def M = self.M
    def MM = self.UM
    def pivotA = self.pivotA
  }

  object MLU extends MutableLUImpl[M, V, A] {
    def M = self.M
    def MM = self.UM
    def V = self.V
    def VM = self.UV
    def MS = self.MShift
    def VS = self.VShift
    def MF = self.MFactory
    def VF = self.VFactory
    def pivotA = self.pivotA
  }
  override lazy val MDeterminant = new Determinant[M, A] {
    def determinant(m: M) = MLU.lu(m).determinant
  }
}

/*
  implicit object MLU extends LU[M, V, A] {
    def lu(m: M): LUDecomposition[M, V, A] =
      MutMLU.unsafeLU(toMutM(m)).map(unsafeFromMutM, unsafeToMutM, unsafeFromMutV, unsafeToMutV)
  }

//  implicit object MDeterminant extends Determinant[M, A] {
//    def determinant(m: M) = MLU.lu(m).determinant
//  }

  implicit object MRref extends Rref[M] {
    def rref(m: M): RrefDecomposition[M] =
      MutMRref.unsafeRref(toMutM(m)).map(unsafeFromMutM)
  }


  implicit object MGramSchmidt extends GramSchmidt[M] {
    def gramSchmidt(m: M): M = {
      val res = toMutM(m)
      MutMGramSchmidt.unsafeGramSchmidt(res)
      unsafeFromMutM(res)
    }
  }
 */
