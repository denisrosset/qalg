package com.faacets
package alg

import spire.math.Rational
import net.alasc.Index

trait QTensorInstances {
  implicit object QTensorIndex extends Index[Rational, GenQTensor] {
    def indexLength(t: GenQTensor) = t.length
    def indexElement(t: GenQTensor, i: Int) = t(i)
  }
}

trait QVectorInstances {
/*
  implicit def QVectorOrder[V <: alg.QVectorLike[V, _]] = new QVectorOrder[V]
  implicit def QVectorKron[V <: alg.QVectorLike[V, M], M <: alg.QMatrixLike[M, V]](implicit factory: QVectorFactory[V], vs: VectorSpace[V, Rational]) = new QVectorKron[V, M]
  implicit def QVectorVectorSpace[V <: alg.QVectorLike[V, M], M <: alg.QMatrixLike[M, V]](implicit factory: QVectorFactory[V]) = new QVectorVectorSpace[V, M]
  implicit def QVectorPermutingAction[V <: alg.QVectorLike[V, M], M <: alg.QMatrixLike[M, V], P <: Permuting[P]] =
    new QVectorPermutingAction[V, M, P]
  implicit def QVectorPReprAction[V <: alg.QVectorLike[V, M], M <: alg.QMatrixLike[M, V], F <: Finite[F]](implicit prepr: PRepr[F]) =
    new QVectorPReprAction[V, M, F]*/

  implicit val mutableQVectorInnerProductSpace = 
    new QVectorBaseInnerProductSpace[mutable.QVector](mutable.QVector)
  implicit val immutableQVectorInnerProductSpace = 
    new QVectorBaseInnerProductSpace[immutable.QVector](immutable.QVector)
}

trait QMatrixInstances {
  implicit val immutableQMatrixAlgebra = new QMatrixAlgebra[immutable.QMatrix](immutable.QMatrix)
  implicit val mutableQMatrixAlgebra = new QMatrixAlgebra[mutable.QMatrix](mutable.QMatrix)
}

trait AllInstances extends
    QTensorInstances with
    QVectorInstances with
    QMatrixInstances
