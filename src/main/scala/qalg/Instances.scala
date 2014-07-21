package com.faacets
package qalg

import spire.math.Rational
import net.alasc.{Finite, Index, Permuting, PRepr}

trait QVectorInstances {
  implicit val mutableQVectorIndex = new QTensorBaseIndex[mutable.QVector]
  implicit val mutableQVectorOrder = new QVectorBaseOrder[mutable.QVector]
  implicit val mutableQVectorInnerProductSpace = 
    new QVectorBaseInnerProductSpace[mutable.QVector](mutable.QVector)
  implicit val mutableQVectorKron = new QVectorBaseKron[mutable.QVector](mutable.QVector)
  implicit def mutableQVectorPermutingAction[P <: Permuting[P]] = 
    new QVectorBasePermutingAction[mutable.QVector, P](mutable.QVector)
  implicit def mutableQVectorPReprAction[F <: Finite[F]](implicit prepr: PRepr[F]) = 
    new QVectorBasePReprAction[mutable.QVector, F](mutable.QVector)

  implicit val immutableQVectorIndex = new QTensorBaseIndex[immutable.QVector]
  implicit val immutableQVectorOrder = new QVectorBaseOrder[immutable.QVector]
  implicit val immutableQVectorInnerProductSpace = 
    new QVectorBaseInnerProductSpace[immutable.QVector](immutable.QVector)
  implicit val immutableQVectorKron = new QVectorBaseKron[immutable.QVector](immutable.QVector)
  implicit def immutableQVectorPermutingAction[P <: Permuting[P]] = 
    new QVectorBasePermutingAction[immutable.QVector, P](immutable.QVector)
  implicit def immutableQVectorPReprAction[F <: Finite[F]](implicit prepr: PRepr[F]) = 
    new QVectorBasePReprAction[immutable.QVector, F](immutable.QVector)
}

trait QMatrixInstances {
  implicit val mutableQMatrixIndex = new QTensorBaseIndex[mutable.QMatrix]
  implicit val mutableQMatrixAlgebra = new QMatrixAlgebra[mutable.QMatrix](mutable.QMatrix)
  implicit val mutableQMatrixKron = new QMatrixBaseKron[mutable.QMatrix](mutable.QMatrix)

  implicit val immutableQMatrixIndex = new QTensorBaseIndex[immutable.QMatrix]
  implicit val immutableQMatrixAlgebra = new QMatrixAlgebra[immutable.QMatrix](immutable.QMatrix)
  implicit val immutableQMatrixKron = new QMatrixBaseKron[immutable.QMatrix](immutable.QMatrix)
}

trait AllInstances extends
    QVectorInstances with
    QMatrixInstances
