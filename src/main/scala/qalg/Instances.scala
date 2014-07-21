package com.faacets
package qalg

import spire.math.Rational

trait QVectorInstances {
  implicit val mutableQVectorOrder = new QVectorBaseOrder[mutable.QVector]
  implicit val mutableQVectorInnerProductSpace = 
    new QVectorBaseInnerProductSpace[mutable.QVector](mutable.QVector)
  implicit val mutableQVectorKron = new QVectorBaseKron[mutable.QVector](mutable.QVector)

  implicit val immutableQVectorOrder = new QVectorBaseOrder[immutable.QVector]
  implicit val immutableQVectorInnerProductSpace = 
    new QVectorBaseInnerProductSpace[immutable.QVector](immutable.QVector)
  implicit val immutableQVectorKron = new QVectorBaseKron[immutable.QVector](immutable.QVector)
}

trait QMatrixInstances {
  implicit val mutableQMatrixAlgebra = new QMatrixAlgebra[mutable.QMatrix](mutable.QMatrix)
  implicit val mutableQMatrixKron = new QMatrixBaseKron[mutable.QMatrix](mutable.QMatrix)

  implicit val immutableQMatrixAlgebra = new QMatrixAlgebra[immutable.QMatrix](immutable.QMatrix)
  implicit val immutableQMatrixKron = new QMatrixBaseKron[immutable.QMatrix](immutable.QMatrix)
}

trait AllInstances extends
    QVectorInstances with
    QMatrixInstances
