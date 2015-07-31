package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.cfor._

import indup.algebra._

import algebra._

final class DenseMatrixBuilder[@sp(Double, Long) A: ClassTag: Zero, IM <: ImmMut](val nRows: Int, val nCols: Int)(implicit M: MatBuild[Matrix[A, IM], A]) extends MatBuilder[Matrix[A, IM], A] {
  def size0: Int = nRows
  def size1: Int = nCols
  def size: IntInt = IntInt(nRows, nCols)
  val data = new Array[A](nRows * nCols)
  def add(r: Int, c: Int, a: A): Unit = { data(c * nRows + r) = a }
  def result(): DenseMatrix[A, IM] = new DenseMatrix[A, IM](nRows, nCols, data)
}
