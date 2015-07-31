package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.cfor._

import indup.algebra._

import algebra._

final class DenseVectorBuilder[@sp(Double, Long) A: ClassTag: Zero, IM <: ImmMut](val size: Int)(implicit V: VecBuild[Vector[A, IM], A]) extends VecBuilder[Vector[A, IM], A] {
  val data = new Array[A](size)
  def add(i: Int, a: A): Unit = { data(i) = a }
  def result(): DenseVector[A, IM] = new DenseVector[A, IM](data)
}
