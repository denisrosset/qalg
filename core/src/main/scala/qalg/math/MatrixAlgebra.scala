package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._

import indup.algebra._

import algebra._

final class MatrixMatRing[@sp(Double, Long) A: ClassTag, IM <: ImmMut](implicit val zeroA: Zero[A], val scalar: Ring[A], val V: VecBuild[Vector[A, IM], A]) extends MatRing[Matrix[A, IM], A] with MatSlice[Matrix[A, IM], Vector[A, IM], A] {
  type Options = Unit
  def defaultOptions: Unit = ()
  type M = Matrix[A, IM]
  def options(m: M): Unit = ()
  def apply(m: M, r: Int, c: Int): A = m.apply(r, c)
  def apply(m: M, i: IntInt): A = m.apply(i._1, i._2)
  override def nRows(m: M): Int = m.nRows
  override def nCols(m: M): Int = m.nCols
  def size0(m: M): Int = m.nRows
  def size1(m: M): Int = m.nCols
  def size(m: M): IntInt = IntInt(size0(m), size1(m))
  def size(m: M, d: Int): Int = if (d == 0) size0(m) else size1(m)
  def offsetHead(m: M): OptOffset = m.offsetHead
  def offsetNext(m: M, offset: Offset): OptOffset = m.offsetNext(offset)
  def offsetValue(m: M, offset: Offset): A = m.offsetValue(offset)
  def offsetX(m: M, offset: Offset): IntInt = m.offsetX(offset)
  def offsetX0(m: M, offset: Offset): Int = m.offsetX0(offset)
  def offsetX1(m: M, offset: Offset): Int = m.offsetX1(offset)
  def feedTo(m: M, builder: MatBuilder[_, A]): Unit = m.feedTo(builder)
  def builder(nRows: Int, nCols: Int, options: Unit): MatBuilder[M, A] = new DenseMatrixBuilder[A, IM](nRows, nCols)(implicitly, implicitly, this)
  def copy(m: M): M = m.copy
  def negate(x: M): M = impl.MatDense.negate(x)(this)
  def plus(x: M, y: M): M = impl.MatDense.plus(x, y)(this)
  override def minus(x: M, y: M): M = impl.MatDense.minus(x, y)(this)
  def timesl(a: A, m: M): M = impl.MatDense.timesl(a, m)(this)
  override def timesr(m: M, a: A): M = impl.MatDense.timesr(m, a)(this)
  def times(x: M, y: M): M = impl.MatDense.times(x, y)(this)
  def t(m: M): M = impl.MatDense.t(m)(this)
}

final class MatrixMatVecProduct[@sp(Double, Long) A, IM <: ImmMut](implicit val M: MatRing[Matrix[A, IM], A], val V: VecRing[Vector[A, IM], A]) extends MatVecProduct[Matrix[A, IM], Vector[A, IM]] {
  type M = Matrix[A, IM]
  type V = Vector[A, IM]
  def timesl2(v: V, m: M): V = impl.MatDense.vecTimesMat(v, m)(M, V)
  def timesr2(m: M, v: V): V = impl.MatDense.matTimesVec(m, v)(M, V)
}

final class MatrixMatField[@sp(Double, Long) A: ClassTag, IM <: ImmMut](implicit val zeroA: Zero[A], val scalar: Field[A], val V: VecBuild[Vector[A, IM], A]) extends MatField[Matrix[A, IM], A] with MatSlice[Matrix[A, IM], Vector[A, IM], A] {
  type Options = Unit
  def defaultOptions: Unit = ()
  type M = Matrix[A, IM]
  def options(m: M): Unit = ()
  def apply(m: M, r: Int, c: Int): A = m.apply(r, c)
  def apply(m: M, i: IntInt): A = m.apply(i._1, i._2)
  override def nRows(m: M): Int = m.nRows
  override def nCols(m: M): Int = m.nCols
  def size0(m: M): Int = m.nRows
  def size1(m: M): Int = m.nCols
  def size(m: M): IntInt = IntInt(size0(m), size1(m))
  def size(m: M, d: Int): Int = if (d == 0) size0(m) else size1(m)
  def offsetHead(m: M): OptOffset = m.offsetHead
  def offsetNext(m: M, offset: Offset): OptOffset = m.offsetNext(offset)
  def offsetValue(m: M, offset: Offset): A = m.offsetValue(offset)
  def offsetX(m: M, offset: Offset): IntInt = m.offsetX(offset)
  def offsetX0(m: M, offset: Offset): Int = m.offsetX0(offset)
  def offsetX1(m: M, offset: Offset): Int = m.offsetX1(offset)
  def feedTo(m: M, builder: MatBuilder[_, A]): Unit = m.feedTo(builder)
  def builder(nRows: Int, nCols: Int, options: Unit): MatBuilder[M, A] = new DenseMatrixBuilder[A, IM](nRows, nCols)(implicitly, implicitly, this)
  def copy(m: M): M = m.copy
  def negate(x: M): M = impl.MatDense.negate(x)(this)
  def plus(x: M, y: M): M = impl.MatDense.plus(x, y)(this)
  override def minus(x: M, y: M): M = impl.MatDense.minus(x, y)(this)
  def timesl(a: A, m: M): M = impl.MatDense.timesl(a, m)(this)
  override def timesr(m: M, a: A): M = impl.MatDense.timesr(m, a)(this)
  def times(x: M, y: M): M = impl.MatDense.times(x, y)(this)
  def t(m: M): M = impl.MatDense.t(m)(this)
  override def divr(m: M, a: A): M = impl.MatDense.divr(m, a)(this)
}

final class MatrixUpdate[@sp(Double, Long) A](implicit val index2: Index2[Matrix[A, Mut], Matrix[A, Mut], Vector[A, Mut], A]) extends Update2[Matrix[A, Mut], A] {
  type M = Matrix[A, Mut]
  def update(m: M, r: Int, c: Int, a: A): Unit = m.update(r, c, a)
  def update(m: M, i: IntInt, a: A): Unit = m.update(i._1, i._2, a)
}
