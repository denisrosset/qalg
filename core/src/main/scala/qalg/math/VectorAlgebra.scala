package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._

import indup.algebra._

import algebra._

final class VectorVecRing[@sp(Double, Long) A: ClassTag, IM <: ImmMut](implicit val zeroA: Zero[A], val scalar: Ring[A]) extends VecRing[Vector[A, IM], A] {
  type Options = Unit
  def defaultOptions: Unit = ()
  type V = Vector[A, IM]
  def options(v: V): Unit = ()
  def apply(v: V, i: Int): A = v.apply(i)
  def size(v: V): Int = v.size
  def offsetHead(v: V): OptOffset = v.offsetHead
  def offsetNext(v: V, offset: Offset): OptOffset = v.offsetNext(offset)
  def offsetValue(v: V, offset: Offset): A = v.offsetValue(offset)
  def offsetX(v: V, offset: Offset): Int = v.offsetX(offset)
  def offsetX0(v: V, offset: Offset): Int = v.offsetX(offset)
  def feedTo(v: V, builder: VecBuilder[_, A]): Unit = v.feedTo(builder)
  def builder(size: Int, options: Unit): VecBuilder[V, A] = new DenseVectorBuilder[A, IM](size)(implicitly, implicitly, this)
  def copy(v: V): V = v.copy
  def negate(x: V): V = impl.VecDense.negate(x)(this)
  def plus(x: V, y: V): V = impl.VecDense.plus(x, y)(this)
  override def minus(x: V, y: V): V = impl.VecDense.minus(x, y)(this)
  def timesl(a: A, v: V): V = impl.VecDense.timesl(a, v)(this)
  override def timesr(v: V, a: A): V = impl.VecDense.timesr(v, a)(this)
}

final class VectorVecField[@sp(Double, Long) A: ClassTag, IM <: ImmMut](implicit val zeroA: Zero[A], val scalar: Field[A]) extends VecField[Vector[A, IM], A] {
  type Options = Unit
  def defaultOptions: Unit = ()
  type V = Vector[A, IM]
  def options(v: V): Unit = ()
  def apply(v: V, i: Int): A = v.apply(i)
  def size(v: V): Int = v.size
  def offsetHead(v: V): OptOffset = v.offsetHead
  def offsetNext(v: V, offset: Offset): OptOffset = v.offsetNext(offset)
  def offsetValue(v: V, offset: Offset): A = v.offsetValue(offset)
  def offsetX(v: V, offset: Offset): Int = v.offsetX(offset)
  def offsetX0(v: V, offset: Offset): Int = v.offsetX(offset)
  def feedTo(v: V, builder: VecBuilder[_, A]): Unit = v.feedTo(builder)
  def builder(size: Int, options: Unit): VecBuilder[V, A] = new DenseVectorBuilder[A, IM](size)(implicitly, implicitly, this)
  def copy(v: V): V = v.copy
  def negate(x: V): V = impl.VecDense.negate(x)(this)
  def plus(x: V, y: V): V = impl.VecDense.plus(x, y)(this)
  override def minus(x: V, y: V): V = impl.VecDense.minus(x, y)(this)
  def timesl(a: A, v: V): V = impl.VecDense.timesl(a, v)(this)
  override def timesr(v: V, a: A): V = impl.VecDense.timesr(v, a)(this)
  override def divr(v: V, a: A): V = impl.VecDense.divr(v, a)(this)
  def dot(x: V, y: V): A = impl.VecDense.dot(x, y)(this)
}

final class VectorUpdate[@sp(Double, Long) A](implicit val index1: Index1[Vector[A, Mut], Vector[A, Mut], A]) extends Update1[Vector[A, Mut], A] {
  type V = Vector[A, Mut]
  def update(v: V, i: Int, a: A): Unit = v.update(i, a)
}
