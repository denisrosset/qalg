package com.faacets.qalg
package math

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.NoImplicit
import spire.algebra._
import spire.syntax.cfor._

import indup.algebra._

import algebra._

sealed trait Vector[@sp(Double, Long) A, IM <: ImmMut] { self =>
  implicit def classTagA: ClassTag[A]
  implicit def zeroA: Zero[A]
  override def toString = impl.Print.print(1, size, (r, c) => apply(c).toString)
  //  def sparse: SparseVector[A, IM]
  override def hashCode = impl.VecDense.hash(self)
  override def equals(rhs: Any): Boolean = rhs match {
    case that: Vector[A, _] => impl.VecDense.equal(self, that)
    case _ => false
  }
  def dense: DenseVector[A, IM]

  def apply(i: Int): A
  def update(i: Int, newA: A)(implicit ev: IM =:= Mut): Unit
  def size: Int
  def offsetHead: OptOffset
  def offsetNext(offset: Offset): OptOffset
  def offsetValue(offset: Offset): A
  def offsetX(offset: Offset): Int
  def feedTo(builder: VecBuilder[_, A]): Unit
  def copy: Vector[A, IM]
}

trait VectorLowPriority {
  implicit def VectorVecRing[@sp(Double) A: Ring: Zero: ClassTag, IM <: ImmMut]: VecRing[Vector[A, IM], A] { type Options = Unit } = new VectorVecRing[A, IM] 
}

object Vector extends VectorLowPriority {
  def apply[@sp(Double, Long) A: Zero: ClassTag, IM <: ImmMut](implicit A: Ring[A], ev: NoImplicit[Field[A]]): VecRing[Vector[A, IM], A] = VectorVecRing[A, IM]
  def apply[@sp(Double, Long) A: Zero: ClassTag, IM <: ImmMut](implicit A: Field[A]): VecField[Vector[A, IM], A] = VectorVecField[A, IM]
  implicit def VectorVecField[@sp(Double) A: Field: Zero: ClassTag, IM <: ImmMut]: VecField[Vector[A, IM], A] { type Options = Unit } = new VectorVecField[A, IM]
  implicit def VectorUpdate[@sp(Double, Long) A](implicit V: Index1[Vector[A, Mut], Vector[A, Mut], A]): Update1[Vector[A, Mut], A] = new VectorUpdate[A]
  trait Tag[IM <: ImmMut]
  implicit def Base[@sp(Double, Long) A, IM <: ImmMut]: Base[Vector[A, IM], Tag[IM]] = null
  implicit def Refine[@sp(Double, Long) A, IM <: ImmMut]: Refine[Tag[IM], A, Vector[A, IM]] = null
}

final class DenseVector[@sp(Double, Long) A, IM <: ImmMut](val data: Array[A])(implicit val V: VecBuild[Vector[A, IM], A], val classTagA: ClassTag[A], val zeroA: Zero[A]) extends Vector[A, IM] {
  def dense: DenseVector[A, IM] = this

  def apply(i: Int): A = data(i)
  def update(i: Int, newA: A)(implicit ev: IM =:= Mut): Unit = data(i) = newA
  def size: Int = data.length
  def copy: DenseVector[A, IM] = new DenseVector[A, IM](data.clone)
  def feedTo(builder: VecBuilder[_, A]): Unit = impl.VecDense.feedTo[Vector[A, IM], A](this, builder)
  def offsetHead: OptOffset = if (data.length == 0) NoOffset else Offset(0)
  def offsetNext(offset: Offset): OptOffset = if (offset.o + 1 >= data.length) NoOffset else Offset(offset.o + 1)
  def offsetValue(offset: Offset): A = data(offset.o.toInt)
  def offsetX(offset: Offset): Int = offset.o.toInt
}
