package com.faacets.qalg
package std

import algebra._
import impl._

import indup.algebra._

import scala.{specialized => sp}

import scala.reflect.ClassTag

trait ArrayTag

final class ArrayBuilder[@sp(Double, Long) A: ClassTag: Zero](val size: Int) extends VecBuilder[Array[A], A] {
  val array: Array[A] = Array.fill[A](size)(Zero[A].zero)
  def add(i: Int, a: A): Unit = {
    array(i) = a
  }
  def result(): Array[A] = array
}

trait ArrayVecBuild[@sp(Double, Long) A] extends Any
    with VecBuild[Array[A], A] with Dense1[Array[A], Array[A], A] { self =>

  type Options = Unit
  def defaultOptions: Unit = ()
  def options(t: Array[A]): Unit = ()

  implicit def classTagA: ClassTag[A]
  implicit def zeroA: Zero[A]
  implicit def index1: Index1[Array[A], Array[A], A] = self
  def update(t: Array[A], i: Int, a: A): Unit = { t(i) = a }
  def size(t: Array[A]): Int = t.length
  def apply(t: Array[A], i: Int): A = t(i)
  def builder(size: Int, options: Unit): ArrayBuilder[A] = new ArrayBuilder[A](size)
  def copy(v: Array[A]): Array[A] = v.clone
  override def apply(v: Array[A], at: At): Array[A] = (self: VecBuild[Array[A], A]).apply(v, at)
  def feedTo(v: Array[A], b: VecBuilder[_, A]): Unit = VecDense.feedTo(v, b)(self)
}

trait ArrayInstances {
  implicit def ArrayVecBuild[@sp(Double, Long) A: ClassTag: Zero]: VecBuild[Array[A], A] = new ArrayVecBuild[A] {
    def classTagA = implicitly[ClassTag[A]]
    def zeroA = implicitly[Zero[A]]
  }
  implicit def ArrayMapper[@sp(Double, Long) A: ClassTag: Zero, @sp(Double, Long) B: ClassTag: Zero](implicit A: VecBuild[Array[A], A], B: VecBuild[Array[B], B]): Mapper[A, B, Array[A], Array[B]] = new VecDense.MapperImpl[A, B, Array[A], Array[B]](v => B.builder(A.length(v)))
}
