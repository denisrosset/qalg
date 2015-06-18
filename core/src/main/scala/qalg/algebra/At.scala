package com.faacets.qalg
package algebra

import scala.language.implicitConversions

import scala.collection.immutable.Range

import util._

sealed abstract class At1 extends AnyRef {
  def apply(k: Int): Int
  def forSize(size: Int): AtIndices1
  def forVec[V](v: V)(implicit V: Vec[V, _]): AtIndices1
  def forRowsOf[M](m: M)(implicit M: Mat[M, _]): AtIndices1
  def forColsOf[M](m: M)(implicit M: Mat[M, _]): AtIndices1
}

case object AtAll1 extends At1 {
  override def toString: String = "::"
  def forSize(size: Int): AtIndices1 = AtFirst1(size)
  def apply(k: Int): Int = k
  def forVec[V](v: V)(implicit V: Vec[V, _]): AtIndices1 = AtFirst1(V.length(v))
  def forRowsOf[M](m: M)(implicit M: Mat[M, _]): AtIndices1 = AtFirst1(M.nRows(m))
  def forColsOf[M](m: M)(implicit M: Mat[M, _]): AtIndices1 = AtFirst1(M.nCols(m))
}

sealed abstract class AtIndices1 extends At1 { self =>
  override def toString: String = toArray.mkString("(", ",", ")")
  def length: Int
  def toArray: Array[Int] = Array.tabulate(length)(apply(_))
  def forSize(size: Int): AtIndices1 = self
  def forVec[V](v: V)(implicit V: Vec[V, _]): AtIndices1 = self
  def forRowsOf[M](m: M)(implicit M: Mat[M, _]): AtIndices1 = self
  def forColsOf[M](m: M)(implicit M: Mat[M, _]): AtIndices1 = self
}

final case class AtArray1(indices: Array[Int]) extends AtIndices1 {
  def length = indices.length
  def apply(k: Int): Int = indices(k)
}

final case class AtSeq1(indices: Seq[Int]) extends AtIndices1 {
  def length = indices.size
  def apply(k: Int): Int = indices(k)
}

final case class AtRange1(range: Range) extends AtIndices1 {
  def length = range.length
  def apply(k: Int): Int = range(k)
}

final case class AtFirst1(length: Int) extends AtIndices1 {
  def apply(k: Int): Int = k
}

object At1 {
  implicit def at1FromDoubleColon(all: ::.type): At1 = AtAll1
  implicit def at1FromRange(range: Range): At1 = AtRange1(range)
  implicit def at1FromSeq(seq: Seq[Int]): At1 = AtSeq1(seq)
  implicit def at1FromArray(array: Array[Int]): At1 = AtArray1(array)
  implicit def at1FromTuple2(tuple: (Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2))
  implicit def at1FromTuple3(tuple: (Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3))
  implicit def at1FromTuple4(tuple: (Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4))
  implicit def at1FromTuple5(tuple: (Int, Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5))
  implicit def at1FromTuple6(tuple: (Int, Int, Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6))
  implicit def at1FromTuple7(tuple: (Int, Int, Int, Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7))
  implicit def at1FromTuple8(tuple: (Int, Int, Int, Int, Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8))
  implicit def at1FromTuple9(tuple: (Int, Int, Int, Int, Int, Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9))
  implicit def at1FromTuple10(tuple: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)): At1 = AtArray1(Array(tuple._1, tuple._2, tuple._3, tuple._4, tuple._5, tuple._6, tuple._7, tuple._8, tuple._9, tuple._10))
}

object At {
  def apply(at: At1): At1 = at
}
