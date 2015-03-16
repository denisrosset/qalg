package com.faacets.qalg
package algebra

import scala.language.implicitConversions

import scala.collection.immutable.Range

import util._

sealed trait At1 {
  override def toString: String = toArray.mkString("(", ",", ")")
  def length: Int
  def apply(k: Int): Int
  def toArray: Array[Int] = Array.tabulate(length)(apply(_))
}

final case class AtArray1(indices: Array[Int]) extends At1 {
  @inline def length = indices.length
  @inline def apply(k: Int): Int = indices(k)
}

final case class AtSeq1(indices: Seq[Int]) extends At1 {
  @inline def length = indices.size
  @inline def apply(k: Int): Int = indices(k)
}

final case class AtRange1(range: Range) extends At1 {
  @inline def length = range.length
  @inline def apply(k: Int): Int = range(k)
}

object At1 {
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

final case class At2(rows: At1, cols: At1) {
  @inline def nRows: Int = rows.length
  @inline def nCols: Int = cols.length
  @inline def size: IntInt = IntInt(nRows, nCols)
  @inline def apply(r: Int, c: Int): IntInt = IntInt(rows(r), cols(c))
}

object At {
  def apply(at: At1): At1 = at
  def apply(rows: At1, cols: At1): At2 = At2(rows, cols)
}
