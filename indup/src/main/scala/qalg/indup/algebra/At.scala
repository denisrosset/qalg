package com.faacets.qalg.indup
package algebra

import scala.language.implicitConversions

import scala.collection.immutable.Range

import util._

sealed abstract class At extends AnyRef {
  def apply(k: Int): Int
  def forSize(size: Int): AtSized
  def sizeOrElse(n: Int): Int
}

case object AtAll extends At {
  override def toString: String = "::"
  def forSize(size: Int): AtSized = AtFirst(size)
  def sizeOrElse(size: Int): Int = size
  def apply(k: Int): Int = k
}

sealed abstract class AtSized extends At { self =>
  override def toString: String = toArray.mkString("(", ",", ")")
  def size: Int
  def sizeOrElse(n: Int): Int = size
  def toArray: Array[Int] = Array.tabulate(size)(apply(_))
  def forSize(size: Int): AtSized = self
}

final case class AtArray(indices: Array[Int]) extends AtSized {
  def size = indices.size
  def apply(k: Int): Int = indices(k)
}

final case class AtSeq(indices: Seq[Int]) extends AtSized {
  def size = indices.size
  def apply(k: Int): Int = indices(k)
}

final case class AtFirst(size: Int) extends AtSized {
  def apply(k: Int): Int = k
}

object At {
  implicit def atFromSeq(seq: Seq[Int]): At = AtSeq(seq)
  implicit def atFromDoubleColon(all: ::.type): At = AtAll
  def apply(array: Array[Int]): AtSized = AtArray(array)
  def apply(indices: Int*): AtSized = AtSeq(indices)
}
