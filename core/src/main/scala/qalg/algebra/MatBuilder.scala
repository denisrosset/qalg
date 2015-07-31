package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import indup.algebra.IntInt

trait MatBuilder[M, @sp(Double, Long) A] extends Any {
  def size: IntInt
  def size0: Int
  def size1: Int
  def add(r: Int, c: Int, a: A): Unit
  def result(): M
}
