package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

trait LU[M, V, @sp(Double) A] extends Any { // not @sp(Long)
  def lu(m: M): LUDecomposition[M, V, A]
}

object LU {
  implicit def fromPack[M, V, @sp(Double, Long) A](implicit pack: PackField.ForMV[M, V, A]): LU[M, V, A] = pack.MLU
}

trait MutableLU[M, V, @sp(Double) A] extends Any with LU[M, V, A] { // not @sp(Long)
  def unsafeLU(m: M): LUDecomposition[M, V, A]
}

object MutableLU {
  implicit def fromPack[M, V, @sp(Double, Long) A](implicit pack: PackFieldMutable.ForMV[M, V, A]): MutableLU[M, V, A] = pack.MLU
}
