package com.faacets.qalg
package algos

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.all._

import algos._
import syntax.all._

trait Determinant[M, @sp(Double, Long) A] extends Any {
  /** Computes the determinant of the given matrix. */
  def determinant(m: M): A
}

object Determinant {
  implicit def fromPack[M, @sp(Double, Long) A](implicit pack: PackRing.ForM[M, A]): Determinant[M, A] = pack.MDeterminant
}
