package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

// todo add orthonormalization

trait VGramSchmidt[V, A] extends Any {
  def orthogonalized[V1](v: V, other: V1*)(implicit V1: Vec[V1, A]): V
  def orthogonalBasis(vs: Seq[V]): Seq[V]
  def orthogonalComplement(vs: Seq[V], d: Int): Seq[V]
}

object VGramSchmidt {
  implicit def fromPack[V, @sp(Double, Long) A](implicit pack: PackEuclideanRing.ForV[V, A]): VGramSchmidt[V, A] = pack.VGramSchmidt
}

trait MGramSchmidt[M] extends Any {
  def orthogonalized(m: M): M
}

object MGramSchmidt {
  implicit def fromPack[M, @sp(Double, Long) A](implicit pack: PackEuclideanRing.ForM[M, A]): MGramSchmidt[M] = pack.MGramSchmidt
}

trait MutableMGramSchmidt[M] extends Any with MGramSchmidt[M] {
  def orthogonalize(m: M): Unit
}

object MutableMGramSchmidt {
  implicit def fromPack[M](implicit pack: PackEuclideanRingMutable.ForM[M, _]): MutableMGramSchmidt[M] = pack.MGramSchmidt
}
