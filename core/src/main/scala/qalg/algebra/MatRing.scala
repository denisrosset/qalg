package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.ring._

import indup.algebra._

trait MatRing[M, @sp(Double, Long) A] extends Any with MatBuild[M, A] with Module[M, A] with MultiplicativeSemigroup[M] { self =>
  override implicit def scalar: Ring[A]

  def zero: M = builder(0, 0).result()

  def id: M = fill(1, 1)(scalar.one)
}

object MatRing {
  def apply[M, @sp(Double, Long) A](implicit M: MatRing[M, A]): MatRing[M, A] = M
}
