package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._
import util._

trait MatCat[M1, @sp(Double, Long) A] extends Any {
  def vertcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1
  def horzcat[M2](first: M1, rest: M2*)(implicit M2: Mat[M2, A]): M1
}

object MatCat {
  implicit def fromPack[M, @sp(Double, Long) A](implicit pack: PackRing.ForM[M, A]): MatCat[M, A] = pack.MCat
}
