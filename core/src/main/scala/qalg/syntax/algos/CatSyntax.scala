package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait CatSyntax {
  def cat[V1, V2, @sp(Double, Long) A](first: V1, rest: V2*)(implicit V1: VecCat[V1, A], V2: Vec[V2, A]): V1 = V1.cat(first, rest: _*)
  def vertcat[M1, M2, @sp(Double, Long) A](first: M1, rest: M2*)(implicit M1: MatCat[M1, A], M2: Mat[M2, A]): M1 = M1.vertcat(first, rest: _*)
  def horzcat[M1, M2, @sp(Double, Long) A](first: M1, rest: M2*)(implicit M1: MatCat[M1, A], M2: Mat[M2, A]): M1 = M1.horzcat(first, rest: _*)
}
