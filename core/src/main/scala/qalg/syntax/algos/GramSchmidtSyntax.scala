package com.faacets
package qalg
package syntax
package algos

import scala.{specialized => sp}

import algebra._
import qalg.algos._

trait GramSchmidtSyntax {
  implicit class MGramSchmidtOps[M](m: M)(implicit M: MGramSchmidt[M]) {
    def orthogonalized: M = M.orthogonalized(m)
  }
  implicit class VGramSchmidtOps[V, @sp(Double, Long) A](v: V)(implicit V: VGramSchmidt[V, A]) {
    def orthogonalized[V1](other: V1*)(implicit V1: Vec[V1, A]): V = V.orthogonalized(v, other: _*)
  }
  implicit class SeqVGramSchmidtOps[V](vs: Seq[V])(implicit V: VGramSchmidt[V, _]) {
    def orthogonalBasis: Seq[V] = V.orthogonalBasis(vs)
    def orthogonalComplement(d: Int): Seq[V] = V.orthogonalComplement(vs, d)
  }
  implicit class MutableMGramSchmidtOps[M](m: M)(implicit M: MutableMGramSchmidt[M]) {
    def orthogonalize: Unit = M.orthogonalize(m)
  }
}
