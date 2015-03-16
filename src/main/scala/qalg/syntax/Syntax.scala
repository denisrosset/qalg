package com.faacets.qalg
package syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import algebra._

trait VecSyntax {
  implicit def vecSyntax[V[_], A](v: V[A])(implicit ev: Vec[V, A]): VecOps[V, A] = new VecOps(v)
}

trait MatSyntax {
  implicit def matSyntax[M[_], V[_], A](m: M[A])(implicit ev: Mat[M, V, A]): MatOps[M, V, A] = new MatOps(m)
}

trait AllSyntax
    extends VecSyntax
    with MatSyntax
