package com.faacets.qalg
package syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import algebra._

trait LinSyntax {
  implicit def linSyntax[LA, A](l: LA)(implicit ev: Lin[LA, A]): LinOps[LA, A] = new LinOps(l)
}

trait VecSyntax extends LinSyntax {
  implicit def vecSyntax[VA, A](v: VA)(implicit ev: Vec[VA, A]): VecOps[VA, A] = new VecOps(v)
  implicit def vecMutableSyntax[VA, A](v: VA)(implicit ev: VecMutable[VA, A]): VecMutableOps[VA, A] = new VecMutableOps(v)
}

trait MatSyntax extends LinSyntax {
  implicit def matSyntax[M, A](m: M)(implicit ev: Mat[M, A]): MatOps[M, A] = new MatOps(m)
  implicit def matBuilderSyntax[M, A](m: M)(implicit ev: MatBuilder[M, A]): MatBuilderOps[M, A] = new MatBuilderOps(m)
  implicit def matInRingSyntax[M, A](m: M)(implicit ev: MatInRing[M, A]): MatInRingOps[M, A] = new MatInRingOps(m)
  implicit def matMutableSyntax[M, A](m: M)(implicit ev: MatMutable[M, A]): MatMutableOps[M, A] = new MatMutableOps(m)
}

trait AllSyntax
    extends LinSyntax
    with VecSyntax
    with MatSyntax
