package com.faacets.qalg
package syntax

import scala.language.higherKinds
import scala.language.implicitConversions

import algebra._

trait VecSyntax {
  implicit def vecSyntax[VA, A](v: VA)(implicit ev: Vec[VA, A]): VecOps[VA, A] = new VecOps(v)
  implicit def vecMutableSyntax[VA, A](v: VA)(implicit ev: VecMutable[VA, A]): VecMutableOps[VA, A] = new VecMutableOps(v)
}

trait MatSyntax {
  implicit def matSyntax[MA, A](m: MA)(implicit ev: Mat[MA, A]): MatOps[MA, A] = new MatOps(m)
  implicit def matBuilderSyntax[MA, A](m: MA)(implicit ev: MatBuilder[MA, A]): MatBuilderOps[MA, A] = new MatBuilderOps(m)
  implicit def matVecSyntax[MA, A](m: MA)(implicit ev: Mat[MA, A]): MatVecOps[MA, A] = new MatVecOps(m)
  implicit def matMutableSyntax[MA, A](m: MA)(implicit ev: MatMutable[MA, A]): MatMutableOps[MA, A] = new MatMutableOps(m)
  implicit def matAlgSyntax[MA, A](m: MA)(implicit ev: MatAlg[MA, A]): MatAlgOps[MA, A] = new MatAlgOps[MA, A](m)
}

trait AllSyntax
    extends VecSyntax
    with MatSyntax
