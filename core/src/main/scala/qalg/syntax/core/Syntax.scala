package com.faacets.qalg
package syntax.core

import algebra._

/*trait LinSyntax {
  implicit def linSyntax[LA, A](l: LA)(implicit ev: Lin[LA, A]): LinOps[LA, A] = new LinOps(l)
  implicit def linBuilderSyntax[LA, A](l: LA)(implicit ev: LinBuilder[LA, A]): LinBuilderOps[LA, A] = new LinBuilderOps(l)
}

otrait IndexSyntax {
  implicit def index1Syntax[T, A](t: T)(implicit ev: Index1[T, A]): Index1Ops[T, A] = new Index1Ops[T, A](t)
  implicit def index2Syntax[T, A](t: T)(implicit ev: Index2[T, A]): Index2Ops[T, A] = new Index2Ops[T, A](t)
}

trait UpdateSyntax {
  implicit def update1Syntax[T, A](t: T)(implicit ev: Update1[T, A]): Update1Ops[T, A] = new Update1Ops[T, A](t)
  implicit def update2Syntax[T, A](t: T)(implicit ev: Update2[T, A]): Update2Ops[T, A] = new Update2Ops[T, A](t)
}*/

trait VecSyntax {
  implicit def vecSyntax[V, A](v: V)(implicit ev: Vec[V, A]): VecOps[V, A] = new VecOps(v)
  implicit def vecBuildSyntax[V, A](v: V)(implicit ev: VecBuild[V, A]): VecBuildOps[V, A] = new VecBuildOps(v)
}

trait MatSyntax {
  implicit def matVecProductOps[M](m: M): MatVecProductOps[M] = new MatVecProductOps(m)
  implicit def matSyntax[M, A](m: M)(implicit ev: Mat[M, A]): MatOps[M, A] = new MatOps(m)
  implicit def matBuildSyntax[M, A](m: M)(implicit ev: MatBuild[M, A]): MatBuildOps[M, A] = new MatBuildOps(m)
}

/*
trait MatSyntax extends LinSyntax {
  implicit def matSyntax[M, A](m: M)(implicit ev: Mat[M, A]): MatOps[M, A] = new MatOps(m)
  implicit def matBuilderSyntax[M, A](m: M)(implicit ev: MatBuilder[M, A]): MatBuilderOps[M, A] = new MatBuilderOps(m)
  implicit def matInRingSyntax[M, A](m: M)(implicit ev: MatInRing[M, A]): MatInRingOps[M, A] = new MatInRingOps(m)
  implicit def matMutableSyntax[M, A](m: M)(implicit ev: MatMutable[M, A]): MatMutableOps[M, A] = new MatMutableOps(m)
}
 */
