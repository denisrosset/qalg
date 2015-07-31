package com.faacets.qalg
package syntax.indup

import scala.language.implicitConversions

import indup.algebra._

trait IndexSyntax {
  implicit def sizeSyntax[T, I](t: T)(implicit ev: Size[T, I]): SizeOps[T, I] = new SizeOps[T, I](t
  )
  implicit def indexSyntax[T, I, A](t: T)(implicit ev: Index[T, I, A]): IndexOps[T, I, A] = new IndexOps[T, I, A](t)

  implicit def index1Syntax[T1, R1, A](t: T1)(implicit ev: Index1[T1, R1, A]): Index1Ops[T1, R1, A] = new Index1Ops[T1, R1, A](t)
  implicit def index2Syntax[T2, R2, R1, A](t: T2)(implicit ev: Index2[T2, R2, R1, A]): Index2Ops[T2, R2, R1, A] = new Index2Ops[T2, R2, R1, A](t)

  implicit def sparseNext0Syntax[T](t: T)(implicit ev: SparseNext0[T, _, _]): SparseNext0Ops[T] = new SparseNext0Ops[T](t)
  implicit def sparseNext1Syntax[T](t: T)(implicit ev: SparseNext1[T, _, _]): SparseNext1Ops[T] = new SparseNext1Ops[T](t)
}

trait UpdateSyntax {
  implicit def update1Syntax[T1, A](t: T1)(implicit ev: Update1[T1, A]): Update1Ops[T1, A] = new Update1Ops[T1, A](t)
  implicit def update2Syntax[T2, A](t: T2)(implicit ev: Update2[T2, A]): Update2Ops[T2, A] = new Update2Ops[T2, A](t)
}

trait AllSyntax
    extends IndexSyntax
    with UpdateSyntax
