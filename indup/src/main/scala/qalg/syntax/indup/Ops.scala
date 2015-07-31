package com.faacets.qalg
package syntax.indup

import com.faacets.qalg.macros.Ops

import indup.algebra._

final class SizeOps[T, I](lhs: T)(implicit ev: Size[T, I]) {
  def size(): I = macro Ops.unop[I]
  def size(rhs: Int): Int = macro Ops.binop[Int, Int]
}

final class IndexOps[T, I, A](lhs: T)(implicit ev: Index[T, I, A]) {
  def offsetHead(): OptOffset = macro Ops.unop[OptOffset]
  def offsetNext(rhs: Offset): OptOffset = macro Ops.binop[Offset, OptOffset]
  def offsetValue(rhs: Offset): A = macro Ops.binop[Offset, A]
  def offsetX(rhs: Offset): I = macro Ops.binop[Offset, I]
}

final class SparseNext0Ops[T](lhs: T)(implicit ev: SparseNext0[T, _, _]) {
  def offsetNext0(rhs: Offset): OptOffset = macro Ops.binop[Offset, OptOffset]
}

final class SparseNext1Ops[T](lhs: T)(implicit ev: SparseNext1[T, _, _]) {
  def offsetNext1(rhs: Offset): OptOffset = macro Ops.binop[Offset, OptOffset]
}

final class Index1Ops[T1, +R1, A](lhs: T1)(implicit ev: Index1[T1, R1, A]) {
  def apply(rhs: Int): A = macro Ops.binop[Int, A]
  def offsetX0(rhs: Offset): Int = macro Ops.binop[Offset, Int]

  def apply(rhs: At)(implicit ev1: IndexAt1[T1, R1, A]): T1 = macro Ops.binopUsingEv2[At, IndexAt1[T1, R1, A], T1]
  def toIndexedSeq(): IndexedSeq[A] = macro Ops.unop[IndexedSeq[A]]

}

final class Index2Ops[T2, R2, R1, A](lhs: T2)(implicit ev: Index2[T2, R2, R1, A]) {
  def size0(): Int = macro Ops.unop[Int]
  def size1(): Int = macro Ops.unop[Int]
  def offsetX0(rhs: Offset): Int = macro Ops.binop[Offset, Int]
  def offsetX1(rhs: Offset): Int = macro Ops.binop[Offset, Int]

  type IA2 = IndexAt2[T2, R2, R1, A]
  type IA21 = IndexAt2To1[T2, R2, R1, A]
  def apply(rhs1: At, rhs2: At)(implicit ev1: IA2): R2 = macro Ops.triopUsingEv2[At, At, IA2, R2]
  def apply(rhs1: At, rhs2: Int)(implicit ev1: IA21): R1 = macro Ops.triopUsingEv2[At, Int, IA21, R1]
  def apply[T1](rhs1: Int, rhs2: At)(implicit ev1: IA21): R1 = macro Ops.triopUsingEv2[Int, At, IA21, R1]

  def apply(rhs1: ::.type, rhs2: ::.type)(implicit ev1: IA2): R2 = macro Ops.triopUsingEv2[At, At, IA2, R2]
  def apply(rhs1: ::.type, rhs2: At)(implicit ev1: IA2): R2 = macro Ops.triopUsingEv2[At, At, IA2, R2]
  def apply(rhs1: At, rhs2: ::.type)(implicit ev1: IA2): R2 = macro Ops.triopUsingEv2[At, At, IA2, R2]
  def apply[T1](rhs1: ::.type, rhs2: Int)(implicit ev1: IA21): R1 = macro Ops.triopUsingEv2[At, Int, IA21, R1]
  def apply[T1](rhs1: Int, rhs2: ::.type)(implicit ev1: IA21): R1 = macro Ops.triopUsingEv2[Int, At, IA21, R1]
  def apply(rhs1: Int, rhs2: Int): A = macro Ops.triop[Int, Int, A]
}

final class Update1Ops[T1, A](lhs: T1)(implicit ev: Update1[T1, A]) {
  def update(rhs1: Int, rhs2: A): Unit = macro Ops.triop[Int, A, Unit]
  def update(rhs1: ::.type, rhs2: A): Unit = macro Ops.triop[At, A, Unit]
  def update(rhs1: At, rhs2: A): Unit = macro Ops.triop[At, A, Unit]
  def update[U1, R1](rhs1: At, rhs2: U1)(implicit ev1: Index1[U1, R1, A]): Unit = macro Ops.triopWithEv2[At, U1, Index1[U1, R1, A], Unit]
  def update[U1, R1](rhs1: ::.type, rhs2: U1)(implicit ev1: Index1[U1, R1, A]): Unit = macro Ops.triopWithEv2[At, U1, Index1[U1, R1, A], Unit]
}

final class Update2Ops[T2, A](lhs: T2)(implicit ev: Update2[T2, A]) {
  def update(rhs1: Int, rhs2: Int, rhs3: A): Unit = macro Ops.tetraop[Int, Int, A, Unit]
  def update(rhs1: At, rhs2: Int, rhs3: A): Unit = macro Ops.tetraop[At, Int, A, Unit]
  def update(rhs1: Int, rhs2: At, rhs3: A): Unit = macro Ops.tetraop[Int, At, A, Unit]
  def update(rhs1: At, rhs2: At, rhs3: A): Unit = macro Ops.tetraop[At, At, A, Unit]
  def update(rhs1: ::.type, rhs2: Int, rhs3: A): Unit = macro Ops.tetraop[At, Int, A, Unit]
  def update(rhs1: Int, rhs2: ::.type, rhs3: A): Unit = macro Ops.tetraop[Int, At, A, Unit]
  def update(rhs1: ::.type, rhs2: At, rhs3: A): Unit = macro Ops.tetraop[At, At, A, Unit]
  def update(rhs1: At, rhs2: At, rhs3: ::.type): Unit = macro Ops.tetraop[At, At, A, Unit]
  def update(rhs1: ::.type, rhs2: ::.type, rhs3: A): Unit = macro Ops.tetraop[At, At, A, Unit]
  def update[U1, R1](rhs1: At, rhs2: Int, rhs3: U1)(implicit ev1: Index1[U1, R1, A]): Unit = macro Ops.tetraopWithEv2[At, Int, U1, Index1[U1, R1, A], Unit]
  def update[U1, R1](rhs1: Int, rhs2: At, rhs3: U1)(implicit ev1: Index1[U1, R1, A]): Unit = macro Ops.tetraopWithEv2[Int, At, U1, Index1[U1, R1, A], Unit]
  def update[U2, R2, R1](rhs1: At, rhs2: At, rhs3: U2)(implicit ev1: Index2[U2, R2, R1, A]): Unit = macro Ops.tetraopWithEv2[At, At, U2, Index2[U2, R2, R1, A], Unit]
  def update[U1, R1](rhs1: ::.type, rhs2: Int, rhs3: U1)(implicit ev1: Index1[U1, R1, A]): Unit = macro Ops.tetraopWithEv2[At, Int, U1, Index1[U1, R1, A], Unit]
  def update[U1, R1](rhs1: Int, rhs2: ::.type, rhs3: U1)(implicit ev1: Index1[U1, R1, A]): Unit = macro Ops.tetraopWithEv2[Int, At, U1, Index1[U1, R1, A], Unit]
  def update[U2, R2, R1](rhs1: ::.type, rhs2: At, rhs3: U2)(implicit ev1: Index2[U2, R2, R1, A]): Unit = macro Ops.tetraopWithEv2[At, At, U2, Index2[U2, R2, R1, A], Unit]
  def update[U2, R2, R1](rhs1: At, rhs2: ::.type, rhs3: U2)(implicit ev1: Index2[U2, R2, R1, A]): Unit = macro Ops.tetraopWithEv2[At, At, U2, Index2[U2, R2, R1, A], Unit]
  def update[U2, R2, R1](rhs1: ::.type, rhs2: ::.type, rhs3: U2)(implicit ev1: Index2[U2, R2, R1, A]): Unit = macro Ops.tetraopWithEv2[At, At, U2, Index2[U2, R2, R1, A], Unit]
}
