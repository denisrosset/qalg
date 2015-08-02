package com.faacets.qalg
package syntax

import algebra._
import com.faacets.qalg.macros.Ops

final class VecOps[V, A](lhs: V)(implicit ev: Vec[V, A]) {
  def length(): Int = macro Ops.unop[Int]
  def feedTo[W](rhs: VecBuilder[W, A]): Unit = macro Ops.binop[VecBuilder[W, A], Unit]
}

final class VecBuildOps[V, A](lhs: V)(implicit val ev: VecBuild[V, A]) {
  def copy(): V = macro Ops.unop[V]
  def options(): ev.Options = macro Ops.unop[ev.Options]
  def maps[B, W](f: A => B)(implicit ev1: Mapper[A, B, V, W]): W = ev1.map(lhs)(f)
}

final class MatOps[M, A](lhs: M)(implicit ev: Mat[M, A]) {
  def nRows(): Int = macro Ops.unop[Int]
  def nCols(): Int = macro Ops.unop[Int]
}

final class MatBuildOps[M, A](lhs: M)(implicit val ev: MatBuild[M, A]) {
  def copy(): M = macro Ops.unop[M]
  def options(): ev.Options = macro Ops.unop[ev.Options]
}

final class MatVecProductOps[M](lhs: M) {
  def ::*[V](rhs: V)(implicit ev: MatVecProduct[M, V]): V = macro Ops.binopWithEv[V, MatVecProduct[M, V], V]
  def *::[V](lhs: V)(implicit ev: MatVecProduct[M, V]): V = macro Ops.rbinopWithEv[V, MatVecProduct[M, V], V]
}
