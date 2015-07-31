package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import indup.algebra._

import spire.syntax.cfor._

trait MatSlice[M, V, @sp(Double, Long) A] extends IndexAt2To1[M, M, V, A] with MatBuild[M, A] with IndexAt2[M, M, V, A] {
  implicit val V: VecBuild[V, A]
  def convertOptions(options: Options): V.Options = V.defaultOptions

  def apply(m: M, at0: At, i1: Int): V = {
    val a0 = at0.forSize(nRows(m))
    val b = V.builder(a0.size, convertOptions(options(m)))
    cforRange(0 until a0.size) { k0 =>
      val i0 = a0(k0)
      b.add(k0, apply(m, i0, i1))
    }
    b.result()
  }

  def apply(m: M, i0: Int, at1: At): V = {
    val a1 = at1.forSize(nCols(m))
    val b = V.builder(a1.size, convertOptions(options(m)))
    cforRange(0 until a1.size) { k1 =>
      val i1 = a1(k1)
      b.add(k1, apply(m, i0, i1))
    }
    b.result()
  }
}

object MatSlice {
  def apply[M, V, @sp(Double, Long) A](M: MatSlice[M, V, A]): MatSlice[M, V, A] = M
}
