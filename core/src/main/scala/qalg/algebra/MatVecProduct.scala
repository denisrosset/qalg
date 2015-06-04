package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.ring._
import spire.syntax.cfor._
import util._

trait MatVecProduct[M, V] extends Any { self =>
  def timesl2(v: V, m: M): V
  def timesr2(m: M, v: V): V
}

trait MatVecProductImpl[M, V, @sp(Double, Long) A] extends Any with MatVecProduct[M, V] { self =>
  implicit def M: Mat[M, A]
  implicit def V: VecInRing[V, A]
  implicit def A: Ring[A]
  def timesl2(v: V, m: M): V = {
    require(M.nRows(m) == V.length(v))
    V.tabulate(M.nCols(m)) { c =>
      var acc = A.zero
      cforRange(0 until M.nRows(m)) { r =>
        acc += V.apply(v, r) * M.apply(m, r, c)
      }
      acc
    }
  }
  def timesr2(m: M, v: V): V = {
    require(M.nCols(m) == V.length(v))
    V.tabulate(M.nRows(m)) { r =>
      var acc = A.zero
      cforRange(0 until M.nCols(m)) { c =>
        acc += M.apply(m, r, c) * V.apply(v, c)
      }
      acc
    }
  }
}

object MatVecProduct {
  def apply[M, V](implicit P: MatVecProduct[M, V]): MatVecProduct[M, V] = P
}
