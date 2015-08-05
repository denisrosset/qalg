package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

final class MGramSchmidtF[M, V, @sp(Double) A](implicit M: MatField[M, A] with MatSlice[M, V, A], MM: MatMut[M, A], MF: MatFactory[M], MC: MatCat[M, A], V: VecField[V, A], eqA: Eq[A]) extends MutableMGramSchmidt[M] with VGramSchmidt[V, A] {
  implicit def A: Field[A] = M.scalar

  def orthogonalized[V1](v: V, other: V1*)(implicit V1: Vec[V1, A]): V = {
    val mat = M.tabulate(other.size + 1, v.length) {
      (r, c) => if (r < other.size) other(r)(c) else v(c)
    }
    orthogonalize(mat)
    mat(mat.nRows - 1, ::)
  }

  def orthogonalBasis(vs: Seq[V]): Seq[V] = { // warning duplicated from MGramSchmidtE
    if (vs.isEmpty) return vs
    val mat = M.fromRows(vs.head.length)(vs: _*)
    orthogonalize(mat)
    val res = Seq.newBuilder[V]
    cforRange(0 until mat.nRows) { r =>
      var rowIsZero = true
      cforRange(0 until mat.nCols) { c =>
        if (!mat(r, c).isZero)
          rowIsZero = false
      }
      if (!rowIsZero)
        res += mat(r, ::)
    }
    res.result
  }

  def orthogonalComplement(vs: Seq[V], d: Int): Seq[V] = { // warning duplicated from MGramSchmidtE
    if (vs.nonEmpty) require (vs.head.length == d)
    val n = vs.size
    val mat = vertcat(M.fromRows(d)(vs: _*), eye[M](d))
    orthogonalize(mat)
    val res = Seq.newBuilder[V]
    cforRange(n until mat.nRows) { r =>
      var rowIsZero = true
      cforRange(0 until mat.nCols) { c =>
        if (!mat(r, c).isZero)
          rowIsZero = false
      }
      if (!rowIsZero)
        res += mat(r, ::)
    }
    res.result
  }

  def orthogonalized(m: M): M = {
    val res = M.copy(m)
    orthogonalize(res)
    res
  }

  def orthogonalize(res: M): Unit = {
    val nR = res.nRows
    val nC = res.nCols
    cforRange(0 until nR) { i =>
      cforRange(i + 1 until nR) { j =>
        var uv = Field[A].zero
        var uu = Field[A].zero
        cforRange(0 until nC) { c =>
          uv = uv + res(i, c) * res(j, c)
          uu = uu + res(i, c) * res(i, c)
        }
        if (!uu.isZero) {
          val factor = uv / uu
          cforRange(0 until nC) { c =>
            res(j, c) = res(j, c) - factor * res(i, c)
          }
        }
      }
    }
  }
}
