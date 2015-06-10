package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

// TODO: add orthonormalization

trait VGramSchmidt[V, A] extends Any {
  def orthogonalized[V1](v: V, other: V1*)(implicit V1: Vec[V1, A]): V
  def orthogonalBasis(vs: Seq[V]): Seq[V]
  def orthogonalComplement(vs: Seq[V], d: Int): Seq[V]
}

object VGramSchmidt {
  implicit def fromAlg[V, @sp(Double, Long) A](implicit ev: AlgVE[V, A]): VGramSchmidt[V, A] = ev.VGramSchmidt
}

trait MGramSchmidt[M] extends Any {
  def orthogonalized(m: M): M
}

object MGramSchmidt {
  implicit def fromAlg[M](implicit ev: AlgMVE[M, _, _]): MGramSchmidt[M] = ev.MGramSchmidt
}

trait MutableMGramSchmidt[M] extends Any with MGramSchmidt[M] {
  def orthogonalized(m: M): M
  def orthogonalize(m: M): Unit
}

object MutableMGramSchmidt {
  implicit def fromAlg[M](implicit ev: AlgUMVE[M, _, _]): MutableMGramSchmidt[M] = ev.MGramSchmidt
}

final class GramSchmidtE[M, V, @sp(Long) A](implicit M: MatInRing[M, A], MM: MatMutable[M, A], MF: MatFactory[M], MC: MatCat[M, A], V: VecInRing[V, A], MV: MatSlicer[M, V], A: EuclideanRing[A]) extends MutableMGramSchmidt[M] with VGramSchmidt[V, A] {
 // TODO: remove duplicated code with GramSchmidtF
  def orthogonalized[V1](v: V, other: V1*)(implicit V1: Vec[V1, A]): V = {
    val mat = MatBuilder[M, A].tabulate(other.size + 1, v.length) {
      (r, c) => if (r < other.size) other(r)(c) else v(c)
    }
    orthogonalize(mat)
    mat(mat.nRows - 1, ::)
  }

  def orthogonalBasis(vs: Seq[V]): Seq[V] = {
    implicit def eqA: Eq[A] = M.eqA
    if (vs.isEmpty) return vs
    val mat = MatBuilder[M, A].fromRows(vs.head.length, vs: _*)
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

  def orthogonalComplement(vs: Seq[V], d: Int): Seq[V] = {
    implicit def eqA: Eq[A] = M.eqA
    if (vs.nonEmpty) require (vs.head.length == d)
    val n = vs.size
    val mat = vertcat(MatBuilder[M, A].fromRows(d, vs: _*), eye[M](d))
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
    val res = MM.copy(m)
    orthogonalize(res)
    res
  }

  def orthogonalize(res: M): Unit = {
    val nR = res.nRows
    val nC = res.nCols
    cforRange(0 until nR) { i =>
      cforRange(i + 1 until nR) { j =>
        var uv = A.zero
        var uu = A.zero
        cforRange(0 until nC) { c =>
          uv = uv + res(i, c) * res(j, c)
          uu = uu + res(i, c) * res(i, c)
        }
        var g = A.zero
        cforRange(0 until nC) { c =>
          res(j, c) = uu * res(j, c) - uv * res(i, c)
          g = A.gcd(res(j, c), g)
        }
        cforRange(0 until nC) { c =>
          res(j, c) = res(j, c) /~ g
        }
      }
    }
  }
}

final class MGramSchmidtF[M, V, @sp(Double) A](implicit M: MatInField[M, A], MM: MatMutable[M, A], MF: MatFactory[M], MC: MatCat[M, A], MV: MatSlicer[M, V], V: VecInField[V, A]) extends MutableMGramSchmidt[M] with VGramSchmidt[V, A] {
  implicit def A: Field[A] = M.A
  implicit def eqA: Eq[A] = M.eqA
 // TODO: remove duplicated code with GramSchmidtF

  def orthogonalized[V1](v: V, other: V1*)(implicit V1: Vec[V1, A]): V = {
    val mat = MatBuilder[M, A].tabulate(other.size + 1, v.length) {
      (r, c) => if (r < other.size) other(r)(c) else v(c)
    }
    orthogonalize(mat)
    mat(mat.nRows - 1, ::)
  }

  def orthogonalBasis(vs: Seq[V]): Seq[V] = { // warning duplicated from MGramSchmidtE
    implicit def eqA: Eq[A] = M.eqA
    if (vs.isEmpty) return vs
    val mat = MatBuilder[M, A].fromRows(vs.head.length, vs: _*)
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
    implicit def eqA: Eq[A] = M.eqA
    if (vs.nonEmpty) require (vs.head.length == d)
    val n = vs.size
    val mat = vertcat(MatBuilder[M, A].fromRows(d, vs: _*), eye[M](d))
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
    val res = MM.copy(m)
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

/* TODO: add orthonormalization
trait MGramSchmidtRoot[M, @sp(Double) A] extends Any with MutableMGramSchmidt[M] {
  implicit def sourceGS: MutableMGramSchmidt[M]
  implicit def M: MatInField[M, A]
  implicit def A: Field[A] = M.A
  implicit def eqA: Eq[A] = M.eqA
  implicit def nrootA: NRoot[A]
  implicit def MM: MatMutable[M, A]

  def orthonormalized(m: M): M = {
    val res = MM.copy(m)
    unsafeGramSchmidt(res)
    res
  }

  def orthonormalize(res: M): Unit = {
    sourceGS.unsafeGramSchmidt(res)
    cforRange(0 until res.nRows) { r =>
      var norm2 = Field[A].zero
      cforRange(0 until res.nCols) { c =>
        norm2 = norm2 + res(r, c) * res(r, c)
      }
      val normInv = spire.math.sqrt(norm2).reciprocal
      cforRange(0 until res.nCols) { c =>
        res(r, c) = res(r, c) * normInv
      }
    }
  }
}*/
