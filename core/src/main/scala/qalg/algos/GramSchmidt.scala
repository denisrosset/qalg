package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

trait GramSchmidt[M] extends Any {
  def gramSchmidt(m: M): M
}

trait MutableGramSchmidt[M] extends Any with GramSchmidt[M] {
  def gramSchmidt(m: M): M
  def unsafeGramSchmidt(m: M): Unit
}

trait GramSchmidtNoRoot[M, @sp(Double) A] extends Any with MutableGramSchmidt[M] {
  implicit def M: MatInField[M, A]
  implicit def MM: MatMutable[M, A]
  implicit def A: Field[A] = M.A
  implicit def eqA: Eq[A] = M.eqA

  def gramSchmidt(m: M): M = {
    val res = MM.copy(m)
    unsafeGramSchmidt(res)
    res
  }

  def unsafeGramSchmidt(res: M): Unit = {
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

trait GramSchmidtRoot[M, @sp(Double) A] extends Any with MutableGramSchmidt[M] {
  implicit def sourceGS: MutableGramSchmidt[M]
  implicit def M: MatInField[M, A]
  implicit def A: Field[A] = M.A
  implicit def eqA: Eq[A] = M.eqA
  implicit def nrootA: NRoot[A]
  implicit def MM: MatMutable[M, A]

  def gramSchmidt(m: M): M = {
    val res = MM.copy(m)
    unsafeGramSchmidt(res)
    res
  }

  def unsafeGramSchmidt(res: M): Unit = {
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
}

trait GramSchmidtEuclidean[M, @sp(Long) A] extends Any with MutableGramSchmidt[M] {
  implicit def M: MatInRing[M, A]
  implicit def A: EuclideanRing[A]
  implicit def MM: MatMutable[M, A]

  def gramSchmidt(m: M): M = {
    val res = MM.copy(m)
    unsafeGramSchmidt(res)
    res
  }

  def unsafeGramSchmidt(res: M): Unit = {
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
