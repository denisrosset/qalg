package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._
import util._

trait GramSchmidt {
  def gramSchmidt[M, @sp(Double) A](m: M)(implicit M: MatInField[M, A], MM: MatMutable[M, A]): M = {
    import M.{scalar, eqA}
    val res = m.copy
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
    res
  }

  def normGramSchmidt[M, @sp(Double) A](m: M)(implicit M: MatInField[M, A], MM: MatMutable[M, A], A: NRoot[A]): M = {
    import M.scalar
    val res = gramSchmidt(m)
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
    res
  }

  def euclideanGramSchmidt[M, @sp(Long) A](m: M)(implicit M: MatInRing[M, A], MM: MatMutable[M, A], A: EuclideanRing[A]): M = {
    val res = m.copy
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
    res
  }
}
