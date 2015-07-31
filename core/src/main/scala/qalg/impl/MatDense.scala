package com.faacets.qalg
package impl

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import algebra._
import indup.algebra._

object MatDense {
  val seed = 0xFAACE73
  @inline def hash[M, @sp(Double, Long) A](m: M)(implicit M: Mat[M, A]): Int = {
    import M._
    import scala.util.hashing.MurmurHash3._
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until nRows(m)) { r =>
      cforRange(0 until nCols(m)) { c =>
        val x = apply(m, r, c)
        if (zeroA.canBeNonZero(x)) {
          val h = (r + c * 41) * 41 + x.##
          a += h
          if (h != 0) b *= h
          n += 1

        }
      }
    }
    var h = seed
    h = mix(h, nRows(m))
    h = mix(h, nCols(m))
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  @inline def feedTo[M, @sp(Double, Long) A](m: M, b: MatBuilder[_, A])(implicit M: MatBuild[M, A]): Unit = {
    import M._
    cforRange(0 until nRows(m)) { r =>
      cforRange(0 until nCols(m)) { c =>
        b.add(r, c, apply(m, r, c))
      }
    }
  }

  @inline def plus[M, @sp(Double, Long) A](x: M, y: M)(implicit M: MatRing[M, A]): M = {
    import M._
    val nR = nRows(x)
    require(nR == nRows(y))
    val nC = nCols(x)
    require(nC == nCols(y))
    val b = builder(nR, nC, options(x))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, apply(x, r, c) + apply(y, r, c))
      }
    }
    b.result()
  }

  @inline def minus[M, @sp(Double, Long) A](x: M, y: M)(implicit M: MatRing[M, A]): M = {
    import M._
    val nR = nRows(x)
    require(nR == nRows(y))
    val nC = nCols(x)
    require(nC == nCols(y))
    val b = builder(nR, nC, options(x))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, apply(x, r, c) - apply(y, r, c))
      }
    }
    b.result()
  }

  @inline def negate[M, @sp(Double, Long) A](m: M)(implicit M: MatRing[M, A]): M = {
    import M._
    val nR = nRows(m)
    val nC = nCols(m)
    val b = builder(nR, nC, options(m))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, -apply(m, r, c))
      }
    }
    b.result()
  }

  @inline def timesl[M, @sp(Double, Long) A](a: A, m: M)(implicit M: MatRing[M, A]): M = {
    import M._
    val nR = nRows(m)
    val nC = nCols(m)
    val b = builder(nR, nC, options(m))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, a * apply(m, r, c))
      }
    }
    b.result()
  }

  @inline def timesr[M, @sp(Double, Long) A](m: M, a: A)(implicit M: MatRing[M, A]): M = {
    import M._
    val nR = nRows(m)
    val nC = nCols(m)
    val b = builder(nR, nC, options(m))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, apply(m, r, c) * a)
      }
    }
    b.result()
  }

  @inline def divr[M, @sp(Double, Long) A](m: M, a: A)(implicit M: MatField[M, A]): M = {
    import M._
    val nR = nRows(m)
    val nC = nCols(m)
    val b = builder(nR, nC, options(m))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, apply(m, r, c) / a)
      }
    }
    b.result()
  }

  @inline def times[M, @sp(Double, Long) A](lhs: M, rhs: M)(implicit M: MatRing[M, A]): M = {
    import M._
    val xR = nRows(lhs)
    val yR = nRows(rhs)
    val xC = nCols(lhs)
    val yC = nCols(rhs)
    val nR = xR
    val nC = yC
    val nK = xC
    require(yR == nK)
    val b = builder(nR, nC, options(lhs))
    var r = 0
    while (r < nR) {
      var c = 0
      while (c < nC) {
        var acc = apply(lhs, r, 0) * apply(rhs, 0, c) // x(r)(0)*y(0)(c)
        var k = 1
        while (k < nK) {
          acc += apply(lhs, r, k) * apply(rhs, k, c) // x(r)(k)*y(k)(c)
          k += 1
        }
        b.add(r, c, acc)
        c += 1
      }
      r += 1
    }
    b.result()
  }

  @inline def t[M, @sp(Double, Long) A](m: M)(implicit M: MatRing[M, A]): M = {
    import M._
    val nR = nRows(m)
    val nC = nCols(m)
    val b = builder(nC, nR, options(m))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(c, r, M.apply(m, r, c))
      }
    }
    b.result()
  }


  @inline def vecTimesMat[M, V, @sp(Double, Long) A](lhs: V, rhs: M)(implicit M: MatRing[M, A], V: VecRing[V, A]): V = {
    import M.scalar
    require(M.nRows(rhs) == V.length(lhs))
    val b = V.builder(M.nCols(rhs), V.options(lhs))
    cforRange(0 until M.nCols(rhs)) { c =>
      var acc = V.apply(lhs, 0) * M.apply(rhs, 0, c)
      cforRange(1 until M.nRows(rhs)) { r =>
        acc += V.apply(lhs, r) * M.apply(rhs, r, c)
      }
      b.add(c, acc)
    }
    b.result()
  }

  @inline def matTimesVec[M, V, @sp(Double, Long) A](lhs: M, rhs: V)(implicit M: MatRing[M, A], V: VecRing[V, A]): V = {
    import M.scalar
    require(M.nCols(lhs) == V.length(rhs))
    val b = V.builder(M.nRows(lhs), V.options(rhs))
    cforRange(0 until M.nRows(lhs)) { r =>
      var acc = M.apply(lhs, r, 0) * V.apply(rhs, 0)
      cforRange(1 until M.nCols(lhs)) { c =>
        acc += M.apply(lhs, r, c) * V.apply(rhs, c)
      }
      b.add(r, acc)
    }
    b.result()
  }
}
