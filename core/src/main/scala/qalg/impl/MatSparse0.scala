package com.faacets.qalg
package impl

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import algebra._
import indup.algebra._

object MatSparse0 {/*
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
    val b = builder(nR, nC, storageSize(x))
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
    val b = builder(nR, nC, storageSize(x))
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
    val b = builder(nR, nC, storageSize(m))
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
    val b = builder(nR, nC, storageSize(m))
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
    val b = builder(nR, nC, storageSize(m))
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
    val b = builder(nR, nC, storageSize(m))
    cforRange(0 until nR) { r =>
      cforRange(0 until nC) { c =>
        b.add(r, c, apply(m, r, c) / a)
      }
    }
    b.result()
  }*/
}
