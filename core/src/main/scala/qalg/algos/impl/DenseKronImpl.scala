package com.faacets
package qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

final class DenseVecKronImpl[V, @sp(Double, Long) A](implicit val V: VecRing[V, A]) extends Kron[V] {
  import V.scalar

  def kron(x: V, y: V): V = {
    val nx = x.length
    val ny = y.length
    var i = 0
    val b = V.builder(nx * ny, V.options(x))
    cforRange(0 until nx) { ix =>
      cforRange(0 until ny) { iy =>
        b.add(i, x(ix) * y(iy))
        i += 1
      }
    }
    b.result()
  }
}

final class DenseMatKronImpl[M, @sp(Double, Long) A](implicit val M: MatRing[M, A]) extends Kron[M] {
  import M.scalar

  def kron(x: M, y: M): M = {
    val nrx = x.nRows
    val ncx = x.nCols
    val nry = y.nRows
    val ncy = y.nCols
    val nR = nrx * nry
    val nC = ncx * ncy
    val b = M.builder(nR, nC, M.options(x))
    var r = 0
    cforRange(0 until nrx) { rx =>
      cforRange(0 until nry) { ry =>
        var c = 0
        cforRange(0 until ncx) { cx =>
          cforRange(0 until ncy) { cy =>
            b.add(r, c, x(rx, cx) * y(ry, cy))
            c += 1
          }
        }
        r += 1
      }
    }
    b.result()
  }
}
