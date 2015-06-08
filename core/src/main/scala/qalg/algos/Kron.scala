package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.monoid._
import spire.syntax.vectorSpace._
import spire.syntax.cfor._

import algebra._
import syntax.all._

trait Kron[L] extends Any {
  def kron(x: L, y: L): L
}

final class VecKronImpl[V, @sp(Double, Long) A](implicit V: VecInRing[V, A]) extends Kron[V] {
  implicit def A: Ring[A] = V.A

  def kron(x: V, y: V): V = {
    val nx = x.length
    val ny = y.length
    V.tabulate(nx * ny) { k =>
      val kx = k / ny
      val ky = k % ny
      x(kx) * y(ky)
    }
  }
}

final class MatKronImpl[M, @sp(Double, Long) A](implicit M: MatInRing[M, A]) extends Kron[M] {
  implicit def A: Ring[A] = M.A

  def kron(x: M, y: M): M = {
    val nrx = x.nRows
    val ncx = x.nCols
    val nry = y.nRows
    val ncy = y.nCols
    val nR = nrx * nry
    val nC = ncx * ncy
    M.tabulate(nR, nC) { (r, c) =>
      val rx = r / nry
      val cx = c / ncy
      val ry = r % nry
      val cy = c % ncy
      x(rx, cx) * y(ry, cy)
    }
  }
}
