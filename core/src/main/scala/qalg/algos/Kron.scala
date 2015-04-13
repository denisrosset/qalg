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

trait KronLin[L, @sp(Double, Long) A] extends Any {
  def kron(x: L, y: L): L
}

trait KronVec[V, @sp(Double, Long) A] extends Any with KronLin[V, A] {
  implicit def V: VecInRing[V, A]
  implicit def A: Ring[A] = V.scalar

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

trait KronMat[M, @sp(Double, Long) A] extends Any with KronLin[M, A] {
  implicit def M: MatInRing[M, A]
  implicit def A: Ring[A] = M.scalar

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

trait KronInstances {
  implicit def KronMat[M, @sp(Double, Long) A](implicit M0: MatInRing[M, A]): KronMat[M, A] = new KronMat[M, A] {
    def M = M0
  }
  implicit def KronVec[V, @sp(Double, Long) A](implicit V0: VecInRing[V, A]): KronVec[V, A] = new KronVec[V, A] {
    def V = V0
  }
}

trait Kron extends KronInstances {
  def kron[L, @sp(Double, Long) A](lins: L*)(implicit L: Lin[L, A], ev: KronLin[L, A]): L =
    lins.tail.foldLeft(lins.head)(ev.kron(_, _))

  def reverseKron[L, @sp(Double, Long) A](lins: L*)(implicit L: Lin[L, A], ev: KronLin[L, A]): L =
    kron[L, A](lins.reverse:_*)
}
