package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import spire.algebra._
import spire.syntax.eq._
import spire.syntax.cfor._
import util._

trait Vec[V, @sp(Double, Long) A] extends Any with Lin[V, A] { self =>

  def sameShape(x: V, y: V): Boolean = length(x) == length(y)
  def linearLength(v: V): Int = length(v)
  def linearApply(v: V, k: Int): A = apply(v, k)

  def length(v: V): Int
  def apply(v: V, k: Int): A
  def toIndexedSeq(v: V): IndexedSeq[A] = new IndexedSeq[A] {
    def length: Int = self.length(v)
    def apply(k: Int): A = self.apply(v, k)
  }
  def rowMat[M](v: V)(implicit M: MatBuilder[M, A]): M = M.from(new FunM[A] {
    def nR: Int = 1
    def nC: Int = self.length(v)
    def f(r: Int, c: Int): A = self.apply(v, c)
  })
  def colMat[M](v: V)(implicit M: MatBuilder[M, A]): M = M.from(new FunM[A] {
    def nR: Int = self.length(v)
    def nC: Int = 1
    def f(r: Int, c: Int): A = self.apply(v, r)
  })
  def view(v: V, at: At1): FunV[A] = new FunV[A] {
    def len: Int = at.length
    def f(k: Int): A = self.apply(v, at(k))
  }
  def view(v: V, at: ::.type): FunV[A] = new FunV[A] {
    def len: Int = self.length(v)
    def f(k: Int): A = self.apply(v, k)
  }
}

object Vec {
  implicit def fromMatVec[M, V, @sp(Double, Long) A](implicit MV: MatVec[M, V, A]): Vec[V, A] = MV.V
}
