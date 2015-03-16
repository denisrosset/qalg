package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import util._

/*trait VecBuilder[@sp(Double, Long) A, V] {
  def fill(length: Int)(a: A): V
  def tabulate(length: Int)(f: Int => A): V
  def from[V1](v1: V1)(implicit V1: Vec[A, V1]): V
}*/

trait Vec[V[_], @sp(Double, Long) A] { self =>
  def length(v: V[A]): Int
  def apply(v: V[A], k: Int): A
}

trait BuildableVec[V[_], @sp(Double, Long) A] extends Vec[V, A] { self =>
  implicit def AdditiveMonoidA: AdditiveMonoid[A]
  implicit def EqA: Eq[A]
  implicit def FunVA: Vec[FunV, A]

  def fromVec[V1[_]](v1: V1[A])(implicit V1: Vec[V1, A]): V[A]
  def apply(v: V[A], at: At1): V[A] = fromVec(new FunV[A] {
      def len: Int = at.length
      def f(k: Int): A = self.apply(v, at(k))
  })
}

trait Mat[M[_], V[_], @sp(Double, Long) A] {
  def size(m: M[A]): IntInt = IntInt(nRows(m), nCols(m))
  def nRows(m: M[A]): Int
  def nCols(m: M[A]): Int
  def apply(m: M[A], r: Int, c: Int): A
}

trait BuildableMat[M[_], V[_], @sp(Double, Long) A] extends Mat[M, V, A] { self =>
  implicit def AdditiveMonoidA: AdditiveMonoid[A]
  implicit def EqA: Eq[A]
  implicit def FunMA: Mat[FunM, FunV, A]

  def fromMat[M1[_], V1[_]](m1: M1[A])(implicit M1: Mat[M1, V1, A]): M[A]

  def apply(m: M[A], rows: At1, cols: At1): M[A] = fromMat(new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, rows(r), cols(c))
  })
  def apply(m: M[A], rows: ::.type, cols: ::.type): M[A] = m
  def apply(m: M[A], rows: ::.type, cols: At1): M[A] = fromMat(new FunM[A] {
    def nR: Int = self.nRows(m)
    def nC: Int = cols.length
    def f(r: Int, c: Int): A = self.apply(m, r, cols(c))
  })
  def apply(m: M[A], rows: At1, cols: ::.type): M[A] = fromMat(new FunM[A] {
    def nR: Int = rows.length
    def nC: Int = self.nCols(m)
    def f(r: Int, c: Int): A = self.apply(m, rows(r), c)
  })
}

trait BuildableMatVec[M[_], V[_], @sp(Double, Long) A] extends BuildableMat[M, V, A] with BuildableVec[V, A] {
  self =>
  def apply(m: M[A], rows: At1, c: Int): V[A] = fromVec(new FunV[A] {
    def len: Int = rows.length
    def f(r: Int): A = self.apply(m, rows(r), c)
  })
  def apply(m: M[A], rows: ::.type, c: Int): V[A] = fromVec(new FunV[A] {
    def len: Int = self.nRows(m)
    def f(r: Int): A = self.apply(m, r, c)
  })
  def apply(m: M[A], r: Int, cols: At1): V[A] = fromVec(new FunV[A] {
    def len: Int = cols.length
    def f(c: Int): A = self.apply(m, r, cols(c))
  })
  def apply(m: M[A], r: Int, cols: ::.type): V[A] = fromVec(new FunV[A] {
    def len: Int = self.nCols(m)
    def f(c: Int): A = self.apply(m, r, c)
  })
  def rowMat(v: V[A]): M[A] = fromMat(new FunM[A] {
    def nR: Int = 1
    def nC: Int = self.length(v)
    def f(r: Int, c: Int): A = self.apply(v, c)
  })
  def colMat(v: V[A]): M[A] = fromMat(new FunM[A] {
    def nR: Int = self.length(v)
    def nC: Int = 1
    def f(r: Int, c: Int): A = self.apply(v, r)
  })
}
