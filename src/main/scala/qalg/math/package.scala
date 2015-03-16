package com.faacets.qalg

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._

import algebra._

package object math {
  def zeros[M[A], @sp(Double, Long) A](rows: Int, cols: Int)(implicit M: MatBuilder[M[A], A], A: AdditiveMonoid[A]): M[A] = M.from(new FunM[A] {
    def nR = rows
    def nC = cols
    def f(r: Int, c: Int) = A.zero
  })
  def ones[M[_], @sp(Double, Long) A](rows: Int, cols: Int)(implicit M: MatBuilder[M[A], A], A: MultiplicativeMonoid[A]): M[A] = M.from(new FunM[A] {
    def nR = rows
    def nC = cols
    def f(r: Int, c: Int) = A.one
  })
  def eye[M[_], @sp(Double, Long) A](rows: Int, cols: Int)(implicit M: MatBuilder[M[A], A], A: Ring[A]): M[A] =
    M.from(new FunM[A] {
      def nR = rows
      def nC = cols
      def f(r: Int, c: Int) = if (r == c) A.one else A.zero
    })
  def eye[M[_], @sp(Double, Long) A](n: Int)(implicit M: MatBuilder[M[A], A], A: Ring[A]): M[A] =
    M.from(new FunM[A] {
      def nR = n
      def nC = n
      def f(r: Int, c: Int) = if (r == c) A.one else A.zero
    })
  /*
   def zeros[V[_], @sp(Double, Long) A](length: Int)(implicit V: VecBuilder[V, A], A: AdditiveMonoid[A]): V[A] = {
    import V.FunVA
    V.fromVec[FunV](new FunV[A] {
      def len = length
      def f(k: Int): A = A.zero
    })
  }
  def ones[V[_], @sp(Double, Long) A](length: Int)(implicit V: VecBuilder[V, A], A: MultiplicativeMonoid[A]): V[A] = {
    import V.FunVA
    V.fromVec[FunV](new FunV[A] {
      def len = length
      def f(k: Int): A = A.one
    })
  }*/
}
