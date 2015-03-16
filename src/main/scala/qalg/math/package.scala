package com.faacets.qalg

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra.Ring

import algebra._

package object math {
  def eye[M[_], V[_], @sp(Double, Long) A](rows: Int, cols: Int)(implicit FunM: FunAlgebra[A], M: BuildableMat[M, V, A], A: Ring[A]): M[A] =
    M.fromMat[FunM, FunV](new FunM[A] {
      def nR = rows
      def nC = cols
      def f(r: Int, c: Int) = if (r == c) A.one else A.zero
    })
  def eye[M[_], V[_], @sp(Double, Long) A](n: Int)(implicit FunM: FunAlgebra[A], M: BuildableMat[M, V, A], A: Ring[A]): M[A] =
    M.fromMat[FunM, FunV](new FunM[A] {
      def nR = n
      def nC = n
      def f(r: Int, c: Int) = if (r == c) A.one else A.zero
    })
}
