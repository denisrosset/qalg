package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

/** LU Decomposition.
  * For an m-by-n matrix A with m >= n, the LU decomposition is an m-by-n
  * unit lower triangular matrix L, an n-by-n upper triangular matrix U,
  * and a permutation vector piv of length m so that A(piv,:) = L*U.
  * If m < n, then L is m-by-m and U is m-by-n.
  * 
  * The LU decompostion with pivoting always exists, even if the matrix is
  * singular, so the constructor will never fail.  The primary use of the
  * LU decomposition is in the solution of square systems of simultaneous
  * linear equations.  This will fail if isSingular returns true.
  */
trait LUDecomposition[M, V, @sp(Double) A] extends Any { self => // not @sp(Long)
  def pivots: Array[Int]
  def permutationCount: Int
  def permutation: M
  def determinant: A
  def lower: M
  def upper: M
  def isSingular: Boolean
  def solveV(b: V): V
  def solveM(b: M): M
  def inverse: M
  def map[M1, V1](fm: M => M1, fmInv: M1 => M, fv: V => V1, fvInv: V1 => V): LUDecomposition[M1, V1, A] = new LUDecomposition[M1, V1, A] {
    def pivots = self.pivots
    def permutationCount = self.permutationCount
    def permutation = fm(self.permutation)
    def determinant = self.determinant
    def lower = fm(self.lower)
    def upper = fm(self.upper)
    def isSingular: Boolean = self.isSingular
    def solveV(b: V1): V1 = fv(self.solveV(fvInv(b)))
    def solveM(b: M1): M1 = fm(self.solveM(fmInv(b)))
    def inverse: M1 = fm(self.inverse)
  }
}
