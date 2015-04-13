package com.faacets.qalg
package algos

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

trait LU {
  case class LUDecomposition[M, @sp(Double, Long) A](compact: M, order: Array[Int], sign: Int)(implicit val M: MatInField[M, A], eqA: Eq[A]) {
    implicit def A: Field[A] = M.scalar
    def matP: M = M.tabulate(compact.nRows, compact.nCols) { (r, c) =>
      if (r == order(c)) A.one else A.zero
    }
    def matU: M = M.tabulate(compact.nRows, compact.nCols) { (r, c) =>
      if (r < c) compact(r, c) // upper triangular part
      else if (r == c) A.one // diagonal elements are = 1 by definition
      else A.zero
    }
    def matL: M = M.tabulate(compact.nRows, compact.nCols) { (r, c) =>
      if (r >= c) compact(r, c) // lower triangular part
      else A.zero
    }
    def original: M = matP * matL * matU
    def isSingular: Boolean = {
      cforRange(0 until compact.nRows) { i =>
        if (compact(i, i).isZero)
          return true
      }
      false
    }
    def solve[V](b: V)(implicit MV: MatVecInField[M, V, A], VM: VecMutable[V, A]): V = {
      require(!isSingular)
      import MV.V
      var n = compact.nRows
      // rearrange the elements of the b vector, hold them into x
      val x = V.tabulate(n)(k => b(order(k)))
      // do forward substitution, replacing x vector
      x(0) = x(0) / compact(0,0)
      cforRange(1 until n) { i =>
        var sum: A = A.zero
        cforRange(0 until i) { j =>
          sum = sum + compact(i,j) * x(j)
        }
        x(i) = (x(i) - sum)/compact(i,i)
      }
      // now get the solution vector, x(n-1) is already done
      cforRange(n - 2 to 0 by -1) { i =>
        var sum: A = A.zero
        cforRange(i + 1 until n) { j =>
          sum = sum + compact(i,j) * x(j)
        }
        x(i) = x(i) - sum
      }
      x
    }
  }

  // assumes non-singular square matrix
  def lu[M, @sp(Double, Long) A](m: M)(implicit M: MatInField[M, A], MM: MatMutable[M, A], orderA: Order[A], signedA: Signed[A]): LUDecomposition[M, A] = {
    implicit def fieldA: Field[A] = M.scalar
    val n = m.nRows
    require(n == m.nCols)
    var sign = 1 // changes sign with each row interchange
    val order: Array[Int] = Array.tabulate(n)(i => i) // establish initial ordering in order vector
    val a: M = m.copy
     /* Find pivot element
     * 
     * The function pivot finds the largest element for a pivot in "jcol"
     * of Matrix "a", performs interchanges of the appropriate
     * rows in "a", and also interchanges the corresponding elements in
     * the order vector.
     *
     * using    a      - n by n Matrix of coefficients
     * using    order  - integer vector to hold row ordering
     * @param   jcol   - column of "a" being searched for pivot element
     */
    def pivot(jcol: Int): Boolean = {
      var ipvt = jcol
      var big = a(ipvt, ipvt).abs
      // Find biggest element on or below diagonal. This will be the pivot row.
      cforRange(ipvt + 1 until n) { i =>
        val anext = a(i, jcol).abs
        if (anext > big) {
          big = anext
          ipvt = i
        }
      }
      if (big.isZero)
        throw new IllegalArgumentException("LU decomposition is implemented for now only for non-singular matrices.")
      // Interchange pivot row (ipvt) with current row (jcol).
      if (ipvt == jcol) false
      else {
        cforRange(0 until n) { c =>
          val el = a(jcol, c)
          a(jcol, c) = a(ipvt, c)
          a(ipvt, c) = el
        }
        val tmp = order(jcol)
        order(jcol) = order(ipvt)
        order(ipvt) = tmp
        true
      }
    }

    /* do pivoting for first column and check for singularity */
    if (pivot(0)) sign = -sign
    val diag0 = fieldA.one/a(0,0)
    cforRange(1 until n) { i =>
      a(0,i) = a(0,i) * diag0
    }

    //  Now complete the computing of L and U elements.
    //  The general plan is to compute a column of L's, then
    //  call pivot to interchange rows, and then compute
    //  a row of U's.

    var nm1 = n - 1

    cforRange(1 until nm1) { j =>
      /* column of L's */
      cforRange(j until n) { i =>
	var sum = fieldA.zero
        cforRange(0 until j) { k =>
          sum += a(i,k) * a(k,j)
        }
        a(i,j) = a(i,j) - sum
      }
      /* pivot, and check for singularity */
      if (pivot(j)) sign = -sign
      /* row of U's */
      val diag = fieldA.one/a(j,j)
      cforRange (j + 1 until n) { k =>
        var sum = fieldA.zero
        cforRange(0 until j) { i =>
          sum += a(j,i) * a(i,k)
        }
        a(j,k) = (a(j,k) - sum) * diag
      }
    }

    /* still need to get last element in L Matrix */

    var suml = fieldA.zero
    cforRange(0 until nm1) { k =>
      suml += a(nm1,k) * a(k,nm1)
    }
    a(nm1,nm1) = a(nm1, nm1) - suml
    LUDecomposition(a, order, sign)
  }
}
