package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.math._
import spire.syntax.all._

import algebra._
import syntax.all._

/** Implementation taken from the JAMA library (NIST), in the public domain, and
  * translated to Scala.
  */
final class DenseMutableLUImpl[M, V, @sp(Double, Long) A](implicit M: MatField[M, A], V: VecField[V, A], MM: MatMut[M, A], VM: VecMut[V, A], MF: MatFactory[M], VF: VecFactory[V], MS: MutableMatShift[M], VS: MutableVecShift[V], pivotA: Pivot[A], eqA: Eq[A]) extends MutableLU[M, V, A] {
  implicit def A: Field[A] = M.scalar
  
  def lu(m: M): LUDecomposition[M, V, A] = unsafeLU(M.copy(m))

  class LUDecompositionImpl(
    val pivots: Array[Int],
    val permutationCount: Int,
    lu: M) extends LUDecomposition[M, V, A] {
    val m = lu.nRows // row dimension
    val n = lu.nCols // column dimension
    lazy val pivotsInverse = {
      val res = new Array[Int](pivots.length)
      cforRange(0 until pivots.length) { i =>
        res(pivots(i)) = i
      }
      res
    }
    def permutation: M = {
      val p = zeros[M](n, n)
      cforRange(0 until n) { i => p(pivots(i), i) = A.one }
      p
    }
    def lower: M = {
      val l = zeros[M](m, n)
      cforRange(0 until n) { i => l(i, i) = A.one }
      cforRange(0 until m) { i =>
        cforRange(0 until i) { j =>
            l(i, j) = lu(i, j)
        }
      }
      l
    }
    def upper: M = {
      val u = zeros[M](n, n)
      cforRange(0 until n) { i =>
        cforRange(i until n) { j =>
          u(i, j) = lu(i, j)
        }
      }
      u
    }
    def isSingular: Boolean = {
      cforRange(0 until n) { j =>
        if (lu(j, j).isZero) return true
      }
      false
    }
    def determinant: A = {
      if (m != n)
        throw new IllegalArgumentException("Matrix must be square.")
      var d = if ((permutationCount & 1) == 0) A.one else -A.one
      cforRange(0 until n) { j =>
        d *= lu(j, j)
      }
      d
    }
    def solveV(b: V): V = {
      if (b.length != m)
        throw new IllegalArgumentException("Matrix row dimensions must agree.")
      if (isSingular)
        throw new RuntimeException("Matrix is singular.")
      // Copy right hand side with pivoting
      val x = b.permuted(pivots)
      // Solve L*Y = B(piv,:)
      cforRange(0 until n) { k =>
        cforRange(k + 1 until n) { i =>
          x(i) = x(i) - x(k) * lu(i, k)
        }
      }

      // Solve U*X = Y;
      cforRange(n - 1 to 0 by -1) { k =>
        x(k) = x(k) / lu(k, k)
        cforRange(0 until k) { i =>
          x(i) = x(i) - x(k) * lu(i, k)
        }
      }
      x
    }

    def solveM(b: M): M = {
      if (b.nRows != m)
        throw new IllegalArgumentException("Matrix row dimensions must agree.")
      if (isSingular)
        throw new RuntimeException("Matrix is singular.")
      val nx = b.nCols
      // Copy right hand side with pivoting
      val x = b.rowsPermuted(pivots)

      // Solve L*Y = B(piv,:)
      cforRange(0 until n) { k =>
        cforRange(k + 1 until n) { i =>
          cforRange(0 until nx) { j =>
            x(i, j) = x(i, j) - x(k, j) * lu(i, k)
          }
        }
      }

      // Solve U*X = Y;
      cforRange(n - 1 to 0 by -1) { k =>
        cforRange(0 until nx) { j =>
          x(k, j) = x(k, j) / lu(k, k)
        }
        cforRange(0 until k) { i =>
          cforRange(0 until nx) { j =>
            x(i, j) = x(i, j) - x(k, j) * lu(i, k)
          }
        }
      }
      x
    }
    def inverse: M = {
      require(m == n)
      val r = upper
      // Calculate inv(upper)
      cforRange(n - 1 to 0 by -1) { j =>
        r(j, j) = r(j, j).reciprocal
        cforRange(j - 1 to 0 by -1) { i =>
          var sum = -r(i, j) * r(j, j)
          cforRange(j - 1 until i by - 1) { k =>
            sum = sum - r(i, k) * r(k, j)
          }
          r(i, j) = sum / r(i, i)
        }
      }
      // Solve inv(I) * lower = inv(upper)
      cforRange(0 until n) { i =>
        cforRange(n - 2 to 0 by -1) { j =>
          cforRange(j + 1 until n) { k =>
            r(i, j) = r(i, j) - r(i, k) * lu(k, j)
          }
        }
      }
      // Correct pivot permutations.
      r.colsPermuteInverse(pivotsInverse)
      r
    }
  }

  def unsafeLU(lu: M): LUDecomposition[M, V, A] = {
    val m = lu.nRows // row dimension
    val n = lu.nCols // column dimension
    require(m >= n)
    val piv = Array.tabulate(m)(identity) // internal storage of pivot vector
    var pCount = 0 // permutation count
   // Use a "left-looking", dot-product, Crout/Doolittle algorithm.

    // Outer loop.
    val luColj = zeros[V](m)

    cforRange(0 until n) { j =>
      // Make a copy of the j-th column to localize references.
      cforRange(0 until m) { i => luColj(i) = lu(i, j) }

      // Apply previous transformations.
      cforRange(0 until m) { i =>
        // Most of the time is spent in the following dot product.
        val kmax = min(i, j)
        var s = A.zero
        cforRange(0 until kmax) { k =>
          s = s + lu(i, k) * luColj(k)
        }
        val nv = luColj(i) - s
        luColj(i) = nv
        lu(i, j) = nv
      }
      // Find pivot and exchange if necessary.
      var p = j
      var pPriority = pivotA.priority(luColj(j))
      cforRange(j + 1 until m) { i =>
        val iPriority = pivotA.priority(luColj(i))
        if (iPriority > pPriority) {
          p = i
          pPriority = iPriority
        }
      }
      if (p != j) {
        lu.rowsPermute(p, j)
        val t = piv(p)
        piv(p) = piv(j)
        piv(j) = t
        pCount += 1
      }

      // Compute multipliers.
      val diag = lu(j, j)
      if (j < m && !diag.isZero) {
        cforRange(j + 1 until m) { i =>
          lu(i, j) = lu(i, j) / diag
        }
      }
    }
    new LUDecompositionImpl(piv, pCount, lu)
  }
}
