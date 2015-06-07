package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.all._

import algos._
import syntax.all._

trait Determinant[M, @sp(Double, Long) A] extends Any {
  /** Computes the determinant of the given matrix. */
  def determinant(m: M): A
}

trait DeterminantMutableRingImpl[M, @sp(Double, Long) A] extends Any with Determinant[M, A] {
  implicit def M: MatInRing[M, A]
  implicit def MF: MatFactory[M]
  implicit def MM: MatMutable[M, A]
  implicit def A: Ring[A] = M.A
  implicit def classTagM: ClassTag[M]
  def determinant(a: M): A = {
    val n = a.nRows
    require(a.nCols == n)
    var current = new Array[M](2)
    val b = n % 2
    current(b) = eye[M](n)
    current(1 - b) = zeros[M](n, n)
    var next = Array(zeros[M](n, n), zeros[M](n, n))
    cforRange(0 to n - 2) { i =>
      cforRange(0 until n) { v =>
        cforRange(0 to v) { u =>
          cforRange(0 to 1) { p =>
            cforRange(u + 1 until n) { w =>
              next(p)(u,w) = next(p)(u,w) + current(p)(u,v) * a(v,w)
              next(1-p)(w,w) = next(1-p)(w,w) + current(p)(u,v) * a(v,u)
            }
          }
        }
      }
      val temp = current
      current = next
      cforRange(0 until n) { v =>
        cforRange(0 until n) { u =>
          cforRange(0 to 1) { p =>
            temp(p)(u,v) = A.zero
          }
        }
      }
      next = temp
    }
    var tplus = A.zero
    var tminus = A.zero
    cforRange(0 until n) { v =>
      cforRange(0 to v) { u =>
        tplus += current(1)(u, v) * a(v, u)
        tminus += current(0)(u, v) * a(v, u)
      }
    }
    tplus - tminus
  }
}
