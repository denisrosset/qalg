package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.all._

import algebra._
import algos._
import syntax.indup.all._
import syntax.all._
import syntax.algos.all._

/** Matrix-in-ring determinant algorithm from Mahajan and Vinay, see
  * http://cjtcs.cs.uchicago.edu/articles/1997/5/cj97-05.pdf
  * 
  * TODO: implement optimizations present in
  * https://github.com/gap-system/gap/blob/master/lib/matrix.gi
  */
final class DeterminantMahajanVinay[M: ClassTag, @sp(Double, Long) A](implicit val M: MatRing[M, A], MM: MatMut[M, A], MF: MatFactory[M]) extends Determinant[M, A] {
  import M.scalar

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
              next(p)(u,w) = next(p)(u, w) + current(p)(u, v) * a(v, w)
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
            temp(p)(u,v) = scalar.zero
          }
        }
      }
      next = temp
    }
    var tplus = scalar.zero
    var tminus = scalar.zero
    cforRange(0 until n) { v =>
      cforRange(0 to v) { u =>
        tplus += current(1)(u, v) * a(v, u)
        tminus += current(0)(u, v) * a(v, u)
      }
    }
    tplus - tminus
  }
}
