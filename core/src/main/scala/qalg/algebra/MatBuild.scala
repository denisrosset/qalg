package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import indup.algebra._

import spire.syntax.cfor._

trait MatBuild[M, @sp(Double, Long) A] extends Any with Mat[M, A] with WithOptions[M] { self =>

  // direct methods

  def builder(accordingTo: M): MatBuilder[M, A] =
    builder(nRows(accordingTo), nCols(accordingTo), options(accordingTo))

  def builder(nRows: Int, nCols: Int, options: Options = defaultOptions): MatBuilder[M, A]

  def from[T](t: T, options: Options = defaultOptions)(implicit T: Index2[T, _, _, A]): M = {
    val b = builder(T.size0(t), T.size1(t), defaultOptions)
    @tailrec def iter(offset: OptOffset): Unit = offset match {
      case Offset(valid) =>
        b.add(T.offsetX0(t, valid), T.offsetX1(t, valid), T.offsetValue(t, valid))
        iter(T.offsetNext(t, valid))
      case _ =>
    }
    iter(T.offsetHead(t))
    b.result()
  }

  def fromCols[V](nRows: Int, options: Options = defaultOptions)(cols: V*)(implicit V: Vec[V, A]): M = {
    val b = builder(nRows, cols.size, options)
    cforRange(0 until cols.size) { c =>
      val col = cols(c)
      require(V.length(col) == nRows)
      cforRange(0 until nRows) { r =>
        b.add(r, c, V.apply(col, r))
      }
    }
    b.result()
  }

  def fromRows[V](nCols: Int, options: Options = defaultOptions)(rows: V*)(implicit V: Vec[V, A]): M = {
    val b = builder(rows.size, nCols, options)
    cforRange(0 until rows.size) { r =>
      val row = rows(r)
      require(V.length(row) == nCols)
      cforRange(0 until nCols) { c =>
        b.add(r, c, V.apply(row, c))
      }
    }
    b.result()
  }

  def build(nRows: Int, nCols: Int, options: Options = defaultOptions)(elements: ((Int, Int), A)*): M = {
    val b = builder(nRows, nCols, options)
    elements.foreach { element =>
      b.add(element._1._1, element._1._2, element._2)
    }
    b.result()
  }

  def rowMajor(nRows: Int, nCols: Int, options: Options = defaultOptions)(elements: A*): M = {
    require(nRows * nCols == elements.size)
    var i0 = 0
    var i1 = 0
    val b = builder(nRows, nCols, options)
    elements.foreach { element =>
      b.add(i0, i1, element)
      i1 += 1
      if (i1 == nCols) {
        i1 = 0
        i0 += 1
      }
    }
    b.result()
  }

  def colMajor(nRows: Int, nCols: Int, options: Options = defaultOptions)(elements: A*): M = {
    require(nRows * nCols == elements.size)
    var i0 = 0
    var i1 = 0
    val b = builder(nRows, nCols, options)
    elements.foreach { element =>
      b.add(i0, i1, element)
      i0 += 1
      if (i0 == nRows) {
        i0 = 0
        i1 += 1
      }
    }
    b.result()
  }

  def tabulate(nRows: Int, nCols: Int, options: Options = defaultOptions)(f: (Int, Int) => A): M = {
    val b = builder(nRows, nCols, options)
    cforRange(0 until nRows) { r =>
      cforRange(0 until nCols) { c =>
        b.add(r, c, f(r, c))
      }
    }
    b.result()
  }

  def fill(nRows: Int, nCols: Int, options: Options = defaultOptions)(a: A): M = {
    val b = builder(nRows, nCols, options)
    cforRange(0 until nRows) { r =>
      cforRange(0 until nCols) { c =>
        b.add(r, c, a)
      }
    }
    b.result()
  }


  // type class methods

  def copy(m: M): M

  def t(m: M): M

  // default implementations

  def apply(m: M, at0: At, at1: At): M =
    if (at0.isInstanceOf[AtAll.type] && at1.isInstanceOf[AtAll.type]) copy(m) else {
      val a0 = at0.forSize(nRows(m))
      val a1 = at1.forSize(nCols(m))
      val b = builder(a0.size, a1.size, options(m))
      cforRange(0 until a0.size) { k0 =>
        val i0 = a0(k0)
        cforRange(0 until a1.size) { k1 =>
          val i1 = a1(k1)
          b.add(k0, k1, apply(m, i0, i1))
        }
      }
      b.result()
    }
}

object MatBuild {
  type WithOptions[M, @sp(Double, Long) A, O] = MatBuild[M, A] { type Options = O }
  def apply[M, @sp(Double, Long) A](implicit M: MatBuild[M, A]): MatBuild[M, A] = M
}
