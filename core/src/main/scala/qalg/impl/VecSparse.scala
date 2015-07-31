package com.faacets.qalg
package impl

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import algebra._
import indup.algebra._

object VecSparse {
  @inline def feedTo[V, @sp(Double, Long) A](v: V, b: VecBuilder[_, A])(implicit V: VecBuild[V, A]): Unit = {
    import V._
    @tailrec def iter(offset: OptOffset): Unit = offset match {
      case Offset(valid) =>
        b.add(offsetX(v, valid), offsetValue(v, valid))
        iter(offsetNext(v, valid))
      case _ =>
    }
    iter(offsetHead(v))
  }

  @inline def plus[V, @sp(Double, Long) A](x: V, y: V)(implicit V: VecRing[V, A] with SparseNext0[V, Int, A]): V = {
    import V._
    val d = length(x)
    require(d == length(y))
    val b = builder(d, options(x))
    @tailrec def iter(xOff: OptOffset, yOff: OptOffset): Unit = xOff match {
      case Offset(xVal) =>
        val ix = offsetX0(x, xVal)
        yOff match {
          case Offset(yVal) =>
            val iy = offsetX0(y, yVal)
            if (ix < iy) {
              b.add(ix, offsetValue(x, xVal))
              iter(offsetNext0(x, xVal), yOff)
            } else if (ix == iy) {
              val a = offsetValue(x, xVal) + offsetValue(y, yVal)
              if (zeroA.canBeNonZero(a))
                b.add(ix, a)
              iter(offsetNext0(x, xVal), offsetNext0(y, yVal))
            } else {
              b.add(iy, offsetValue(y, yVal))
              iter(xOff, offsetNext0(y, yVal))
            }
          case _ =>
            b.add(ix, offsetValue(x, xVal))
            iter(offsetNext0(x, xVal), yOff)
        }
      case _ =>
        y match {
          case Offset(yVal) =>
            val iy = offsetX0(y, yVal)
            b.add(iy, offsetValue(y, yVal))
            iter(xOff, offsetNext0(y, yVal))
          case _ =>
        }
    }
    iter(offsetHead(x), offsetHead(y))
    b.result()
  }

  @inline def minus[V, @sp(Double, Long) A](x: V, y: V)(implicit V: VecRing[V, A] with SparseNext0[V, Int, A]): V = {
    import V._
    val d = length(x)
    require(d == length(y))
    val b = builder(d, options(x))
    @tailrec def iter(xOff: OptOffset, yOff: OptOffset): Unit = xOff match {
      case Offset(xVal) =>
        val ix = offsetX0(x, xVal)
        yOff match {
          case Offset(yVal) =>
            val iy = offsetX0(y, yVal)
            if (ix < iy) {
              b.add(ix, offsetValue(x, xVal))
              iter(offsetNext0(x, xVal), yOff)
            } else if (ix == iy) {
              val a = offsetValue(x, xVal) - offsetValue(y, yVal)
              if (zeroA.canBeNonZero(a))
                b.add(ix, a)
              iter(offsetNext0(x, xVal), offsetNext0(y, yVal))
            } else {
              b.add(iy, -offsetValue(y, yVal))
              iter(xOff, offsetNext0(y, yVal))
            }
          case _ =>
            b.add(ix, offsetValue(x, xVal))
            iter(offsetNext0(x, xVal), yOff)
        }
      case _ =>
        y match {
          case Offset(yVal) =>
            val iy = offsetX0(y, yVal)
            b.add(iy, -offsetValue(y, yVal))
            iter(xOff, offsetNext0(y, yVal))
          case _ =>
        }
    }
    iter(offsetHead(x), offsetHead(y))
    b.result()
  }

  @inline def negate[V, @sp(Double, Long) A](v: V)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    @tailrec def iter(offset: OptOffset): Unit = offset match {
      case Offset(valid) =>
        b.add(offsetX0(v, valid), -offsetValue(v, valid))
        iter(offsetNext(v, valid))
      case _ =>
    }
    iter(offsetHead(v))
    b.result()
  }

  @inline def timesl[V, @sp(Double, Long) A](a: A, v: V)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    @tailrec def iter(offset: OptOffset): Unit = offset match {
      case Offset(valid) =>
        b.add(offsetX0(v, valid), a * offsetValue(v, valid))
        iter(offsetNext(v, valid))
      case _ =>
    }
    iter(offsetHead(v))
    b.result()
  }

  @inline def timesr[V, @sp(Double, Long) A](v: V, a: A)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    @tailrec def iter(offset: OptOffset): Unit = offset match {
      case Offset(valid) =>
        b.add(offsetX0(v, valid), offsetValue(v, valid) * a)
        iter(offsetNext(v, valid))
      case _ =>
    }
    iter(offsetHead(v))
    b.result()
  }

  @inline def divr[V, @sp(Double, Long) A](v: V, a: A)(implicit V: VecField[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    @tailrec def iter(offset: OptOffset): Unit = offset match {
      case Offset(valid) =>
        b.add(offsetX0(v, valid), offsetValue(v, valid) / a)
        iter(offsetNext(v, valid))
      case _ =>
    }
    iter(offsetHead(v))
    b.result()
  }

  @inline def dot[V, @sp(Double, Long) A](x: V, y: V)(implicit V: VecRing[V, A] with SparseNext0[V, Int, A]): A = {
    import V._
    val d = length(x)
    require(d == length(y))
    @tailrec def iter(acc: A, xOff: OptOffset, yOff: OptOffset): A = xOff match {
      case Offset(xVal) =>
        val ix = offsetX0(x, xVal)
        yOff match {
          case Offset(yVal) =>
            val iy = offsetX0(y, yVal)
            if (ix < iy)
              iter(acc, offsetNext0(x, xVal), yOff)
            else if (ix == iy)
              iter(acc + offsetValue(x, xVal) * offsetValue(y, yVal), offsetNext0(x, xVal), offsetNext0(y, yVal))
            else
              iter(acc, xOff, offsetNext0(y, yVal))
          case _ => acc
        }
      case _ => acc
    }
    iter(scalar.zero, offsetHead(x), offsetHead(y))
  }
}
