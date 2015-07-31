package com.faacets.qalg
package impl

import scala.{specialized => sp}
import scala.annotation.tailrec

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.field._

import algebra._
import indup.algebra._

object VecDense {
  final class MapperImpl[@sp(Double, Long) A, @sp(Double, Long) B, V, W](val builder: V => VecBuilder[W, B])(implicit val V: VecBuild[V, A], val W: VecBuild[W, B]) extends Mapper[A, B, V, W] {
    def map(v: V)(f: A => B): W = {
      val d = V.length(v)
      val b = builder(v)
      cforRange(0 until d) { i =>
        b.add(i, f(V.apply(v, i)))
      }
      b.result()
    }
  }
/*  def map[B, Tag, N](m: M)(f: A => B)(implicit ev: Base[M, A, Tag], ev1: Refine[Tag, B, N], N: MatBuild[N, B] { type Options = self.Options }): N = {
    val b = N.builder(size0(m), size1(m), options(m))
    @tailrec def iter(offset: Offset[IsValid]): Unit = offset match {
      case Valid(valid) =>
        b.add(offsetX0(m, valid), offsetX1(m, valid), f(offsetValue(m, valid)))
        iter(offsetNext(m, valid))
      case _ =>
    }
    b.result()
  }*/

  @inline def feedTo[V, @sp(Double, Long) A](v: V, b: VecBuilder[_, A])(implicit V: VecBuild[V, A]): Unit = {
    import V._
    cforRange(0 until length(v)) { i =>
      b.add(i, apply(v, i))
    }
  }

  @inline def plus[V, @sp(Double, Long) A](x: V, y: V)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(x)
    require(d == length(y))
    val b = builder(d, options(x))
    cforRange(0 until d) { i =>
      b.add(i, apply(x, i) + apply(y, i))
    }
    b.result()
  }

  @inline def minus[V, @sp(Double, Long) A](x: V, y: V)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(x)
    require(d == length(y))
    val b = builder(d, options(x))
    cforRange(0 until d) { i =>
      b.add(i, apply(x, i) - apply(y, i))
    }
    b.result()
  }

  @inline def negate[V, @sp(Double, Long) A](v: V)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    cforRange(0 until d) { i =>
      b.add(i, -apply(v, i))
    }
    b.result()
  }

  @inline def timesl[V, @sp(Double, Long) A](a: A, v: V)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    cforRange(0 until d) { i =>
      b.add(i, a * apply(v, i))
    }
    b.result()
  }

  @inline def timesr[V, @sp(Double, Long) A](v: V, a: A)(implicit V: VecRing[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    cforRange(0 until d) { i =>
      b.add(i, apply(v, i) * a)
    }
    b.result()
  }

  @inline def divr[V, @sp(Double, Long) A](v: V, a: A)(implicit V: VecField[V, A]): V = {
    import V._
    val d = length(v)
    val b = builder(d, options(v))
    cforRange(0 until d) { i =>
      b.add(i, apply(v, i) / a)
    }
    b.result()
  }

  @inline def dot[V, @sp(Double, Long) A](x: V, y: V)(implicit V: VecRing[V, A]): A = {
    import V._
    val d = length(x)
    require(d == length(y))
    var a = scalar.zero
    cforRange(0 until d) { i =>
      a += apply(x, i) * apply(y, i)
    }
    a
  }
}
