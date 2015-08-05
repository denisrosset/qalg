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
  val seed = 0xFAACE73

  @inline def hash[@sp(Double, Long) A: Zero](v: math.Vector[A, _]): Int = {
    import scala.util.hashing.MurmurHash3._
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until v.size) { k =>
      val x = v(k)
      if (Zero[A].canBeNonZero(x)) {
        val h = k * 41 + x.##
        a += h
        if (h != 0) b *= h
        n += 1
      }
    }
    var h = seed
    h = mix(h, v.size)
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  @inline def hash[V, @sp(Double, Long) A](v: V)(implicit V: Vec[V, A]): Int = {
    import V._
    import scala.util.hashing.MurmurHash3._
    var a = 0
    var b = 1L
    var n = 0
    cforRange(0 until length(v)) { k =>
        val x = apply(v, k)
      if (zeroA.canBeNonZero(x)) {
        val h = k * 41 + x.##
        a += h
        if (h != 0) b *= h
        n += 1
      }
    }
    var h = seed
    h = mix(h, length(v))
    h = mix(h, a)
    h = mix(h, b.toInt)
    h = mixLast(h, (b >> 32).toInt)
    finalizeHash(h, n)
  }

  @inline def equal[@sp(Double, Long) A](lhs: math.Vector[A, _], rhs: math.Vector[A, _]): Boolean = {
    val n = lhs.size
    if (rhs.size != n) return false
    cforRange(0 until n) { k =>
      if (lhs(k) != rhs(k)) return false
    }
    true
  }

  @inline def eqv[V, @sp(Double, Long) A: Eq](lhs: V, rhs: V)(implicit V: Vec[V, A]): Boolean = {
    val n = V.length(lhs)
    if (V.length(rhs) != n) return false
    cforRange(0 until n) { k =>
      if (Eq[A].neqv(V.apply(lhs, k), V.apply(rhs, k))) return false
    }
    true
  }

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
