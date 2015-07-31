package com.faacets.qalg
package algos
package impl

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._

import algebra._
import algos._

final class DenseVecPrimeImpl[V, @sp(Double, Long) A: Eq](implicit val V: VecBuild[V, A], A: EuclideanRing[A]) extends Prime[V, A] {
  def commonFactor(v: V): A = {
    var cumGcd = A.zero
    cforRange(0 until V.length(v)) { k =>
        val a = V.apply(v, k)
      if (!A.isZero(a))
        cumGcd = spire.math.gcd(cumGcd, a)
    }
    cumGcd
  }

  def withPrimes(v: V): V = {
    val cf = commonFactor(v)
    val b = V.builder(v)
    cforRange(0 until V.length(v)) { k =>
        b.add(k, A.quot(V.apply(v, k), cf))
    }
    b.result()
  }
}

final class DenseMatPrimeImpl[M, @sp(Double, Long) A: Eq](implicit val M: MatBuild[M, A], A: EuclideanRing[A]) extends Prime[M, A] {
  def commonFactor(m: M): A = {
    var cumGcd = A.zero
    cforRange(0 until M.nRows(m)) { r =>
      cforRange(0 until M.nCols(m)) { c =>
        val a = M.apply(m, r, c)
        if (!A.isZero(a))
          cumGcd = spire.math.gcd(cumGcd, a)
      }
    }
    cumGcd
  }

  def withPrimes(m: M): M = {
    val cf = commonFactor(m)
    val b = M.builder(m)
    cforRange(0 until M.nRows(m)) { r =>
      cforRange(0 until M.nCols(m)) { c =>
        b.add(r, c, A.quot(M.apply(m, r, c), cf))
      }
    }
    b.result()
  }
}


final class DenseVecMutablePrimeImpl[V, @sp(Double, Long) A: Eq](implicit V: VecBuild[V, A], VM: VecMut[V, A], A: EuclideanRing[A]) extends MutablePrime[V, A] {
  def commonFactor(v: V): A = {
    var cumGcd = A.zero
    cforRange(0 until V.length(v)) { k =>
      val a = V.apply(v, k)
      if (!A.isZero(a))
        cumGcd = spire.math.gcd(cumGcd, a)
    }
    cumGcd
  }

  def withPrimes(v: V): V = {
    val cf = commonFactor(v)
    val b = V.builder(v)
    cforRange(0 until V.length(v)) { k =>
      b.add(k, A.quot(V.apply(v, k), cf))
    }
    b.result()
  }

  def replaceByPrimes(v: V): Unit = {
    val cf = commonFactor(v)
    cforRange(0 until V.length(v)) { k =>
      VM.update(v, k, A.quot(V.apply(v, k), cf))
    }
  }
}

final class DenseMatMutablePrimeImpl[M, @sp(Double, Long) A: Eq](implicit M: MatBuild[M, A], MM: MatMut[M, A], A: EuclideanRing[A]) extends MutablePrime[M, A] {
  def commonFactor(m: M): A = {
    var cumGcd = A.zero
    cforRange(0 until M.nRows(m)) { r =>
      cforRange(0 until M.nCols(m)) { c =>
        val a = M.apply(m, r, c)
        if (!A.isZero(a))
          cumGcd = spire.math.gcd(cumGcd, a)
      }
    }
    cumGcd
  }

  def withPrimes(m: M): M = {
    val cf = commonFactor(m)
    val b = M.builder(m)
    cforRange(0 until M.nRows(m)) { r =>
      cforRange(0 until M.nCols(m)) { c =>
        b.add(r, c, A.quot(M.apply(m, r, c), cf))
      }
    }
    b.result()
  }

  def replaceByPrimes(m: M): Unit = {
    val cf = commonFactor(m)
    cforRange(0 until M.nRows(m)) { r =>
      cforRange(0 until M.nCols(m)) { c =>
        MM.update(m, r, c, A.quot(M.apply(m, r, c), cf))
      }
    }
  }
}
