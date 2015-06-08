package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._

import algos._

trait Prime[L, @sp(Double, Long) A] extends Any {
  def withPrimes(l: L): L
  def commonFactor(l: L): A
}

trait MutablePrime[L, @sp(Double, Long) A] extends Any with Prime[L, A] {
  def replaceByPrimes(l: L): Unit
}

final class PrimeImpl[L, @sp(Double, Long) A: Eq](implicit L: LinBuilder[L, A], A: EuclideanRing[A]) extends Prime[L, A] {
  def commonFactor(l: L): A = {
    var cumGcd = A.zero
    cforRange(0 until L.linearLength(l)) { k =>
      val a = L.linearApply(l, k)
      if (!A.isZero(a))
        cumGcd = spire.math.gcd(cumGcd, a)
    }
    cumGcd
  }

  def withPrimes(l: L): L = {
    val cf = commonFactor(l)
    L.map(l)( a => A.quot(a, cf) )
  }
}

final class VecMutablePrimeImpl[V, @sp(Double, Long) A: Eq](implicit V: VecBuilder[V, A], VM: VecMutable[V, A], A: EuclideanRing[A]) extends Prime[V, A] {
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
    V.tabulate(V.length(v)) { k => A.quot(V.apply(v, k), cf) }
  }
  def replaceByPrimes(v: V): Unit = {
    val cf = commonFactor(v)
    cforRange(0 until V.length(v)) { k =>
      VM.update(v, k, A.quot(V.apply(v, k), cf))
    }
  }
}

final class MatMutablePrimeImpl[M, @sp(Double, Long) A: Eq](implicit M: MatBuilder[M, A], MM: MatMutable[M, A], A: EuclideanRing[A]) extends Prime[M, A] {
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
    M.tabulate(M.nRows(m), M.nCols(m)) { (r, c) => A.quot(M.apply(m, r, c), cf) }
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
