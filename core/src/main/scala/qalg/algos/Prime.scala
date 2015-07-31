package com.faacets.qalg
package algebra

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._

import algos._

trait Prime[T, @sp(Double, Long) A] extends Any {
  def withPrimes(t: T): T
  def commonFactor(t: T): A
}

object Prime {
  implicit def matPrimeFromPack[M, @sp(Double, Long) A](implicit pack: PackEuclideanRing.ForM[M, A]): Prime[M, A] = pack.MPrime
  implicit def vecPrimeFromPack[V, @sp(Double, Long) A](implicit pack: PackEuclideanRing.ForV[V, A]): Prime[V, A] = pack.VPrime
}

trait MutablePrime[T, @sp(Double, Long) A] extends Any with Prime[T, A] {
  def replaceByPrimes(t: T): Unit
}

object MutablePrime {
  implicit def matPrimeFromPack[M, @sp(Double, Long) A](implicit pack: PackEuclideanRingMutable.ForM[M, A]): MutablePrime[M, A] = pack.MPrime
  implicit def vecPrimeFromPack[V, @sp(Double, Long) A](implicit pack: PackEuclideanRingMutable.ForV[V, A]): MutablePrime[V, A] = pack.VPrime
}
