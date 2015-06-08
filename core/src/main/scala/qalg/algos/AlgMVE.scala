package com.faacets.qalg
package algos

import scala.{specialized => sp}

import algebra._

trait AlgMVE[M, V, @sp(Double, Long) A] extends Any with AlgMVR[M, V, A] {
  implicit def MGramSchmidt: GramSchmidt[M]
  implicit def MPrime: Prime[M, A]
}

trait AlgUMVE[M, V, @sp(Double, Long) A] extends Any with AlgUMVR[M, V, A] with AlgMVE[M, V, A] {
  implicit def MGramSchmidt: MutableGramSchmidt[M]
  implicit def MPrime: MutablePrime[M, A]
}
