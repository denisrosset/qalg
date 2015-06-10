package com.faacets.qalg
package algos

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.additiveGroup._

import algebra._

trait AlgVE[V, @sp(Double, Long) A] extends Any with AlgVR[V, A] {
  implicit def VGramSchmidt: VGramSchmidt[V, A]
  implicit def VPrime: Prime[V, A]
}

trait AlgUVE[V, @sp(Double, Long) A] extends Any with AlgUVR[V, A] with AlgVE[V, A] {
  implicit def VPrime: MutablePrime[V, A]
}
