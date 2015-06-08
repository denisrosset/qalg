package com.faacets.qalg
package algos

import scala.{specialized => sp}
import scala.reflect.ClassTag

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.additiveGroup._

import algebra._

trait AlgVF[V, @sp(Double, Long) A] extends Any with AlgVE[V, A] with PackVF[V, A]

trait AlgUVF[V, @sp(Double, Long) A] extends Any with AlgUVE[V, A] with AlgVF[V, A]
