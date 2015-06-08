package com.faacets.qalg

import scala.language.implicitConversions

import scala.{specialized => sp}

import spire.NoImplicit
import spire.algebra._

trait ConversionsLowerPriority {
  import algebra._
  implicit def packMatModule[M, @sp(Double, Long) A](implicit ev: PackMR[M, A], ev1: NoImplicit[PackMF[M, A]]): Module[M, A] = ev.M
  implicit def packVecModule[V, @sp(Double, Long) A](implicit ev: PackVR[V, A], ev1: NoImplicit[PackVF[V, A]]): Module[V, A] = ev.V
}

package object algebra extends ConversionsLowerPriority {
  implicit def packMatMSG[M, @sp(Double, Long) A](implicit ev: PackMR[M, A]): MultiplicativeSemigroup[M] = ev.M
  implicit def packMatVectorSpace[M, @sp(Double, Long) A](implicit ev: PackMF[M, A]): VectorSpace[M, A] = ev.M
  implicit def packVecIPS[V, @sp(Double, Long) A](implicit ev: PackVF[V, A]): InnerProductSpace[V, A] = ev.V
  implicit def packMatEq[M, @sp(Double, Long) A](implicit ev: PackMR[M, A]): Eq[M] = ev.M
  implicit def packVecEq[V, @sp(Double, Long) A](implicit ev: PackVR[V, A]): Eq[V] = ev.V
}
