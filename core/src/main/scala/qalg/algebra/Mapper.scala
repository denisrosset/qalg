package com.faacets.qalg
package algebra

import scala.{specialized => sp}
import scala.annotation.tailrec

import indup.algebra._
import syntax.indup.all._

trait Mapper[@sp(Double, Long) A, @sp(Double, Long) B, T, U] extends Any {
  def map(t: T)(f: A => B): U
}

object Mapper {
  implicit def Mapper[@sp(Double, Long) A, @sp(Double, Long) B, T, Tag, U, O](implicit ev: Base[T, Tag], ev1: Refine[Tag, B, U], T: VecBuild.WithOptions[T, A, O], U: VecBuild.WithOptions[U, B, O]): Mapper[A, B, T, U] = new Mapper[A, B, T, U] {
    def map(t: T)(f: A => B): U = {
      val options = T.options(t)
      val b = U.builder(T.length(t), options)
      @tailrec def iter(o: OptOffset): Unit = o match {
        case Offset(offset) =>
          b.add(t.offsetX0(offset), f(t.offsetValue(offset)))
          iter(t.offsetNext(offset))
        case _ =>
      }
      iter(t.offsetHead)
      b.result()
    }
  }
}
