package com.faacets.qalg
package algebra

import scala.language.higherKinds

import scala.{specialized => sp}

import spire.algebra._
import spire.syntax.cfor._
import spire.syntax.eq._
import util._

trait MatMutable[MA, @sp(Double, Long) A] extends Any { self =>
  def update(m: MA, r: Int, c: Int, a: A): Unit
  /*
   def update[MA1](m: MA, rows: At1, cols: At1, up: MA1)(implicit MA1: Mat[MA1, A]): MA = {
   var r = 0
   val nR = rows.length
   val nC = cols.length
   while (r <
   }*/
  /*
   def apply(m: MA, rows: ::.type, cols: ::.type): MA = m
   def apply(m: MA, rows: ::.type, cols: At1): MA = from(new FunM[A] {
   def nR: Int = self.nRows(m)
   def nC: Int = cols.length
   def f(r: Int, c: Int): A = self.apply(m, r, cols(c))
   })
   def apply(m: MA, rows: At1, cols: ::.type): MA = from(new FunM[A] {
   def nR: Int = rows.length
   def nC: Int = self.nCols(m)
   def f(r: Int, c: Int): A = self.apply(m, rows(r), c)
   })
   def t(m: MA): MA = from(new FunM[A] {
   def nR: Int = self.nCols(m)
   def nC: Int = self.nRows(m)
   def f(r: Int, c: Int): A = self.apply(m, c, r)
   })*/
}

object MatMutable {
  def apply[MA, @sp(Double, Long) A](implicit MM: MatMutable[MA, A]): MatMutable[MA, A] = MM
}
