package com.faacets.qalg.macros

import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}

//import spire.macros.compat.{termName, freshTermName, resetLocalAttrs, Context, setOrig}

//import scala.language.higherKinds

object Ops extends machinist.Ops {

  def uesc(c: Char): String = "$u%04X".format(c.toInt)

  val operatorNames: Map[String, String] =
    machinist.DefaultOps.operatorNames ++ Map(
      "$colon$colon$times" -> "timesr2",
      "$times$colon$colon" -> "timesl2"
    )

  def binopUsingEv2[A, Ev1, R](c: Context)(rhs: c.Expr[A])(ev1: c.Expr[Ev1]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev1.tree, findMethodName(c)), List(lhs, rhs.tree)))
  }

  def triop[A1, A2, R](c: Context)(rhs1: c.Expr[A1], rhs2: c.Expr[A2]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs, rhs1.tree, rhs2.tree)))
  }

  def triopWithEv2[A1, A2, Ev1, R](c: Context)(rhs1: c.Expr[A1], rhs2: c.Expr[A2])(ev1: c.Expr[Ev1]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Apply(Select(ev, findMethodName(c)), List(lhs, rhs1.tree, rhs2.tree)), List(ev1.tree)))
  }

  def triopUsingEv2[A1, A2, Ev1, R](c: Context)(rhs1: c.Expr[A1], rhs2: c.Expr[A2])(ev1: c.Expr[Ev1]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev1.tree, findMethodName(c)), List(lhs, rhs1.tree, rhs2.tree)))
  }

  def tetraop[A1, A2, A3, R](c: Context)(rhs1: c.Expr[A1], rhs2: c.Expr[A2], rhs3: c.Expr[A3]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Select(ev, findMethodName(c)), List(lhs, rhs1.tree, rhs2.tree, rhs3.tree)))
  }

  def tetraopWithEv2[A1, A2, A3, Ev1, R](c: Context)(rhs1: c.Expr[A1], rhs2: c.Expr[A2], rhs3: c.Expr[A3])(ev1: c.Expr[Ev1]): c.Expr[R] = {
    import c.universe._
    val (ev, lhs) = unpack(c)
    c.Expr[R](Apply(Apply(Select(ev, findMethodName(c)), List(lhs, rhs1.tree, rhs2.tree, rhs3.tree)), List(ev1.tree)))
  }
}
