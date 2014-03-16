package com.faacets
package polytope

import scala.language.existentials
import polytope._
import net.alasc._
import alg._
import spire.math.Rational
import java.io.{File, PrintWriter}

object Sympol extends CachedSolver[SymHRepr, SymVRepr] {
  import sys.process._

  def vExtension = ".ext"
  def vToHExtension = ".ine"
  def hExtension = ".ine"
  def hToVExtension = ".ext"

  def vToH(vFile: File, hFile: File) {
    val content = ("sympol -a --cdd -i " + vFile.getAbsolutePath).!!
    val filteredContent = content.drop(content.indexOf("H-representation"))

    val writer = new PrintWriter(hFile)
    writer.write(filteredContent)
    writer.close
  }

  def hToV(hFile: File, vFile: File) {
    val content = ("sympol -a --cdd -i " + hFile.getAbsolutePath).!!
    val filteredContent = content.drop(content.indexOf("V-representation"))

    val writer = new PrintWriter(vFile)
    writer.write(filteredContent)
    writer.close
  }

  def vReprToString(vRepr: SymVRepr): String = {
    assert(vRepr.r.rows == 0) // FIXME: allow rays
    val vpol = new StringBuilder()
    val eGroupDesc = vRepr.sym.generators.map(g => {
      g.cycles.seq.filter(_.seq.size>1).map(cycle => cycle.seq.map(_._1).mkString(" ")).mkString(",")
    }).toList
    vpol ++= "V-representation\n"
    vpol ++= "begin\n"
    vpol ++= (vRepr.e.rows + " " + (vRepr.e.cols+1) + " rational\n")
    for (r <- 0 until vRepr.e.rows) 
      vpol ++= ("1 " + (0 until vRepr.e.cols).map(vRepr.e(r,_)).mkString(" ") + "\n")
    vpol ++= "end\n"
    vpol ++= "permutation group\n"
    vpol ++= (eGroupDesc.size + "\n")
    eGroupDesc.foreach(vpol ++= _ + "\n")
    vpol ++= "0\n" // FIXME: the group base is not written
    vpol.toString
  }

  def hReprToString(hRepr: SymHRepr): String = {
    assert(hRepr.aeq.rows == 0) // FIXME: equality constraints are not supported

    val hpol = new StringBuilder()
    val ineqGroupDesc = hRepr.sym.generators.map(gel => {
      gel.cycles.seq.filter(_.seq.size>1)
        .map(cycle => cycle.seq.map(_._1).mkString(" ")).mkString(",")
    }).toList
    hpol ++= "H-representation\n"
    hpol ++= "begin\n"
    hpol ++= hRepr.a.rows + " " + (hRepr.a.cols + 1) + " rational\n"

    for (r <- 0 until hRepr.a.rows) {
      hpol ++= hRepr.b(r) + " " + (0 until hRepr.a.cols).map(-hRepr.a(r,_)).mkString(" ") + "\n"
    }
    hpol ++= "end\n"
    hpol ++= "permutation group\n"
    hpol ++= (ineqGroupDesc.size + "\n")
    ineqGroupDesc.foreach(hpol ++= _ + "\n")
    hpol ++= "0\n" // FIXME: the group base is not writtenx
    hpol.toString
  }

  def linesToVRepr(lines: Seq[String]) = V1999Format.readPolytope(lines.mkString("\n"))
  def linesToHRepr(lines: Seq[String]) = H1999Format.readPolytope(lines.mkString("\n"))

}
