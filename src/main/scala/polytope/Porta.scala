package com.faacets
package polytope

import alg._
import scala.util.parsing.combinator._
import spire.math.Rational
import java.io.{File, PrintWriter}

object Porta extends CachedSolver[HRepr, VRepr] {
  import sys.process._

  def vExtension = ".poi"
  def vToHExtension = ".poi.ieq"
  def hExtension = ".ieq"
  def hToVExtension = ".ieq.poi"

  def vToH(vFile: File, hFile: File) {
    val solvedFile = new File(vFile.getAbsoluteFile + ".ieq")
    assert(hFile.getAbsoluteFile == solvedFile.getAbsoluteFile)
    val status = ("traf -o " + vFile.getAbsolutePath).!!
  }

  def hToV(hFile: File, vFile: File) {
    val solvedFile = new File(vFile.getAbsoluteFile + ".poi")
    assert(vFile.getAbsoluteFile == solvedFile.getAbsoluteFile)
    val status = ("traf -o " + hFile.getAbsolutePath).!!
  }

  def vReprToString(vRepr: VRepr) = POIFormat.writePolytope(vRepr).toString
  def hReprToString(hRepr: HRepr) = IEQFormat.writePolytope(hRepr).toString
  def linesToVRepr(lines: Seq[String]) = {
    val filteredLines = lines.filter(!_.matches("""\s*""")).mkString("\n")
    POIFormat.readPolytope(filteredLines)
  }
  def linesToHRepr(lines: Seq[String]) = {
    val filteredLines = lines.filter(!_.matches("""\s*""")).mkString("\n")
    IEQFormat.readPolytope(filteredLines)
  }
}
