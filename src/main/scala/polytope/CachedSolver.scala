package com.faacets
package polytope

trait CachedSolver[HR <: HRepr, VR <: VRepr] {
  import java.io.{File, PrintWriter}

  def vToH(vFile: File, hFile: File): Unit
  def hToV(hFile: File, vFile: File): Unit

  def vReprToString(vRepr: VR): String
  def hReprToString(hRepr: HR): String

  def linesToVRepr(lines: Seq[String]): VRepr
  def linesToHRepr(lines: Seq[String]): HRepr

  def vExtension: String
  def hExtension: String
  def vToHExtension: String
  def hToVExtension: String

  val baseDirectory = new File(".")
  val polytopeDirectory = new File(baseDirectory.getCanonicalPath(), "polytope-cache")

  def sha256(str: String): String = {
    import java.security.MessageDigest

    val md = MessageDigest.getInstance("SHA-256")
    val hash = md.digest(str.getBytes("US-ASCII"))
    BigInt(1, hash).toString(16)
  }

  def sanitizePathElement(pathElement: String): String =
    pathElement.filter(c => java.lang.Character.isJavaIdentifierPart(c))

  def vToH(vRepr: VR, hint: String = ""): HRepr = {
    val vStr = vReprToString(vRepr)
    val hash = sha256(vStr)
    val cacheDir = if (hint == "") polytopeDirectory else new File(polytopeDirectory, sanitizePathElement(hint))
    if (!cacheDir.isDirectory && !cacheDir.mkdirs) sys.error("Could not create cache directory.")
    val inputFile = new File(cacheDir, hash + vExtension)
    val outputFile = new File(cacheDir, hash + vToHExtension)
    if (!outputFile.exists) {
      val writer = new PrintWriter(inputFile)
      writer.write(vStr)
      writer.close

      vToH(inputFile, outputFile)
    }

    val source = scala.io.Source.fromFile(outputFile)
    val lines = source.getLines.toSeq
    val hRepr = linesToHRepr(lines)
    source.close
    hRepr
  }

  def hToV(hRepr: HR, hint: String = ""): VRepr = {
    val hStr = hReprToString(hRepr)
    val hash = sha256(hStr)
    val cacheDir = if (hint == "") polytopeDirectory else new File(polytopeDirectory, sanitizePathElement(hint))
    if (!cacheDir.isDirectory && !cacheDir.mkdirs) sys.error("Could not create cache directory.")
    val inputFile = new File(cacheDir, hash + hExtension)
    val outputFile = new File(cacheDir, hash + hToVExtension)
    if (!outputFile.exists) {
      val writer = new PrintWriter(inputFile)
      writer.write(hStr)
      writer.close

      hToV(inputFile, outputFile)
    }

    val source = scala.io.Source.fromFile(outputFile)
    val lines = source.getLines.toSeq
    val vRepr = linesToVRepr(lines)
    source.close
    vRepr
  }
}
