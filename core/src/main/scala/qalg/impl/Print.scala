package com.faacets.qalg
package impl

import algebra._
import syntax.all._

object Print {
  var maxToStringWidth = 80
  var maxToStringRows = 15
  def print(rows: Int, cols: Int, content: (Int, Int) => String): String = {
    if (rows == 0 || cols == 0)
      return f"$rows%d x $cols%d matrix"

    def colWidth(col: Int) = (0 until rows).map(row => content(row,col).toString.length+1).max
    val colWidths = (0 until cols).map(colWidth)
    val colText = "... " + cols + " total ..."

    def paddedCell(row: Int, col: Int) = {
      val cell = content(row, col).toString
      " " * (colWidths(col) - cell.length) + cell
    }

    val printRow: (Int => String) = if (colWidths.sum > maxToStringWidth && cols > 2) {
      val widthForNandNCols = (colWidths zip colWidths.reverse).take(cols/2).map( Function.tupled( (i,j) => i+j ) ).scanLeft(0)(_+_)
      val maxWidthLeft = maxToStringWidth - colText.length
      val howManyCols = widthForNandNCols.zipWithIndex.find(_._1 > maxWidthLeft).getOrElse((0,2))._2 - 1
      (row: Int) => (List() ++
        (0 until howManyCols).map(col => paddedCell(row, col)) ++
        List(colText) ++
        (cols - howManyCols until cols).map(col => paddedCell(row, col))
      ).mkString
    } else
      (row: Int) => (0 until cols).map(col => paddedCell(row, col)).mkString

    if (rows/2 > maxToStringRows/2 - 1) {
      val howManyRows = List(maxToStringRows/2 - 1, rows/2).min

      (List() ++
        (0 until howManyRows).map(printRow) ++
        List(f"... $rows%d in total ...") ++
        (rows - howManyRows until rows).map(printRow)
      ).mkString("\n")
    } else
      (0 until rows).map(printRow).mkString("\n")
  }
}
