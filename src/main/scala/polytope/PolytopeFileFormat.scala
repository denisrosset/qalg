package com.faacets
package polytope

import alg._
import scala.util.parsing.combinator._

trait PolytopeFileFormat extends RegexParsers with RationalParserTrait {
  def crlf = "\r\n" | "\n"
}

/*
 val str = """ * Test
 H-representation
 begin
 4   3   rational
 12/5   2  -1
 -6  -1   2
 -3   1   1
 1   1   0 
 end
 """
 import com.faacets.polytope._
 val h = HRepresentation1999.parse(HRepresentation1999.content, str)

 */
