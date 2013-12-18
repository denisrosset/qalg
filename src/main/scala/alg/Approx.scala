/*
# Approximations of floating point numbers using `Rational` numbers

To enable interoperability of librairies build with `polyta` and programming
languages such as `MATLAB`, we enable the construction of `QVector` and `QMatrix`
objects using `Double` instances.

The user always specifies a maximal denominator \\( n \\).

The conversion will be successful if an approximation of a given `Double` can be found with
the given maximal denominator and tolerance.
*/

package com.faacets
package alg

import spire.math.Rational

object Approx {
/*
The methods `between0and1` and `apply` converts a floating point number `x`
to a `Rational` approximation with maximal denominator `n`.

The method `between0and1` has the restriction that \\( 0 \le x \le 1 \\).

This uses an implementation of the Farey algorithm translated from the Python one found
at [http://www.johndcook.com/blog/2010/10/20/best-rational-approximation/].

TODO: this is not optimal and should be replaced by the implementation found in
[http://hg.python.org/cpython/file/2.7/Lib/fractions.py].
*/
  def between0and1(x: Double, n: Int): Rational = {
    require(0 <= x && x <= 1)
    var (a, b) = (0, 1)
    var (c, d) = (1, 1)
    while (b <= n && d <= n) {
      val mediant = (a + c).toDouble/(b+d)
      x.compareTo(mediant) match {
        case 0 =>
          if (b + d <= n)
            return Rational(a+c, b+d)
          else if (d > b)
            return Rational(c, d)
          else
            return Rational(a, b)
        case 1 =>
          a += c
          b += d
        case -1 =>
          c += a
          d += b
      }
    }
    if (b > n)
      Rational(c, d)
    else
      Rational(a, b)
  }

  def apply(x: Double, n: Int): Rational = {
    val fracPart = x - x.floor
    val Some(intPart) = BigDecimal.apply(x - fracPart).toBigIntExact
    between0and1(fracPart, n) + intPart
  }
/*
Converts an `Double` array to a `Rational` array with each element having maximal
denominator `n`. Returns the converted array and the maximal error.
*/
  def convertArray(xs: Array[Double], n: Int): (Array[Rational], Double) = {
    val m: Map[Double, Rational] = xs.toSet.map( (x: Double) => (x, apply(x, n)) ).toMap
    val maxError = (m.map { case (d, r) => (d - r.toDouble).abs }).max
    val rationalArray = xs.map(m(_))
    (rationalArray, maxError)
  }
}
