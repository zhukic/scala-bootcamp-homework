package basics

import scala.annotation.tailrec

object Basics {

  def lcm(a: Int, b: Int): Int = (a, b) match {
    case (0, _) => 0
    case (_, 0) => 0
    case (a, b) => Math.abs(a * b) / gcd(a, b)
  }

  @tailrec
  def gcd(a: Int, b: Int): Int = (a, b) match {
    case (0, b) => b
    case (a, 0) => a
    case (a, b) if (a > b) => gcd(b, a % b)
    case (a, b) => gcd(a, b % a)
  }
}

