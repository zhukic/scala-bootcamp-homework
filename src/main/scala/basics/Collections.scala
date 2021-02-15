package basics

object Collections extends App {

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    s.foldLeft(List[(Char, Int)]()) { (acc, c) =>
      acc match {
        case Nil => List((c, 1))
        case _ =>
          val last = acc.head
          if (last._1 == c) last.copy(_2 = last._2 + 1) :: acc.tail
          else (c, 1) :: acc
      }
    }.reverse
  }

  println(count("aaadddbbbdada"))

  // https://leetcode.com/problems/running-sum-of-1d-array
  def runningSum(nums: Array[Int]): Array[Int] = {
    nums.scan(0) { _ + _ }.tail
  }

  // https://leetcode.com/problems/shuffle-the-array/
  def shuffle(nums: Array[Int], n: Int): Array[Int] = {
    nums.take(n).zip(nums.drop(n)).flatMap { case (first, second) => Array(first, second) }
  }

  // https://leetcode.com/problems/richest-customer-wealth/
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.max).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max = candies.max
    candies.map(_ + extraCandies >= max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    val sortedByX = points.map(_(0)).sorted
    sortedByX.zip(sortedByX.tail)
      .map { case (first, second) => second - first }
      .max
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maxDepth(s: String): Int = s.filter(c => c == '(' || c == ')')
    .scanLeft(0) { (acc, c) => if (c == '(') acc + 1 else acc - 1}
    .max

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def balancedStringSplit(s: String): Int = s
    .scanLeft(0) { (acc, c) =>
      c match {
        case 'R' => acc + 1
        case 'L' => acc - 1
      }
    }
    .count(_ == 0) - 1

  // https://leetcode.com/problems/matrix-block-sum
  def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = mat.indices
    .map { i =>
      mat(i).indices.map { j =>
        (math.max(0 , i - K) to math.min(mat.length - 1, i + K)).map {
          ii => (math.max(0, j - K) to math.min(mat(i).length - 1, j + K))
            .map { jj => mat(ii)(jj) }
            .sum
        }.sum
      }.toArray
    }.toArray
}
