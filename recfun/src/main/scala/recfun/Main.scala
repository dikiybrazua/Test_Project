package recfun

import sun.font.TrueTypeFont

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = if (c == 0 | c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val chars1 = chars.map(x => if (x == '(') 1
    else if (x == ')') -1 else 0)
    def check(nums: List[Int]): Boolean =
      if (nums.isEmpty) true
      else if (nums.sum > 0) false
      else check(nums.tail)
    check(chars1) & chars1.sum == 0
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countrec(money: Int, coins: List[Int]): Int =
      if (coins isEmpty) 0
      else if (money == 0) 1
      else if (money < 0) 0
      else countrec(money - coins.head, coins) + countrec(money, coins.tail)


      countrec(money, coins)
  }
}
