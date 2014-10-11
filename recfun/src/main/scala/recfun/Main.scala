package recfun
import common._

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
   * The pascal function takes a column c and a row r, counting from 0
   * and returns the number at that spot in the triangle.
   *
   * For example, pascal(0,2)=1, pascal(1,2)=2 and pascal(1,3)=3.
   */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 && c > 0)
      0
    else if (c == 0)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * A recursive function which verifies the balancing of parentheses in a string,
   * which we represent as a List[Char] not a String.
   */
  def balance(chars: List[Char]): Boolean = {

    def balancedParantheses(chars: List[Char], openParantheses: Int): Boolean = {
      if (chars.isEmpty)
        openParantheses == 0
      else if (chars.head == '(')
        balancedParantheses(chars.tail, openParantheses + 1)
      else if (chars.head == ')')
        openParantheses > 0 && balancedParantheses(chars.tail, openParantheses - 1)
      else
        balancedParantheses(chars.tail, openParantheses)
    }

    balancedParantheses(chars, 0)

  }

  /**
   * A recursive function that counts how many different ways you can make change for an amount,
   * given a list of coin denominations.
   *
   * For example, there are 3 ways to give change for 4 -
   * if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0)
        0
      else if (coins.isEmpty && money >= 1)
        0
      else
        count(money, coins.tail) + count(money - coins.head, coins)
    }

    count(money, coins.sortWith(_.compareTo(_) < 0))
  }
}
