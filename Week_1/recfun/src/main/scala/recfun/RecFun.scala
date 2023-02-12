package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if c == 0 then 1
    else if c == r then 1
    else pascal(c-1, r-1) + pascal (c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def balanced(chars: List[Char], open: Int): Boolean =
      if chars.isEmpty then open == 0
      else if chars.head == '(' then balanced(chars.tail, open+1)
      else if chars.head == ')' then open > 0 && balanced(chars.tail, open-1)
      else balanced(chars.tail, open)

    balanced(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
