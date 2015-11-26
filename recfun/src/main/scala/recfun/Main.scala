package recfun

import java.util

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
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], stack: util.Stack[Char]): Boolean = {
      chars match {
        case Nil => stack.isEmpty
        case head :: rest =>
          head match {
            case '(' => stack.push(head); balance(rest, stack)
            case ')' =>
              if (stack.isEmpty) {
                false
              } else {
                stack.pop()
                balance(rest, stack)
              }
            case _ => balance(rest, stack)
          }
      }
    }

    val s = new util.Stack[Char]()
    balance(chars, s)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def doCountChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money > 0 && coins.nonEmpty) {
        doCountChange(money, coins.tail) + doCountChange(money - coins.head, coins)
      } else {
        0
      }
    }

    doCountChange(money, coins.sortWith(_ < _))
  }
}
