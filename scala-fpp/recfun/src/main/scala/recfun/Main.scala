package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = {
    @tailrec def loop(i: Int, acc: Int): Int = {
      i match {
        case `c` => acc
        case _ => loop(i+1, acc * (r - i) / (i + 1))
      }
    }
    loop(0, 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def loop(chars: List[Char], acc: Int): Boolean = {
      chars match {
        case x if acc < 0 => false
        case Nil => acc == 0
        case '(' :: tail => loop(tail, acc + 1)
        case ')' :: tail => loop(tail, acc - 1)
        case head :: tail => loop(tail, acc)
      }
    }
    loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.isEmpty) return 0

    @tailrec def loop(coins: List[Int], acc: Vector[Int]): Int = {
      coins match {
        case Nil => acc(money)
        case head :: tail => loop(tail, update(acc, head, 0))
      }
    }

    @tailrec def update(counts: Vector[Int], coin: Int, idx: Int): Vector[Int] = {
      idx match {
        case x if x == counts.length => counts
        case x if x < `coin` => update(counts, coin, idx + 1)
        case x if x >= `coin` => update(counts.updated(x, counts(x) + counts(x - coin)), coin, idx + 1)
      }
    }

    coins match {
      case some if money >= 0 => loop(coins, Vector(1) ++ Vector.fill(money)(0))
      case _ => 0
    }
  }
}
