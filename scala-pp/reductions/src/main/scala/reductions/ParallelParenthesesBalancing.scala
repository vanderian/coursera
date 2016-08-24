package reductions

import common._
import org.scalameter._

import scala.annotation._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec def loop(chars: List[Char], acc: Int): Boolean = {
      chars match {
        case x if acc < 0 => false
        case Nil => acc == 0
        case '(' :: tail => loop(tail, acc + 1)
        case ')' :: tail => loop(tail, acc - 1)
        case head :: tail => loop(tail, acc)
      }
    }
    loop(chars.toList, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, total: Int, min: Int): (Int, Int) = {
      if (idx >= until) (total, min)
      else chars(idx) match {
        case ')' => traverse(idx + 1, until, total - 1, math.min(min, total - 1))
        case '(' => traverse(idx + 1, until, total + 1, min)
        case _ => traverse(idx + 1, until, total, min)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val middle = (from + until) / 2
        val ((left1, left2), (right1, right2)) = parallel(
          reduce(from, middle),
          reduce(middle, until))

        (left1 + right1, left2 + right2 + left1)
      }
    }
    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
