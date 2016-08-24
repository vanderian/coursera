package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("poly none") {
    val delta = Polynomial.computeDelta(Var(1), Var(2), Var(5))()
    assert(delta < 0, "delta")
    val res = Polynomial.computeSolutions(Var(1), Var(2), Var(5), Var(delta))
    assert(res().isEmpty)
  }

  test("poly one") {
    val delta = Polynomial.computeDelta(Var(3), Var(-6), Var(3))()
    assert(delta === 0, "delta")
    val res = Polynomial.computeSolutions(Var(3), Var(-6), Var(3), Var(delta))
    assert(Set(1) === res())
  }

  test("poly two") {
    val delta = Polynomial.computeDelta(Var(3), Var(5), Var(2))()
    assert(delta === 1, "delta")
    val res = Polynomial.computeSolutions(Var(3), Var(5), Var(2), Var(delta))
    assert(Set(-2/3.0, -1) === res())
  }

  test("calc simple") {
    val res1 = Calculator.computeValues(Map("a" -> Var(Plus(Literal(1), Literal(1)))))
    assert(res1("a")() === 2, "plus")
    val res2 = Calculator.computeValues(Map("a" -> Var(Minus(Literal(1), Literal(1)))))
    assert(res2("a")() === 0, "minus")
    val res3 = Calculator.computeValues(Map("a" -> Var(Times(Literal(2), Literal(5)))))
    assert(res3("a")() === 10, "times")
    val res4 = Calculator.computeValues(Map("a" -> Var(Divide(Literal(10), Literal(2)))))
    assert(res4("a")() === 5, "div")
  }

  test("calc not existing var") {
    val res = Calculator.computeValues(Map("a" -> Var(Plus(Ref("b"), Literal(1)))))
    assert(res("a")() equals Double.NaN)
  }

  test("calc cyclic var") {
    val res = Calculator.computeValues(Map("a" -> Var(Ref("b")), "b" -> Var(Ref("c")), "c" -> Var(Ref("d")), "d" -> Var(Ref("b"))))
    assert(res("a")() equals Double.NaN)
    assert(res("b")() equals Double.NaN)
  }

}
