package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    @tailrec
    def balanceCount(idx: Int, count: Int): Int = {
      if(count < 0) -1
      else if(idx == chars.length) count
      else if(chars(idx) == '(') balanceCount(idx + 1, count + 1)
      else if(chars(idx) == ')') balanceCount(idx + 1, count - 1)
      else balanceCount(idx + 1, count)
    }

    balanceCount(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) /*: ???*/ = {
      var minNest: Int = 0
      var totNest: Int = 0
      for(current <- idx until until) {
        if(chars(current) == '(') totNest += 1
        else if(chars(current) == ')') totNest -= 1
        if(totNest < minNest) minNest = totNest
      }
      (minNest, totNest)
    }

    def reduce(from: Int, until: Int) : (Int, Int) = {
      if(until - from <= threshold) traverse(from, until, 0, 0)
      else {
        val mid =  from + ((until - from)>>1)
        val (left, right) = parallel(
          reduce(from, mid),
          reduce(mid, until)
        )
        (Math.min(left._1, left._2 + right._1), left._2 + right._2)
      }
    }

    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
