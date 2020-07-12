package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    require(r >= c, "invalid input")
    if(c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceUtil(chars: List[Char], count: => Int): Boolean = {
      if(chars.isEmpty) count == 0
      else {
        val h = chars.head
        val c = if(h == '(') count + 1 else if(h == ')') count - 1 else count
        if(count >= 0) balanceUtil(chars.tail, c)
        else false
      }
    }

    balanceUtil(chars, 0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeUtil(money: Int, coins: List[Int]): Int = {
      if(money < 0 || coins.isEmpty) 0
      else if(money == 0) 1
      else countChangeUtil(money, coins.tail) + countChangeUtil(money-coins.head, coins)
    }

    countChangeUtil(money, coins)
  }
}
