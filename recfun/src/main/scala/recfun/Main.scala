package recfun

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
    factorial(r)/(factorial(c)*factorial(r-c))
  }

  def factorial(n: Int): Int = {
    def loop(acc:Int, n: Int):Int = {
      if(n == 0) acc
      else loop(acc*n,n-1)
    }
    loop(1,n)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def loop(c: Int, s:List[Char]): Boolean = {
        if(s.isEmpty) c == 0
        else if (c < 0) false
        else if (s.head == '(') loop(c+1, s.tail)
        else if (s.head == ')') loop(c-1, s.tail)
        else loop(c, s.tail)
      }
      loop(0, chars)
    }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0)
      1
    else if(money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
      0
  }

  }
