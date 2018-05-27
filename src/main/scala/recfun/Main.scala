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

    def fac(n: Int): Int = {
      def fact(n: Int, acc: Int): Int = n match {
        case 0 | 1 => acc
        case _ => fact(n - 1, n * acc)
      }

      fact(n, 1)
    }

    fac(r) / (fac(r - c) * fac(c))
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def bal(chars: List[Char], openingPara: Int): Boolean = {
      if (chars.isEmpty) {
        if (openingPara != 0) false else true
      } else {
        if (chars.head == '(') {
          bal(chars.tail, openingPara + 1)
        } else if (chars.head == ')') {
          if (openingPara == 0) false else bal(chars.tail, openingPara - 1)
        } else {
          bal(chars.tail, openingPara)
        }
      }
    }

    if (chars.isEmpty) true else bal(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money ==0){
      1
    }
    else if(money<0 || coins.isEmpty){
      0
    }else {
      countChange(money - coins.head,coins)+countChange(money,coins.tail)
    }
  }
}
