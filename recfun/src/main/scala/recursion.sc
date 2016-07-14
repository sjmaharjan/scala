object recursion {
  def pascal(c: Int, r: Int): Int =
    if(c == 0 || c==r ) 1 else pascal(c-1,r-1)+pascal(c,r-1)
                                                  //> pascal: (c: Int, r: Int)Int
     def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }                                               //> main: (args: Array[String])Unit
 pascal(1,3)                                      //> res0: Int = 3
 main(Array("afasfsa"))                           //> Pascal's Triangle
                                                  //| 1 
                                                  //| 1 1 
                                                  //| 1 2 1 
                                                  //| 1 3 3 1 
                                                  //| 1 4 6 4 1 
                                                  //| 1 5 10 10 5 1 
                                                  //| 1 6 15 20 15 6 1 
                                                  //| 1 7 21 35 35 21 7 1 
                                                  //| 1 8 28 56 70 56 28 8 1 
                                                  //| 1 9 36 84 126 126 84 36 9 1 
                                                  //| 1 10 45 120 210 252 210 120 45 10 1 
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], count: Int): Boolean =
      if (chars.isEmpty)
        if (count == 0) return true else false
      else if (chars.head == '(')
        loop(chars.tail, count + 1)
      else if (chars.head == ')')
        loop(chars.tail, count - 1)
      else
        loop(chars.tail, count)
      
      loop(chars,0)
  }                                               //> balance: (chars: List[Char])Boolean

balance("(if (zero? x) max (/ 1 x))".toList)      //> res1: Boolean = true
 def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0) 1
    else if ((money < 0) || (coins.isEmpty)) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)

  }                                               //> countChange: (money: Int, coins: List[Int])Int
  countChange(100,List(1,5,10,25,50))             //> res2: Int = 292

}