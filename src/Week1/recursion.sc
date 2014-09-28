package Week1

object recursion {
  /**
   * Pascals Triangle
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) return 1
    else if (c < 0 || c > r) return 0
    else
      return pascal(c - 1, r - 1) + pascal(c, r - 1)
  }                                               //> pascal: (c: Int, r: Int)Int

  println("Pascal's Triangle")                    //> Pascal's Triangle
  for (row <- 0 to 10) {
    for (col <- 0 to row)
      print(pascal(col, row) + " ")
    println()
  }                                               //> 1 
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

  /**
   * Parentheses Balancing
   */
  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty)
        return open == 0
      else if (chars.head == '(' && open != -1)
        return balanced(chars.tail, open + 1)
      else if (chars.head == ')')
        return balanced(chars.tail, open - 1)
      else
        return balanced(chars.tail, open)
    }
    if (chars.isEmpty)
      return false
    else
      balanced(chars, 0)
  }                                               //> balance: (chars: List[Char])Boolean

  println("Parentheses Balancing")                //> Parentheses Balancing
  balance("(if (zero? x) max (/ 1 x))".toList)    //> res0: Boolean = true
  balance("I told him (that it’s not (yet) done).".toList)
                                                  //> res1: Boolean = true
  balance("())(".toList)                          //> res2: Boolean = false

  /**
   * Counting Change
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(capacity: Int, changes: List[Int]): Int = {
      if (capacity == 0)
        return 1
      else if (capacity < 0)
        return 0
      else if (changes.isEmpty && capacity >= 1)
        return 0
      else
        return count(capacity, changes.tail) + count(capacity - changes.head, changes)
    }
    count(money, coins.sortWith(_.compareTo(_) < 0))
  }                                               //> countChange: (money: Int, coins: List[Int])Int

  println("Counting Change")                      //> Counting Change
  countChange(4, List(1, 2))                      //> res3: Int = 3
}