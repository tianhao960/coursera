package recfun
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
  def pascal(c: Int, r: Int): Int = if(c==0 || c==r ) 1 else pascal(c-1,r-1) + pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if(chars.isEmpty) {
        true
      }else{
        val count = checkBalance(0,chars)
        count==0 
      }
  }

  def checkBalance(count:Int,chars: List[Char]):Int = {
    if(count<0 || chars.isEmpty){
      count
    }else{
      val char = chars.head
      if(char=='('){
        checkBalance(count+1,chars.tail)
      }else if(char==')'){
        checkBalance(count-1,chars.tail)
      }else{
        checkBalance(count,chars.tail)
      }
    }
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money<0){
      0
    }else if(money==0){
      1
    }else if(money>0 && coins.isEmpty){
      0
    }else{
      val head = coins.head
      val tail = coins.tail

      countChangeTail(head,money,tail);
    }
  }

  def countChangeTail(coin:Int,money:Int,coins:List[Int]):Int={
    if(money<0){
      0
    }else if(money==0){
      1
    }else{
      def count = for(i<-0 to money/coin) yield  countChange(money-i*coin,coins)
      count.sum
    }
  }
}
