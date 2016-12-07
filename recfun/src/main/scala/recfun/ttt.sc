def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, acc+f(a))
  }
  loop(a, 0)
}
sum(x => x*x)(1,5)
def prod(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, acc*f(a))
  }
  loop(a, 1)
}
def fact(n:Int) = prod(x=>x)(1,n)
fact(5)
def generalsum(opration:(Int,Int) => Int,neutral:Int)
              (f:Int => Int)(a:Int,b:Int):Int ={
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, opration(acc,f(a)))
  }
  loop(a, neutral)
}

import recfun.Main.countChange
countChange(4,List(1,2))