package com.bank

object A extends App {
  def secondMax(seq: List[Int]): Option[Int] = seq.distinct.sorted.takeRight(2) match {
    case seq if seq.size < 2 => None
    case seq                 => Some(seq(0))
  }

  def secondMax2(seq: List[Int]): Option[Int] = {
    def foo(seq: List[Int], acc: (Int, Int)): (Int, Int) = seq.distinct match {
      case Nil => acc
      case head :: tail =>
        val max1 = acc._1.max(acc._2)
        acc._1.max(head) match {
          case `max1` => foo(tail, (acc._1, acc._2.max(head)))
          case _      => foo(tail, (acc._2, acc._1.max(head)))
        }
    }
    val two = foo(seq, (Int.MinValue, Int.MinValue))
    two._1.min(two._2) match {
      case Int.MinValue if seq.distinct.size < 2 => None
      case second                                => Some(second)
    }
  }

  println(secondMax(Nil) == None)
  println(secondMax(List(1)) == None)
  println(secondMax(List(1, 1)) == None)
  println(secondMax(List(1, 2)) == Some(1))
  println(secondMax(List(2, 1)) == Some(1))
  println(secondMax(List(9, 3, 5)) == Some(5))
  println(secondMax(List(9, 3, 4, 3, 1)) == Some(4))

  println(secondMax2(Nil) == None)
  println(secondMax2(List(1)) == None)
  println(secondMax2(List(1, 1)) == None)
  println(secondMax2(List(1, 2)) == Some(1))
  println(secondMax2(List(2, 1)) == Some(1))
  println(secondMax2(List(9, 3, 5)) == Some(5))
  println(secondMax2(List(9, 3, 4, 3, 1)) == Some(4))

}
