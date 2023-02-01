package com.bank

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.collection.IndexedSeqView
import scala.collection.SeqView
import scala.collection.mutable.{Map => MMap, Seq => MSeq}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool

/** Hills and Steele implementation
  * You can control of paralellism level with explicitly defined taskSupport. But use default which
  * actually adapts paralellism level to number of available processors.
  */
object HillsAndSteeleScan extends App {
  // val taskSupport = new ForkJoinTaskSupport(new ForkJoinPool(6))
  def scan(input: SeqView[Int]): Seq[Int] = {
    val n = input.size
    def inner(i: Int, previous: Map[Int, Int]): Map[Int, Int] = {
      val parRange = (0 to n - 1).par
      // parRange.tasksupport = taskSupport
      parRange.foldLeft(Map.empty[Int, Int]) { (z, j) =>
        val iTo2 = math.pow(2, i)
        if (j < math.pow(2, i))
          z + (j -> previous(j))
        else z + (j -> previous(j).max(previous(j - iTo2.toInt)))
      }
    }
    @tailrec
    def outer(i: Int, previous: Map[Int, Int]): Map[Int, Int] = {
      val newMap = inner(i, previous)
      if (i > math.ceil(math.log(n) / math.log(2)).toInt - 1)
        newMap
      else
        outer(i + 1, newMap)
    }
    val outMap = outer(0, input.zipWithIndex.map { case (value, index) => (index -> value) }.toMap)
    val output = MSeq.fill(n)(0)
    outMap.par.foreach { case (index, value) =>
      output(index) = value
    }
    output.toSeq
  }

  val input = Array[Int](0, 0, 1, 5, 2, 3, 6, 1, 2, 4, 3, 10, 0).view

  println(s"Seq implementation test: ${Scan.sequentialScan(input.map(_.toFloat)).toList}")
  println(s"Par implementation test: ${scan(input).toList}")

  val bigInput = Array.fill(1000000) { (math.random * 100000).toInt }.view

  import java.time

  val seqStart = time.LocalTime.now()
  val seq = Scan.sequentialScan(bigInput.map(_.toFloat))
  val seqEnd = time.LocalTime.now()
  println(s"Seq implementation in ms: ${(seqEnd.toNanoOfDay - seqStart.toNanoOfDay()) / 1000000}")
  println(s"last 10 of seq: ${seq.takeRight(10).toList}")
  // prints >> Seq implementation in ms: 228644

  val parStart = time.LocalTime.now()
  val par = scan(bigInput)
  val parEnd = time.LocalTime.now()
  println(s"Par implementation in ms: ${(parEnd.toNanoOfDay - parStart.toNanoOfDay()) / 1000000}")
  println(s"last 10 of par: ${par.takeRight(10).toList}")
  // prints >> Par implementation in ms: 31667
}