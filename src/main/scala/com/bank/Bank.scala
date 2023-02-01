package com.bank

import scala.util.DynamicVariable
import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.Executors
import scala.collection.IndexedSeqView
import scala.collection.SeqView
import scala.collection.mutable.{Map => MMap, Seq => MSeq}
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool

/** Familiarize yourself with parallel partial reduction trees:
  *
  * https://www.sciencedirect.com/topics/computer-science/partial-reduction
  *
  * In this exercise, your task is to scan left on a given input array and output a new array consisting of the maximum
  * values to the left of a given index in the original array.
  *
  * Example: In: [0, 0, 1, 5, 2, 3, 6] Out: [0, 0, 1, 5, 5, 5, 6]
  *
  * *** Task 1 *** Using the partial parallel reduction tree paradigm, implement the methods described below, making use
  * of the parallelism abstractions provided. You should not utilize data or function parallel constructs that have not
  * been provided.
  *
  * Utilize good judgment when choosing side-effecting vs pure implementations to blend good functional style with
  * performance.
  *
  * Note that a trivial sequential implementation has been provided for reference.
  *
  * *** Task 2 *** Using scalameter, compare performance between a fully sequential implementation and your parallel
  * implementation, and provide some recommendations for optimal threshold values for your system.
  *
  * *** Task 3 *** Using your observations from Task 2, extrapolate to general systems.
  */

class Tree {
  def max(input: IndexedSeqView[Float])(implicit ec: ExecutionContext): Future[Float] = ???
}
case class Node(left: Tree, right: Tree) extends Tree {
  override def max(input: IndexedSeqView[Float])(implicit ec: ExecutionContext) =
    left.max(input).flatMap { lMax => right.max(input).map(rMax => lMax.max(rMax)) }
}
case class Leaf(from: Int, until: Int) extends Tree {
  override def max(input: IndexedSeqView[Float])(implicit ec: ExecutionContext) = Future {
    input.slice(from, until).max
  }
}

object Tree {
  def construct(from: Int, until: Int, threshold: Int): Tree =
    if (until - from - 1 <= threshold)
      Leaf(from, until)
    else {
      Node(
        construct(from, from + ((until - from - 1) / 2).toInt, threshold),
        construct(from + ((until - from - 1) / 2).toInt, until, threshold)
      )
    }
}

trait ScanInterface {
  def sequentialScan(input: IndexedSeqView[Float]): Array[Float]
  def sequentialUpsweep(input: Array[Float]): Float
  def upsweep(input: IndexedSeqView[Float], threshold: Int): Tree
  def downsweep(input: IndexedSeqView[Float], tree: Tree)(implicit ec: ExecutionContext): Future[Float]
  def scan(input: IndexedSeqView[Float], threshold: Int)(implicit ec: ExecutionContext): Array[Float]
}

object Scan extends ScanInterface {

  def sequentialScan(input: IndexedSeqView[Float]): Array[Float] =
    input.drop(1).foldLeft(Array(input(0))) { (z, v) => z :+ v.max(z.last) }

  /** Input: the given part of the array and returns the maximum value. from - inclusive until - non-inclusive
    */
  def sequentialUpsweep(input: Array[Float]): Float = input.max

  /** Traverses the part of the array starting at `from` and until `until`, and returns the reduction tree for that part
    * of the array.
    *
    * The reduction tree is a `Tree.Leaf` if the length of the specified part of the array is smaller or equal to
    * `threshold`, and a `Tree.Node` otherwise. If the specified part of the array is longer than `threshold`, then the
    * work is divided and done recursively in parallel.
    */
  def upsweep(input: IndexedSeqView[Float], threshold: Int): Tree = Tree.construct(0, input.size, threshold)

  /** Pushes the maximum value in the prefix of the array to each leaf of the reduction `tree` in parallel, and then
    * calls `downsweepSequential` to write the `output` values.
    */
  def downsweep(input: IndexedSeqView[Float], tree: Tree)(implicit ec: ExecutionContext): Future[Float] =
    tree.max(input)

  /** Compute the ray shadowing buffer in parallel. */
  def scan(input: IndexedSeqView[Float], threshold: Int)(implicit ec: ExecutionContext): Array[Float] = {
    val inputView = input.view
    Await
      .result(
        Future
          .sequence((1 until input.size + 1).map { untilIndex =>
            val slice = inputView.slice(0, untilIndex)
            Future(upsweep(slice, threshold)).flatMap { tree =>
              downsweep(slice, tree)
            }
          }),
        Duration.Inf
      )
      .toArray
  }
}

object Run extends App {
  import Scan._
  val input = Array[Float](0, 0, 1, 5, 2, 3, 6, 1, 2, 4, 3, 10, 0).view

  println(s"Seq implementation test: ${sequentialScan(input).toList}")

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))

  upsweep(input, 2).max(input).onComplete(r => println(r.get))

  println(s"Par implementation test: ${scan(input, 3).toList}")

  val bigInput = Array.fill(100000) { math.random.toFloat }.view

  import java.time

  val seqStart = time.LocalTime.now()
  val seq = sequentialScan(bigInput)
  val seqEnd = time.LocalTime.now()
  println(s"Seq implementation in ns: ${seqEnd.toNanoOfDay - seqStart.toNanoOfDay}")
  println(s"last 10 of seq: ${seq.takeRight(10).toList}")

  val parStart = time.LocalTime.now()
  val par = scan(bigInput, 1000)
  val parEnd = time.LocalTime.now()
  println(s"Par implementation in ns: ${parEnd.toNanoOfDay - parStart.toNanoOfDay}")
  println(s"last 10 of par: ${par.takeRight(10).toList}")

  ec.shutdownNow()
}


/** Vishkin schema
  */
case class VData(startIdx: Int, endIdx: Int, value: Int)
trait VTree {
  def max(input: SeqView[Int], output: Seq[VData])(implicit ec: ExecutionContext): Future[Seq[VData]] = ???
}
case class VLeaf(startIdx: Int, endIdx: Int) extends VTree {
  override def max(input: SeqView[Int], output: Seq[VData])(implicit ec: ExecutionContext): Future[Seq[VData]] =
    Future {
      val data = input.slice(startIdx, endIdx + 1) match {
        case slice if slice.isEmpty   => VData(startIdx, endIdx, Int.MinValue)
        case slice if slice.size == 1 => VData(startIdx, endIdx, slice.head)
        case slice                    => VData(startIdx, endIdx, slice.max)
      }
      output :+ data
    }
}
case class VNode(startIdx: Int, endIdx: Int, left: VTree, right: VTree) extends VTree {
  override def max(input: SeqView[Int], output: Seq[VData])(implicit ec: ExecutionContext): Future[Seq[VData]] =
    left.max(input, output).flatMap { lSeq =>
      right.max(input, output).map { rSeq =>
        val both = lSeq ++ rSeq
        both :+ VData(startIdx, endIdx, both.maxBy(_.value).value)
      }
    }
}

object VishkinSchema {
  lazy val powsOf2: LazyList[Int] = {
    def pow0f2(radix: Int): LazyList[Int] = (math.pow(2, radix).toInt - 1) #:: pow0f2(radix + 1)
    pow0f2(0)
  }
  def precompute(input: SeqView[Int])(implicit ec: ExecutionContext): Future[Seq[VData]] = {
    val n = powsOf2.dropWhile(_ < input.size - 1).head
    def rec(startIdx: Int, endIdx: Int): VTree = (endIdx - startIdx) match {
      case 1 => VLeaf(startIdx, endIdx)
      case a if a > 1 =>
        VNode(
          startIdx,
          endIdx,
          rec(startIdx, startIdx + ((endIdx - startIdx) / 2).toInt),
          rec(startIdx + ((endIdx - startIdx) / 2).toInt + 1, endIdx)
        )
    }
    Future(rec(0, n)).flatMap { tree =>
      tree.max(input, Seq.empty[VData])
    }
  }

  def toMap(tree: Future[Seq[VData]])(implicit ec: ExecutionContext) = tree.flatMap { seq =>
    import scala.collection.mutable.Map
    val map = Map.empty[Int, (Int, Int)]
    seq.foreach { data =>
      map.contains(data.endIdx) match {
        case true =>
          val (start, value) = map(data.endIdx)
          if (start > data.startIdx)
            map(data.endIdx) = (data.startIdx, data.value)
        case false => map(data.endIdx) = (data.startIdx, data.value)
      }
    }
    Future(map)
  }
}

object VishkinRun extends App {
  println(VishkinSchema.powsOf2.take(10).toList)
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(6))
  println(
    Await.result(VishkinSchema.toMap(VishkinSchema.precompute(List(1, 2, 3, 4, 5, 6, 7, 8, 9).view)), Duration.Inf)
  )
  ec.shutdownNow()
}
