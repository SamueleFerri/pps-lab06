package it.unibo.pps.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if (msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance: Unit =

  /* Linear sequences: List, ListBuffer */
  val list = List(1, 2, 3, 4, 5)
  val secondElement = list(1)
  val newList = list.updated(1, 99)
  val deleteElementList = list.filter(element => element % 2 == 0)

  val listBuffer = ListBuffer[Int](1, 2, 3, 4, 5)
  val thirdElement = listBuffer(2)
  listBuffer(2) = 99
  listBuffer.filterInPlace(element => element % 3 == 0)
  listBuffer.remove(0)


  /* Indexed sequences: Vector, Array, ArrayBuffer */

  /* Sets */

  /* Maps */

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("list last")(lst.last) > measure("vec last")(vec.last))
