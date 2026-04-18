package it.unibo.pps.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
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
  val list = (1 to 5).toList
  val secondElement = list(1)
  val newList = list.updated(1, 99)
  val deleteElementList = list.filter(element => element % 2 == 0)

  val listBuffer = (1 to 5).to(ListBuffer)
  val thirdElement = listBuffer(2)
  listBuffer(2) = 99
  listBuffer.filterInPlace(element => element % 3 == 0)
  listBuffer.remove(0)

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val vector = (1 to 5).toVector
  val firstElement = vector(0)
  val newVector = vector.updated(0, 99)
  val deleteVectorElement = vector.filter(element => element % 2 == 0)
  val deleteIndexTwo = vector.take(2) ++ vector.drop(3)

  val array = (1 to 5).toArray
  var newArray = array.updated(2, 99)
  val deleteElementArray = array.filter(element => element % 3 == 0)

  val arrayBuffer = (1 to 5).to(ArrayBuffer)
  arrayBuffer(2) = 99
  arrayBuffer.filterInPlace(element => element % 3 == 0)
  arrayBuffer.remove(0)


  /* Sets */

  /* Maps */

  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("list last")(lst.last) > measure("vec last")(vec.last))
