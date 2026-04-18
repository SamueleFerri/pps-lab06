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

trait StringAccumulator:
  def add(text: String): Unit
  def getAll: Iterable[String]

class ImmutableStringAccumulator extends StringAccumulator:
  private var state: List[String] = List.empty

  //:+, append. each time return a new List. O(N)
  override def add(text: String): Unit =
    state = state :+ text

  override def getAll: Iterable[String] = state

class MutableStringAccumulator extends StringAccumulator:
  private val state: ListBuffer[String] = ListBuffer.empty

  //+= add the element at the end. O(1)
  override def add(text: String): Unit =
    state += text

  override def getAll: Iterable[String] = state

@main def checkPerformance(): Unit =

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
  val set = (1 to 5).toSet
  val elementFive = set(5)
  val newSet = set ++ (6 to 10).toSet
  val deleteSetElement = set - 2

  val mutSet = scala.collection.mutable.Set(1, 2, 3)
  mutSet.add(4)
  mutSet -= 4

  /* Maps */
  val map = Map[String, Int](
    "Pippo" -> 30,
    "Pluto" -> 40
  )
  val getPluto = map("Pluto")
  val newMap = map.updated("Pluto", 35)
  val newMap2 = map + ("Pluto" -> 35)
  val noPluto = map - "Pluto"

  val mutMap = scala.collection.mutable.Map[String, Int](
    "Pippo" -> 30,
    "Pluto" -> 40
  )
  mutMap("Pluto") = 35
  mutMap -= "Pluto"


  /* Comparison */
  import PerformanceUtils.*
  val lst = (1 to 10000000).toList
  val vec = (1 to 10000000).toVector
  assert(measure("list last")(lst.last) > measure("vec last")(vec.last))

  val insertions = 1 to 10000

  val listTime = measure("list prepend 10k") {
    var tempLst = List.empty[Int]
    for i <- insertions do tempLst = i :: tempLst
  }

  val vecTime = measure("vec prepend 10k") {
    var tempVec = Vector.empty[Int]
    for i <- insertions do tempVec = i +: tempVec
  }

  assert(measure("list add")(2 :: lst) < measure("vec add")(2 +: vec))

  val elementsToAdd = 1 to 50000

  println("Benchmark Accumulator")

  val immutableResult = measure("Immutable (List) Append") {
    val immutableAcc = ImmutableStringAccumulator()
    for i <- elementsToAdd do immutableAcc.add(s"Parola-$i")
  }

  val mutableResult = measure("Mutable (ListBuffer) Append") {
    val mutableAcc = MutableStringAccumulator()
    for i <- elementsToAdd do mutableAcc.add(s"Parola-$i")
  }

  assert(mutableResult < immutableResult)
  println("ListBuffer wins over List for continuous append!")