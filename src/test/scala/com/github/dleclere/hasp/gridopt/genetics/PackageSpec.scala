package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.gridopt.Period
import org.scalatest.{FlatSpec, Matchers}
import com.github.dleclere.hasp.gridopt.genetics._

import scala.collection.SortedSet
import scala.collection.immutable.BitSet

class PackageSpec extends FlatSpec with Matchers {
//
//  def noAllocationIntersection(range: Option[(Period, Period)], allocated: SortedSet[(Int, Int)]): Unit =
//    range
//      .map { case (Period(start), Period(end)) =>
//        BitSet(start to end: _*)
//      }
//      .foreach { range =>
//        (range & allocated) should be(empty)
//      }

  "A task starting at the end of the last allocated period" should "go to the end" in {
    val allocated = SortedSet((0, 5))
    val range = firstAvailablePeriodRange(
      allocated = allocated,
      window = 3,
      earliestStart = 0,
      latestFinish = 20
    )
    range should not be (empty)
    //noAllocationIntersection(range, allocated)
    range.get._1 should be (Period(6))
  }

  "A task starting between two allocation periods" should "go in between them" in {
    val allocated = SortedSet((0, 5), (10, 20))
    val range = firstAvailablePeriodRange(
      allocated = allocated,
      window = 3,
      earliestStart = 0,
      latestFinish = 20
    )
    range should not be (empty)
    //noAllocationIntersection(range, allocated)
  }


  "A task starting before the first allocated period" should "go to the front" in {
    val allocated = SortedSet((10, 15))
    val range = firstAvailablePeriodRange(
      allocated = allocated,
      window = 3,
      earliestStart = 0,
      latestFinish = 20
    )
    range should not be (empty)
    //noAllocationIntersection(range, allocated)
  }

  "A task overlapping possible periods" should "not be allocated" in {
    val allocated = SortedSet((0, 15))
    val range = firstAvailablePeriodRange(
      allocated = allocated,
      window = 3,
      earliestStart = 4,
      latestFinish = 10
    )
    range should be (empty)
  }

  "A task" should "be allocated at or after its earliest start" in {
    val allocated = SortedSet((10, 20))
    val earliestStart = 25
    val latestFinish = 30
    val range = firstAvailablePeriodRange(
      allocated = allocated,
      window = 3,
      earliestStart = earliestStart,
      latestFinish = latestFinish
    )
    assert(range.map(_._1.num).exists(_ == earliestStart))
  }

  "A task" should "be allocated at or before its latest finish" in {
    val allocated = SortedSet((0, 40))
    val earliestStart = 0
    val latestFinish = 30
    val range = firstAvailablePeriodRange(
      allocated = allocated,
      window = 3,
      earliestStart = earliestStart,
      latestFinish = latestFinish
    )
    range should be (empty)
  }

}
