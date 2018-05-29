package com.github.dleclere.hasp.gridopt
import com.github.dleclere.hasp.utils._

object ScheduleUtils extends ResourceOps {

  def estWorstFinish(tasks: Iterable[Task], resources: Iterable[Resource]): Either[Throwable, Period] = {
    def mergeMap(a: WorkTypeTag Map WorkAmount, b: WorkTypeTag Map WorkAmount): WorkTypeTag Map WorkAmount =
      (a.keySet ++ b.keySet)
        .map(k => k -> (a.get(k) ++ b.get(k)).minBy(_.value))
        .toMap

    resources
      .map(_.execRatePerPeriod.capacity)
      .reduceOption(mergeMap).flatMap { worstCaseWork =>
        liftOptionSeq {
          tasks
            .map(_.work)
            .map(_.map(periodToComplete(_, worstCaseWork)))
            .map(liftOptionSeq)
            .map(_.map(_.map(_.num).sum))
        }.map(_.sum).map(Period)
      }.fold[Either[Throwable, Period]](Left(new Error("Resources insufficient to complete available work.")))(Right(_))
  }

}
