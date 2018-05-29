package com.github.dleclere.hasp.gridopt


import io.circe.generic.JsonCodec
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.generic.extras._

trait ResourceOps {

  def periodToComplete(work: Work, capacity: WorkTypeTag Map WorkAmount): Option[Period] =
    capacity
      .get(work.tag)
      .map(_.value)
      .map(_.toFloat)
      .map(work.amount.value.toFloat / _)
      .map(amount => Period(Math.ceil(amount).toInt))

  def periodToComplete(work: Work, resource: Resource): Option[Period] =
    periodToComplete(work, resource.execRatePerPeriod.capacity)

  def periodsToComplete(work: Seq[Work], resource: Resource): Option[Period] =
    work.map(periodToComplete(_, resource)).reduce((a, b) => a.flatMap(b map add(_)))


  // Should this be an option?
  def deductCapacity(execRate: ExecRatePerPeriod, work: Work): (ExecRatePerPeriod, Option[Work]) = {
    val remainingCapacity = execRate.capacity(work.tag).value - work.amount.value
    (
      execRate.copy(
        capacity = execRate.capacity + (work.tag -> WorkAmount(Math.max(0, remainingCapacity)))
      ),
      Option(remainingCapacity)
        .map(Math.abs)
        .filter(_ > 0)
        .map(WorkAmount(_))
        .map(amount => work.copy(amount = amount))
    )
  }

  def deductCapacity(resource: Resource, work: Work): (Resource, Option[Work]) = {
    val (rate, remainder) = deductCapacity(resource.execRatePerPeriod, work)
    (resource.copy(execRatePerPeriod = rate), remainder)
  }

  @inline
  def isResourceCapable(resource: Resource, work: Work): Boolean =
    resource.execRatePerPeriod.capacity.keys.exists(_ == work.tag)

  def isResourceCapable(resource: Resource, work: Seq[Work]): Boolean =
    work.forall(isResourceCapable(resource, _))


}
