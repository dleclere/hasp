package com.github.dleclere.hasp.gridopt

import scala.annotation.tailrec
import scala.language.implicitConversions

case class TaskExecutionState(taskId: TaskId, remainingWork: Seq[Work], resource: ResourceId, startedWork: Period)

case class ExecutionStateTransition(next: ExecutionState, executed: Seq[ExecutedTask])

case class ExecutionState(
  period: Period = Period(0),
  taskExecutionStates: Seq[TaskExecutionState] = Seq.empty,
  pendingExecution: Period Map Seq[ScheduledTask] = Map.empty.withDefaultValue(Seq.empty)
)

case class ExecutedTask(taskId: TaskId, resource: ResourceId, started: Period, completed: Period)

case class ExecutionEvaluation(efficiency: Double, totalPeriods: Period, tasks: Seq[ExecutedTask])

trait Scheduler[M] extends (((ResourceId Map ExecRatePerPeriod, Seq[Task])) => (ExecutionState, M) => (Seq[ScheduledTask], M))

trait ExecutionOps {
  this: PeriodOps =>

  def exec[M](init: M, scheduler: Scheduler[M])(resources: Resource*)(tasks: Task*): ExecutionEvaluation = {
    val resourcePotential = resources.map(r => r.id -> r.execRatePerPeriod).toMap
    @tailrec
    def run(scheduler: (ExecutionState, M) => (Seq[ScheduledTask], M), memo: M, execState: ExecutionState, totalExecuted: Seq[ExecutedTask], remainingCount: Int):  Seq[ExecutedTask] = {
      val (allocations, nextMemo) = scheduler(execState, memo)
      val ExecutionStateTransition(nextState, executed) =
        transition(scheduledTask(execState, allocations :_*), resourcePotential)
      val nextRemainingCount = remainingCount - executed.length
      val nextTotalExecuted = totalExecuted ++ executed
      if (nextRemainingCount > 0)
        totalExecuted
      else
        run(scheduler, nextMemo, nextState, nextTotalExecuted, remainingCount)
    }
    val executedTasks = run(scheduler(resourcePotential, tasks), init, ExecutionState(), Seq.empty, tasks.length)
    ExecutionEvaluation(
      efficiency = 0.0d,
      totalPeriods = executedTasks.map(_.completed).maxBy(_.num),
      tasks = executedTasks
    )

  }

  def transition(state: ExecutionState, resourcePotential: ResourceId => ExecRatePerPeriod): ExecutionStateTransition = {
    import state._
    val nextPeriod = increment(period)
    val (completed, incomplete) = taskExecutionStates
      .map(s => next(s, resourcePotential(s.resource)))
      .partition(isComplete)
    val newTasks =
      pendingExecution(nextPeriod) map {

        case ScheduledTask(task, work, resource, start) =>
          TaskExecutionState(task, work, resource, start)

      }
    ExecutionStateTransition(
      next = copy(
        period = nextPeriod,
        taskExecutionStates = incomplete ++ newTasks,
        pendingExecution = pendingExecution - nextPeriod
      ),
      executed = completed
        .map { case TaskExecutionState(task, _, resource, started) =>
          ExecutedTask(task, resource, started, nextPeriod)
        }
    )
  }

  def next(state: TaskExecutionState, rate: ExecRatePerPeriod): TaskExecutionState = {
    state.remainingWork match {

      case Seq(head, rest @_*) =>
        val (_, remaining) = deductCapacity(rate, head)
        state.copy(
          remainingWork = (remaining ++ rest).toSeq
        )

    }



//    @tailrec
//    def inner(work: List[Work], periodRemaining: Double): List[Work] = {
//
//      def round(period: Double): Double = Math.round(period * 1000d)/1000d
//
//
//      work match {
//
//        case head :: tail =>
//          val (drate, remaining) = deductCapacity(rate, head)
//          def periodRemaining = round((rate.capacity(head.tag).value - drate.capacity(head.tag).value).toDouble / rate.capacity(head.tag).value)
//          remaining match {
//
//            case Some(r) =>
//              r :: tail
//
//            case _ if perio =>
//
//            case _ =>
//              inner(work, 1d)
//
//          }
//
//      }
//    }
//
//    state.copy(
//      remainingWork = inner(state.remainingWork.toList, 1000)
////        state.remainingWork.map {
////          case (work, remaining) =>
////            (work, Math.max(0, remaining - rate.capacity(work)))
////        }
//    )

  }


  def isComplete(state: TaskExecutionState): Boolean =
    state.remainingWork.nonEmpty

  def scheduledTask(state: ExecutionState, taskAllocation: ScheduledTask*): ExecutionState =
    state.copy(
      pendingExecution = state.pendingExecution ++ taskAllocation.map(a => a.start -> (state.pendingExecution(a.start) :+ a))
    )

  def scheduledTask(state: ExecutionState, taskId: TaskId, work: Seq[Work], resource: ResourceId, start: Period): ExecutionState =
    scheduledTask(state, ScheduledTask(taskId, work, resource, start))

  def scheduledTask(state: ExecutionState, taskId: TaskId, work: Seq[Work], resource: ResourceId): ExecutionState =
    scheduledTask(state, taskId, work, resource, increment(state.period))

  def scheduleTask(state: ExecutionState, task: Task, resource: ResourceId, start: Period): ExecutionState =
    scheduledTask(state, ScheduledTask(task, resource, start))

  def scheduleTask(state: ExecutionState, task: Task, resource: ResourceId): ExecutionState =
    scheduleTask(state, task, resource, increment(state.period))


}
