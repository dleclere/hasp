package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.algorithms.scheduling.ccf.CCF
import com.github.dleclere.hasp.genetics.base.{ChromosomeUtils, RandomMutation}
import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
import com.github.dleclere.hasp.gridopt.{Period, Task, TaskId}

object MutateScheduledTask extends RandomMutation[ScheduleLike, ScheduledTaskLike[TaskId]] {


  override def randomMutate(schedule: ScheduleLike[ScheduledTaskLike[TaskId]], target: Int)(implicit utils: ChromosomeUtils[ScheduleLike], gen: RandomNumberProvider): Option[ScheduleLike[ScheduledTaskLike[TaskId]]] = {
    import schedule._
    val scheduledTask = genes(target)
    val dependents = taskSuccessors(scheduledTask.id, tasks)
    val removals = dependents.distinct
    val removedDependents = removals.foldLeft(unscheduleTask(scheduledTask.id, schedule))((s, id) => unscheduleTask(id, s))
    val latestFinish = Period(genes.map(_.end).maxBy(_.num).num * 8)

    implicit val ccfOps: GridGeneticsCCFScheduleOps = new GridGeneticsCCFScheduleOps(
      (task: Task, schedule: ScheduleLike[ScheduledTaskLike[TaskId]]) => {
        def prefRes =
          if (task.taskId == scheduledTask.id) {
            None
          } else {
            Some(taskSchedule(task.taskId).resource)
          }
        genScheduledTask(task, schedule, resources, latestFinish, prefRes)
      }
    )


    CCF(removedDependents)

  }

}
