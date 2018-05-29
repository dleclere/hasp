package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.algorithms.scheduling.ccf.CCF
import com.github.dleclere.hasp.genetics.base.{ChromosomeUtils, CrossoverMutation}
import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
import com.github.dleclere.hasp.gridopt.{Period, Task, TaskId}

object CrossoverScheduledTask extends CrossoverMutation[ScheduleLike, ScheduledTaskLike[TaskId]] {

  override def crossoverMutate(src: ScheduledTaskLike[TaskId], dest: ScheduleLike[ScheduledTaskLike[TaskId]], target: Int)(implicit utils: ChromosomeUtils[ScheduleLike], gen: RandomNumberProvider): Option[ScheduleLike[ScheduledTaskLike[TaskId]]] = {
    import dest._
    val scheduledTask = genes(target)
    if (src != scheduledTask) {
      val span = periodRange(src.start, src.end)
//      val resourceConflicts =
//        if (utilisingResourceWithin(dest, src.resource, src.start, src.end).isEmpty) {
//          Seq.empty
//        } else {
//          dest.genes.collect {
//            case task@ScheduledTask(id, resource, start, end) if id != src.id && resource == scheduledTask.resource && (periodRange(start, end) & span).nonEmpty =>
//              task
//          }
//        }
      val dependents = taskSuccessors(scheduledTask.id, tasks)
      val toRemove = scheduledTask.id +: dependents
      val removals = toRemove
      val removedDependents = removals.foldLeft(dest)((s, id) => unscheduleTask(id, s))
      val latestFinish = Period(genes.map(_.end).maxBy(_.num).num * 8)
      implicit val ccfOps: GridGeneticsCCFScheduleOps = new GridGeneticsCCFScheduleOps(
        (task: Task, schedule: ScheduleLike[ScheduledTaskLike[TaskId]]) => {
          val prefRes =
            if (task.taskId == src.id) {
              src.resource
            } else {
              taskSchedule(task.taskId).resource
            }
          genScheduledTask(task, schedule, resources, latestFinish, Some(prefRes))
        }
      )
      CCF(unscheduleTask(scheduledTask.id, removedDependents))
    } else {
      Some(dest)
    }
  }

}