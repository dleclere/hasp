package com.github.dleclere.hasp.gridopt.genetics

import com.github.dleclere.hasp.algorithms.scheduling.ccf.CCF
import com.github.dleclere.hasp.genetics.base.GenChromosome
import com.github.dleclere.hasp.genetics.random.RandomNumberProvider
import com.github.dleclere.hasp.gridopt.{Period, Resource, ResourceId, Task, TaskId}

class GeneticContext(
  val scheduleName: ScheduleName,
  val tasks: TaskId Map Task,
  val resources: ResourceId Map Resource,
  val latestFinish: Period = 100000
) {


  implicit object CCFMGenerator extends GenChromosome[ScheduleLike, ScheduledTaskLike[TaskId]] {

    def apply()(implicit rand: RandomNumberProvider): Option[ScheduleLike[ScheduledTaskLike[TaskId]]] = {
      implicit val ccfOps: GridGeneticsCCFScheduleOps = new GridGeneticsCCFScheduleOps(
        genScheduledTask(_, _, resources, latestFinish, None)
      )

      CCF(Schedule(name = scheduleName, tasks = tasks, resources = resources))
    }


  }

}