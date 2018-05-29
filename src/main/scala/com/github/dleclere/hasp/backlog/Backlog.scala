package com.github.dleclere.hasp.backlog

import com.github.dleclere.hasp.Domain._
import com.github.dleclere.hasp._


trait Backlog {

  def nextWorkRecommendation(dest: TeamMemberId, project: ScrumProject): Option[TaskOrStoryId]

  def withUpdatedWork(work: Either[Task, Story]): Backlog

  def withoutWork(id: TaskOrStoryId): Backlog

  def withWork(work: Either[Task, Story]): Backlog

}