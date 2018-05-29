package com.github.dleclere.hasp.app.converters

import com.github.dleclere.hasp.gridopt._
import com.github.dleclere.hasp.jira.models._

object JiraIssues2GridTasks {

  def apply(issues: Seq[JiraIssue]): TaskId Map Task =
    TaskBuilder(
      issues.map { issue =>
        val successors = {
          issue.issueLinks
            .filter(_.sourceIssueId == issue.id)
            .map(_.destinationIssueId) ++ issue.parent.toSeq
        }.map(_.id).map(TaskId(_)).map(TaskSuccessor(_, 0))
          val work = issue.estimation
            .map(_ * 100f)
            .orElse(Option(100f))
            .toSeq
            .flatMap { estimation =>
              if (issue.labels.nonEmpty)
                issue.labels
                  .map(WorkTypeTag(_))
                  .map(Work(_, WorkAmount((estimation / issue.labels.length.toFloat).round)))
              else
                Seq(Work(WorkTypeTag("general"), WorkAmount(estimation.toInt)))

            }
          val id = TaskId(issue.id.id)
          (id, Task(id, work, successors, _: Seq[TaskId]), successors)
        } :_*
    ).map(task => task.taskId -> task).toMap

}
