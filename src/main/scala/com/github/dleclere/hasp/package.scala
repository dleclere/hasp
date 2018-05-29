package com.github.dleclere

import com.github.dleclere.hasp.Domain.{StoryId, StoryPoints, TaskId, WorkTypeTag}

package object hasp extends GraphHelpers {

  implicit class IntCaster(num: Int) {

    def sp: StoryPoints = StoryPoints(num)

    def sid: StoryId = StoryId(num.toString)

    def tid: TaskId = TaskId(num.toString)

  }


  implicit class StrCaster(str: String) {

    def sid: StoryId = StoryId(str)

    def tid: TaskId = TaskId(str)

    def wt: WorkTypeTag = WorkTypeTag(str)

  }

  sealed trait IdCaster[T, Id] {

    def cast(id: T): Id

  }

  implicit object IntStoryIdCaster extends IdCaster[Int, StoryId] {

    def cast(id: Int): StoryId =
      StoryId(id.toString)

  }

  implicit object IdentityStoryIdCaster extends IdCaster[StoryId, StoryId] {

    def cast(id: StoryId): StoryId =
      id

  }

  implicit class StoryPointOps(pts: StoryPoints) {


    def +(p2: StoryPoints): StoryPoints =
      StoryPoints(pts.points + p2.points)


    def -(p2: StoryPoints): StoryPoints =
      StoryPoints(pts.points - p2.points)

  }

}
