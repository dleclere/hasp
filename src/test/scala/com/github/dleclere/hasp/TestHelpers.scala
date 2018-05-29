package com.github.dleclere.hasp

import com.github.dleclere.hasp.Domain.{Story, StoryId}

object TestHelpers {

  def story[T](storyId: T, stories: Seq[Story])(implicit caster: IdCaster[T, StoryId]): Option[Story] =
    stories.find(_.id == caster.cast(storyId))

}
