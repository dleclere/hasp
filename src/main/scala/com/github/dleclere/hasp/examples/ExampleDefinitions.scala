package com.github.dleclere.hasp.examples

import com.github.dleclere.hasp._
import com.github.dleclere.hasp.Domain._
import com.github.dleclere.hasp.backlog.SimpleBacklog
import com.github.dleclere.hasp.examples.ExampleDescriptor.DescriptorBuilder



object ExampleDefinitions {

  val init1: DescriptorBuilder =
    init(
      5.sp,
      Map(
        WorkTypeTag("frontend") -> 0.5,
        WorkTypeTag("backend") -> 0.5
      )
    )

  val exampleConfig1 = ScrumProjectConfig(
    initialVelocityEstimate = 10.sp,
    daysPerSprint = 10,
    estimateAccuracyProbability = 0.5,
    estimateVarianceRange = 5
  )

  val alice =
    TeamMemberConfig(
      name = TeamMemberId("Alice"),
      velocityMultiplierByTag =
        Map(
          WorkTypeTag("frontend") -> 0.5,
          WorkTypeTag("backend") -> 1.0
        )
    )

  val bob =
    TeamMemberConfig(
      name = TeamMemberId("Bob"),
      velocityMultiplierByTag =
        Map(
          WorkTypeTag("frontend") -> 1.0,
          WorkTypeTag("backend") -> 0.5
        )
    )

  val stories1: Seq[Story] =
    storiesWithDeps {
      init1 :|
      1 :~> 2 :|
      2 :~> (3, 4) :|
      3 :& 3.sp :& ("frontend".wt -> 0.3, "backend".wt -> 0.7) :~> 5 :|
      4 :~> 5 :|
      5
    }

  val example1 = ScrumProject(
    config =
      exampleConfig1,
    teamMembers =
      Seq(alice, bob).map(m => m.name -> m).toMap,
    stories =
      stories1.map(s => s.id -> s).toMap,
    tasks =
      Map.empty,
    backlog =
      SimpleBacklog(stories1.map(_.id).map(Right(_))),
  )

}



