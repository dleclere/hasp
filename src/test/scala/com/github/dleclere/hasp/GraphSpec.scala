package com.github.dleclere.hasp

import org.scalatest._
import com.github.dleclere.hasp._
import com.github.dleclere.hasp.Domain._
import com.github.dleclere.hasp.backlog.SimpleBacklog
import com.github.dleclere.hasp.examples.ExampleDescriptor.DescriptorBuilder
import com.github.dleclere.hasp.examples._
import TestHelpers._
import com.github.dleclere.hasp.examples.ExampleDefinitions.stories1
import org.scalatest.Matchers._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

class GraphSpec extends FlatSpec with Matchers  {

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

  val storiesBuilder1: ExampleDescriptor.StoryDepsDescriptorBuilder =
    init1 :|
    1 :~> 2 :|
    2 :~> (3, 4) :|
    3 :& 3.sp :& ("frontend".wt -> 0.3, "backend".wt -> 0.7) :~> 5 :|
    4 :~> 5 :|
    5

  val stories1: Seq[Story] =
    storiesWithDeps(storiesBuilder1)

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
      SimpleBacklog(stories1.map(_.id).map(Right(_)))
  )

  "A set of stories" should "match graph dependencies" in {
    story(1, stories1).map(_.dependents) should contain (Seq(2.sid))
    story(2, stories1).map(_.precedents) should contain (Seq(1.sid))
    story(2, stories1).map(_.dependents) should contain (Seq(3.sid, 4.sid))
    story(3, stories1).map(_.precedents) should contain (Seq(2.sid))
    story(3, stories1).map(_.dependents) should contain (Seq(5.sid))
    story(4, stories1).map(_.precedents) should contain (Seq(2.sid))
    story(4, stories1).map(_.dependents) should contain (Seq(5.sid))
    story(5, stories1).map(_.precedents.toSet) should contain (Set(3.sid, 4.sid))
    storiesBuilder1.graph should be (graph(stories1))
  }

}