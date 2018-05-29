package com.github.dleclere.hasp

import com.github.dleclere.hasp.Domain.{Story, StoryId, StoryPoints, WorkTypeTag}
import com.github.dleclere.hasp.examples.ExampleDescriptor.{DescriptorBuilder, StoryDepsDescriptorBuilder}
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
package object examples {


  implicit class StoryDepBuilderOps(builder: StoryDepsDescriptorBuilder) {

    def :~>[T](storyId: T*)(implicit caster: IdCaster[T, StoryId]): StoryDepsDescriptorBuilder =
      builder.copy(
        graph =
          builder.graph ++
            storyId
              .map(caster.cast)
              .map(builder.currentStory ~> _)
              .toList
      )

    def :&(points: StoryPoints): StoryDepsDescriptorBuilder =
      builder.copy(
        noDepStories =
          builder.noDepStories.dropRight(1) :+
            builder.noDepStories.last
              .copy(
                points =
                  points
              )
      )

    def :&(workTypeFraction: (WorkTypeTag, Double)*): StoryDepsDescriptorBuilder = {
      val lastStory = builder.noDepStories.last
      builder.copy(
        noDepStories =
          builder.noDepStories.dropRight(1) :+
            lastStory
              .copy(
                workTypeFractions =
                  lastStory.workTypeFractions ++ workTypeFraction
              )
      )
    }

    def :&(workTypeFractions: Map[WorkTypeTag, Double]): StoryDepsDescriptorBuilder = {
      val lastStory = builder.noDepStories.last
      builder.copy(
        noDepStories =
          builder.noDepStories.dropRight(1) :+
            lastStory
              .copy(
                workTypeFractions =
                  lastStory.workTypeFractions ++ workTypeFractions
              )
      )
    }


  }

  implicit class BuilderOps(builder: ExampleDescriptor.Builder) {

    def :|[T](id: T, points: StoryPoints = builder.defaultStoryPoints, workTypeTag: Map[WorkTypeTag, Double] = builder.defaultWorkTypeFractions)(implicit caster: IdCaster[T, StoryId]): StoryDepsDescriptorBuilder =
      builder addStory Story(caster.cast(id), points, workTypeTag)

  }

  def storyDependencies(graph: Graph[StoryId, DiEdge]): (StoryId Map Seq[StoryId], StoryId Map Seq[StoryId]) = {
    val emptyMap =
      Map.empty[StoryId, Seq[StoryId]].withDefaultValue(Seq.empty)
    graph
      .edges
      .map(e => e.source.value -> e.target.value)
      .foldLeft(emptyMap -> emptyMap) {
        case ((p, d), (src, target)) =>
          (
            p + (target -> (p(target) :+ src)),
            d + (src -> (d(src) :+ target))
          )
      }
  }

  def storiesWithDeps(builder: ExampleDescriptor.Builder): Seq[Story] =
    storiesWithDeps(builder.noDepStories, builder.graph)

  def storiesWithDeps(descs: Seq[Story], graph: Graph[StoryId, DiEdge]): Seq[Story] = {
    val (precedes, depends) = storyDependencies(graph)
    descs
      .map { story =>
        story.copy(
          dependents =
            depends(story.id),
          precedents =
            precedes(story.id)
        )
      }
  }

  def graph(stories: Seq[Story]): Graph[StoryId, DiEdge] =
    Graph(
      stories.flatMap { story =>
        story.dependents.map(story.id ~> _)
      }:_*
    )


  def init(
    defaultStoryPoints: StoryPoints,
    defaultWorkTypeFractions: WorkTypeTag Map Double
  ): DescriptorBuilder =
    DescriptorBuilder(defaultStoryPoints, defaultWorkTypeFractions, Seq.empty, Graph.empty)


}
