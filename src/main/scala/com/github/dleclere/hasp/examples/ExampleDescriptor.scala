package com.github.dleclere.hasp.examples

import com.github.dleclere.hasp.Domain.{Story, StoryId, StoryPoints, WorkTypeTag}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.Graph

object ExampleDescriptor {

  sealed trait Builder {

    val defaultStoryPoints: StoryPoints
    val defaultWorkTypeFractions: WorkTypeTag Map Double
    val noDepStories: Seq[Story]
    val graph: Graph[StoryId, DiEdge]

    def addStory(desc: Story): StoryDepsDescriptorBuilder

  }
  case class DescriptorBuilder(
    defaultStoryPoints: StoryPoints,
    defaultWorkTypeFractions: WorkTypeTag Map Double,
    noDepStories: Seq[Story],
    graph: Graph[StoryId, DiEdge]
  ) extends Builder {

    def addStory(desc: Story): StoryDepsDescriptorBuilder =
      StoryDepsDescriptorBuilder(
        defaultStoryPoints = defaultStoryPoints,
        defaultWorkTypeFractions = defaultWorkTypeFractions,
        noDepStories = noDepStories :+ desc,
        graph = graph,
        currentStory = desc.id
      )
  }

  case class StoryDepsDescriptorBuilder(
    defaultStoryPoints: StoryPoints,
    defaultWorkTypeFractions: WorkTypeTag Map Double,
    noDepStories: Seq[Story],
    graph: Graph[StoryId, DiEdge],
    currentStory: StoryId
  ) extends Builder {

    def addStory(desc: Story): StoryDepsDescriptorBuilder =
      copy(
        currentStory = desc.id,
        noDepStories = noDepStories :+ desc
      )

  }


}
