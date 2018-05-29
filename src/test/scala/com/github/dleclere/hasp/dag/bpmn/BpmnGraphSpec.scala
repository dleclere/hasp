package com.github.dleclere.hasp.dag.bpmn

import com.github.dleclere.hasp.bpmn._
import org.scalatest.{FlatSpec, Matchers}
import scalax.collection.Graph
import scalax.collection.GraphPredef.EdgeLikeIn

import scala.language.higherKinds

class BpmnGraphSpec extends FlatSpec with Matchers  {

  val relativeLocation = getClass.getClassLoader.getResource("com.github.dleclere.hasp/bpmn/Incident Management(Whole Collab).bpmn")

  val bpmn = xml.XML.load(relativeLocation)

  "The dependency graph" should "contain at least one element" in {
    val diagram = parse(bpmn, BuilderRegister.builder)

    val graph = bpmnDependencyGraph(diagram)

    graph.nodes should not be empty
  }

  "The task dependency graph" should "contain at least one element" in {
    val diagram = parse(bpmn, BuilderRegister.builder)

    val graph = bpmnTaskDependencyGraph(bpmnDependencyGraph(diagram))

    graph.nodes should not be empty
  }

  
}
