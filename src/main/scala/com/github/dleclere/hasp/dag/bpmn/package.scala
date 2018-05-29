package com.github.dleclere.hasp.dag

import com.github.dleclere.hasp.bpmn._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._

import scala.reflect.ClassTag

package object bpmn {

  def all[T <: FlowElement : ClassTag](diagram: BpmnDiagram): Seq[T] ={

    def inner(elements: Seq[BaseElement]): Seq[T] =
      elements.flatMap {

        case sf: T =>
          Seq(sf)

        case container: FlowElementsContainer =>
          inner(container.flowElements.flowElements)

        case _ =>
          Seq.empty

      }

    inner(diagram.elements)

  }

  def allSequenceFlows(diagram: BpmnDiagram): Seq[SequenceFlow] =
    all[SequenceFlow](diagram)

  def bpmnDependencyGraph(diagram: BpmnDiagram): Graph[FlowNodeRef, DiEdge] =
      Graph(
        allSequenceFlows(diagram).map {

          case SequenceFlow(_, _, _, source, target) =>
            source ~> target

        } :_*
      )

  def bpmnTaskDependencyGraph(graph: Graph[FlowNodeRef, DiEdge]): Graph[TaskRef, DiEdge] = {

    val startEvents =
      graph.nodes
        .filter(_.value.isInstanceOf[StartEventRef])

    def buildPath(startEventNode: graph.NodeT): Set[DiEdge[TaskRef]] = {

      def inner(node: graph.NodeT, ignoredStack: Set[graph.NodeT], lastTask: Option[TaskRef]): Set[DiEdge[TaskRef]] = node.value match {

        case task: TaskRef =>
          lastTask.map(_ ~> task).toSet ++ node.diSuccessors.flatMap(inner(_, ignoredStack + node, Some(task)))

        case _ if ignoredStack contains node =>
          Set.empty

        case _ =>
          node.diSuccessors.flatMap(inner(_, ignoredStack + node, lastTask))

      }

      inner(startEventNode, Set.empty, None)

    }

    Graph(
      startEvents
        .flatMap(buildPath).toSeq: _*
    )

  }


}
