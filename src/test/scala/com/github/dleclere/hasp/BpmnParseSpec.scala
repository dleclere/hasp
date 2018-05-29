package com.github.dleclere.hasp

import bpmn._
import org.scalatest.{FlatSpec, Matchers}


class BpmnParseSpec extends FlatSpec with Matchers  {

  val relativeLocation = getClass.getClassLoader.getResource("com.github.dleclere.hasp/bpmn/Incident Management(Whole Collab).bpmn")

  val bpmn = xml.XML.load(relativeLocation)

  "An xml file" should "be loaded" in {
    assert(bpmn.child.nonEmpty)
  }

  "The xml file" should "contain at least one task" in {
    val BpmnDiagram(elements) = parse(bpmn, BuilderRegister.builder)
    val firstTask = elements.collectFirst {

      case p: Process =>
        p.flowElements.flowNodes.values.collectFirst {

          case task: SimpleTask =>
            task

        }

    }.flatten

    firstTask should not be empty
  }

  "The xml file" should "contain at least one gateway" in {
    val BpmnDiagram(elements) = parse(bpmn, BuilderRegister.builder)
    val firstGateway = elements.collect {

      case p: Process =>
        p.flowElements.flowNodes.values.collectFirst {

          case gateway: Gateway =>
            gateway

        }

    }.flatten

    firstGateway should not be empty
  }

  "The xml file" should "contain at least one event" in {
    val BpmnDiagram(elements) = parse(bpmn, BuilderRegister.builder)
    val firstEvent = elements.collect {

      case p: Process =>
        p.flowElements.flowNodes.values.collectFirst {

          case event: Event =>
            event

        }

    }.flatten

    firstEvent should not be empty
  }

  "The xml file" should "contain at least one sequenceFlow" in {
    val BpmnDiagram(elements) = parse(bpmn, BuilderRegister.builder)

    val firstSequenceFlow = elements.collect {

      case p: Process =>
        p.flowElements.sequenceFlows.headOption

    }.flatten

    firstSequenceFlow should not be empty
  }

  "The xml file" should "contain at least one laneSet" in {
    val BpmnDiagram(elements) = parse(bpmn, BuilderRegister.builder)

    val firstLaneSet = elements.collect {

      case p: Process =>
        p.laneSets.collectFirst {

          case laneSet: LaneSet =>
            laneSet

        }

    }.flatten

    firstLaneSet should not be empty
  }

  
  "The xml file" should "contain at least one lane" in {
    val BpmnDiagram(elements) = parse(bpmn, BuilderRegister.builder)

    val firstLane = elements.collect {

      case p: Process =>
        p.laneSets.flatMap(_.lanes).headOption

    }.flatten

    firstLane should not be empty
  }

  
  
}
