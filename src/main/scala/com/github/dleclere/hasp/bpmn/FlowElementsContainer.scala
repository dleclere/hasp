package com.github.dleclere.hasp.bpmn


object FlowElementsCollection {

  val empty: FlowElementsCollection = FlowElementsCollection(
    flowNodes = Map.empty,
    sequenceFlows = Seq.empty,
    flowElements = Seq.empty
  )

}

case class FlowElementsCollection(
  flowNodes: Map[FlowNodeRef, FlowNode],
  sequenceFlows: Seq[SequenceFlow],
  flowElements: Seq[FlowElement]
) {

  def withFlowElement: FlowElement => FlowElementsCollection = {

    case el: FlowNode =>
      el.id
        .map(_ -> el)
        .fold(this) { tpl =>
          copy(flowNodes = flowNodes + tpl)
        }.copy(flowElements = flowElements :+ el)

    case el: SequenceFlow =>
      copy(
        sequenceFlows = sequenceFlows :+ el,
        flowElements = flowElements :+ el
      )

    case el =>
      copy(flowElements = flowElements :+ el)

  }

}

trait FlowElementContainerBuilder[T <: FlowElementsContainer] {

  val container: T

  def withFlowElements(elements: FlowElementsCollection): T

  def withLaneSets(laneSets:Seq[LaneSet]): T

}

object FlowElementsContainer {

  def withElement[T <: FlowElementsContainer](
    builder: FlowElementContainerBuilder[T],
    el: BaseElement
  ): T = el match {

    case fe: FlowElement =>
      builder.withFlowElements(builder.container.flowElements.withFlowElement(fe))

    case ls: LaneSet =>
      builder.withLaneSets(builder.container.laneSets :+ ls)

    case _ =>
      builder.container

  }

}

trait FlowElementsContainer extends BaseElement {
  val flowElements: FlowElementsCollection
  val laneSets: Seq[LaneSet]

}
