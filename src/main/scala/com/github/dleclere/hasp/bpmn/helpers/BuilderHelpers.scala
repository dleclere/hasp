package com.github.dleclere.hasp.bpmn.helpers

import com.github.dleclere.hasp.bpmn._

import scala.language.implicitConversions

trait BuilderHelpers {

  type ElementBuilder = helpers.ElementBuilder

  type FactoryInfo[T <: BaseRef] = helpers.FactoryInfo[T]

  type BpmnXmlFactorySig[R <: BaseRef] = helpers.BpmnXmlFactorySig[R]

  type ElementFilter = helpers.ElementFilter

  object FlowNodeInfo {

    def apply[R <: FlowNodeRef]: FactoryInfo[R] => FlowNodeInfo = factoryInfo =>
      FlowNodeInfo(
        name = factoryInfo.attributes.get("name"),
        incoming = readIncomingSequenceFlows(factoryInfo.node),
        outgoing = readOutgoingSequenceFlows(factoryInfo.node)
      )
  }

  case class FlowNodeInfo(
    name: Option[String],
    incoming: Seq[SequenceFlowRef],
    outgoing: Seq[SequenceFlowRef]
  )

  def nodeLabelWhiteList(allowed: String*): FilterMatcher =
    NodeLabelWhiteList(allowed:_*)

  def nodeLabelEndsWith(allowed: String): FilterMatcher =
    NodeLabelEndsWith(allowed)

  def findElementBuilder(node: xml.Node, current: ElementBuilder): Option[ElementBuilder] =
    if (current.filter test node)
      Some(current)
    else
      current.altBuilder.flatMap(findElementBuilder(node, _))

  def build[R <: BaseRef](refFactory: String => R)(elementFactorySig: BpmnXmlFactorySig[R]): ElementBuilder = {
    val BpmnXmlFactorySig(matchers, factory) = elementFactorySig
    ElementBuilder(
      filter = ElementFilter(matchers: _*),
      refFactory = refFactory,
      //TODO Use the id map!
      elFactory = (node: xml.Node, builder: ElementBuilder, refs: String Map BaseRef) => factory(FactoryInfo(node, refs, refFactory, builder)),
      altBuilder = None
    )
  }

  def buildFlowNode(elementFactory: BpmnXmlFactorySig[FlowNodeRef]): ElementBuilder =
    build(FlowNodeRef(_))(elementFactory)

  def matching[R <: BaseRef](filter: FilterMatcher)(factory: BpmnXmlFactorySig[R]): BpmnXmlFactorySig[R] =
    BpmnXmlFactorySigMatcher[R](filter, factory)

  def factoryWithFlowNodeInfo[R <: FlowNodeRef](fn: FlowNodeInfo => FactoryInfo[R] => Option[BaseElement]): BpmnXmlFactorySigApplicator[R] =
    factory(info => fn(FlowNodeInfo(info))(info))

  def factory[R <: BaseRef](fn: FactoryInfo[R] => Option[BaseElement]): BpmnXmlFactorySigApplicator[R] =
    BpmnXmlFactorySigApplicator(fn)
}
