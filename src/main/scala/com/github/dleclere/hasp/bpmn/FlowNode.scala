package com.github.dleclere.hasp.bpmn

object FlowNodeRef extends (String => FlowNodeRef) {

  def apply(id: String): FlowNodeRef =
    FlowNodeRefImp(id)

}

trait FlowNodeRef extends BaseRef

case class FlowNodeRefImp(id: String) extends FlowNodeRef

trait FlowNode extends FlowElement {

  override val id: Option[FlowNodeRef]

  def incoming: Seq[BaseRef]

  def outgoing: Seq[BaseRef]

}
