package com.github.dleclere.hasp.bpmn



object SequenceFlow extends ElementCompanion {

  override val builder: ElementBuilder =
    build(SequenceFlowRef) {
      matching(nodeLabelWhiteList("sequenceFlow")) {
        factory { info =>
          for {
            source      <-  info.attributes.get("sourceRef")
            sourceRef   <-  info.refs.get(source).collect { case r: FlowNodeRef => r}
            target      <-  info.attributes.get("targetRef")
            targetRef   <-  info.refs.get(target).collect { case r: FlowNodeRef => r}
          } yield
            SequenceFlow(
              id = info.id,
              name = info.attributes.get("name"),
              isImmediate = info.attributes.get("isImmediate").contains("true"),
              sourceRef = sourceRef,
              targetRef = targetRef
            )
        }
      }
    }
}

case class SequenceFlowRef(id: String) extends BaseRef

case class SequenceFlow(
  override val id: Option[SequenceFlowRef],
  name: Option[String],
  isImmediate: Boolean,
  sourceRef: FlowNodeRef,
  targetRef: FlowNodeRef
) extends FlowElement