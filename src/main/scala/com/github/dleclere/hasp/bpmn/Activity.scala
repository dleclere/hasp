package com.github.dleclere.hasp.bpmn

import com.github.dleclere.hasp.bpmn.Gateway.GatewayDirection

object Activity extends ElementCompanion {

  override val builder: ElementBuilder =
    build(TaskRef) {
      matching(nodeLabelEndsWith("Task")) {
        factoryWithFlowNodeInfo {

          case FlowNodeInfo(name, incoming, outgoing) => info =>
            Some {
              SimpleTask(
                id = info.id,
                name = name,
                incoming = incoming,
                outgoing = outgoing,
                stereotype = info.label.replaceAllLiterally("Task", "")
              )
            }
        }
      }
    } | SubProcess.builder

}

sealed trait ActivityRef extends FlowNodeRef

case class TaskRef(id: String) extends ActivityRef

case class SubProcessRef(id: String) extends ActivityRef

trait Activity extends FlowNode {


}

object SubProcess extends ElementCompanion {

  implicit class SubProcessFlowElementContainerBuilder(val container: SubProcess) extends FlowElementContainerBuilder[SubProcess] {

    def withFlowElements(elements: FlowElementsCollection): SubProcess =
      container.copy(flowElements = elements)

    def withLaneSets(laneSets: Seq[LaneSet]): SubProcess =
      container.copy(laneSets = laneSets)

  }
  
  override val builder: ElementBuilder =
    build(SubProcessRef) {
      matching(nodeLabelWhiteList("subProcess")) {
        factoryWithFlowNodeInfo {
  
          case FlowNodeInfo(name, incoming, outgoing) => info =>
            Some {
              info.childElements.foldLeft(
                SubProcess(
                  id = info.id,
                  name = name,
                  incoming = incoming,
                  outgoing = outgoing,
                  flowElements = FlowElementsCollection.empty,
                  laneSets = Seq.empty
                )
              )(FlowElementsContainer.withElement(_, _))
            }
        }
      }
    }
  
}

case class SubProcess(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef],
  flowElements: FlowElementsCollection,
  laneSets: Seq[LaneSet]
) extends Activity with FlowElementsContainer

trait Task extends Activity {

}

//case class ReceiveTask(
//
//)


case class SimpleTask(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef],
  stereotype: String
) extends Activity