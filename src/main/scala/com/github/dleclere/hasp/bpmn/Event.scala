package com.github.dleclere.hasp.bpmn
import com.github.dleclere.hasp.bpmn.helpers.FilterMatcher
import henkan.convert.Syntax._

object Event extends ElementCompanion {

  override val builder: ElementBuilder =
    build(StartEventRef)(factory(nodeLabelWhiteList("startEvent"))) |
    build(EndEventRef)(factory(nodeLabelWhiteList("endEvent"))) |
    build(EventRef)(factory(nodeLabelEndsWith("Event")))

  def factory[R <: EventRef](filter: FilterMatcher): BpmnXmlFactorySig[R] =
    matching(filter) {
      factoryWithFlowNodeInfo {

        case FlowNodeInfo(name, incoming, outgoing) => info =>
          Event(
            EventTemplate(
              id = info.id,
              name = name,
              incoming = incoming,
              outgoing = outgoing
            ),
            info
          ).lift(info.label)
            .fold[Option[Event]](throw new Error(s"Unable to find ${info.label}"))(Option(_))
      }
    }

  def apply[R <: EventRef](
    template: EventTemplate,
    info: FactoryInfo[R]): PartialFunction[String, Event] = {

    case "startEvent" =>
      template.to[StartEvent].set(
        parallelMultiple = info.attributes.get("parallelMultiple").contains("true")
      )

    case "boundaryEvent" if info.attributes.get("attachedToRef").nonEmpty =>
      template.to[BoundaryEvent].set(
        parallelMultiple = info.attributes.get("parallelMultiple").contains("true"),
        cancelActivity = info.attributes.get("cancelActivity").forall(_ == "true"),
        attachedToRef = info.attributes.get("attachedToRef").map(FlowNodeRef).get
      )

    case "intermediateCatchEvent" =>
      template.to[IntermediateCatchEvent].set(
        parallelMultiple = info.attributes.get("parallelMultiple").contains("true")
      )

    case "endEvent" =>
      template.to[EndEvent]()

    case "implicitThrowEvent" =>
      template.to[ImplicitThrowEvent]()

    case "intermediateThrowEvent" =>
      template.to[IntermediateThrowEvent]()

  }

  case class EventTemplate(
    id: Option[FlowNodeRef],
    name: Option[String],
    incoming: Seq[SequenceFlowRef],
    outgoing: Seq[SequenceFlowRef]
  )


}

object EventRef extends (String => EventRef) {

  def apply(id: String): EventRef =
    EventRefImp(id)

}

sealed trait EventRef extends FlowNodeRef

case class EndEventRef(id: String) extends EventRef
case class StartEventRef(id: String) extends EventRef
case class EventRefImp(id: String) extends EventRef

sealed trait Event extends FlowNode {


}

trait CatchEvent extends Event {

  /*
  <xsd:element ref="dataOutput" minOccurs="0" maxOccurs="unbounded"/>
  <xsd:element ref="dataOutputAssociation" minOccurs="0" maxOccurs="unbounded"/>
  <xsd:element ref="outputSet" minOccurs="0" maxOccurs="1"/>
  <xsd:element ref="eventDefinition" minOccurs="0" maxOccurs="unbounded"/>
  <xsd:element name="eventDefinitionRef" type="xsd:QName" minOccurs="0" maxOccurs="unbounded"/>
   */

  val parallelMultiple: Boolean //default false

}
/*
<xsd:element ref="dataInput" minOccurs="0" maxOccurs="unbounded"/>
<xsd:element ref="dataInputAssociation" minOccurs="0" maxOccurs="unbounded"/>
<xsd:element ref="inputSet" minOccurs="0" maxOccurs="1"/>
<xsd:element ref="eventDefinition" minOccurs="0" maxOccurs="unbounded"/>
<xsd:element name="eventDefinitionRef" type="xsd:QName" minOccurs="0" maxOccurs="unbounded"/>
 */
trait ThrowEvent extends Event {



}

case class StartEvent(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef],
  parallelMultiple: Boolean
) extends CatchEvent

/*
<xsd:attribute name="cancelActivity" type="xsd:boolean" default="true"/>
<xsd:attribute name="attachedToRef" type="xsd:QName"/>
 */

case class BoundaryEvent(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef],
  parallelMultiple: Boolean,
  cancelActivity: Boolean, // default true
  attachedToRef: FlowNodeRef // Activities only
) extends CatchEvent

case class IntermediateCatchEvent(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef],
  parallelMultiple: Boolean
) extends CatchEvent


case class EndEvent(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef]
) extends ThrowEvent

case class ImplicitThrowEvent(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef]
) extends ThrowEvent

case class IntermediateThrowEvent(
  id: Option[FlowNodeRef],
  name: Option[String],
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef]
) extends ThrowEvent