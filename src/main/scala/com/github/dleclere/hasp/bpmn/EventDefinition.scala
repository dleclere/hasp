package com.github.dleclere.hasp.bpmn

case class EventDefinitionRef(id: String) extends BaseRef

sealed trait EventDefinition extends BaseElement {

  val id: Option[EventDefinitionRef]

}

case class CancelEventDefinition(
  id: Option[EventDefinitionRef]
) extends EventDefinition

case class CompensateEventDefinition(
  id: Option[EventDefinitionRef],
  waitForCompletion: Boolean, // attribute
  activity: Option[FlowNodeRef] // Activity (attribute ref)
) extends EventDefinition

case class ConditionalEventDefinition(
  id: Option[EventDefinitionRef]
  //condition: BpmnExpression // element
) extends EventDefinition

case class ErrorEventDefinition(
  id: Option[EventDefinitionRef],
  error: Option[BaseRef] // Error (element)
) extends EventDefinition

case class EscalationEventDefinition(
  id: Option[EventDefinitionRef],
  escalation: Option[BaseRef] // Escalation (attribute ref)
) extends EventDefinition

case class LinkEventDefinition(
  id: Option[EventDefinitionRef],
  name: String,
  sources: Seq[EventDefinitionRef], // LinkEventDefinition
  target: Option[EventDefinitionRef] // LinkEventDefinition
) extends EventDefinition

case class MessageEventDefinition(
  id: Option[EventDefinitionRef],
  name: String,
  operation: Option[BaseRef], // Operation (element)
  message: Option[BaseRef] // Message (attribute ref)
) extends EventDefinition

case class TerminateEventDefinition(
  id: Option[EventDefinitionRef]
) extends EventDefinition

/*
<xsd:complexType name="tTimerEventDefinition">
<xsd:complexContent>
<xsd:extension base="tEventDefinition">
<xsd:choice>
<xsd:element name="timeDate" type="tExpression" minOccurs="0" maxOccurs="1"/>
<xsd:element name="timeDuration" type="tExpression" minOccurs="0" maxOccurs="1"/>
<xsd:element name="timeCycle" type="tExpression" minOccurs="0" maxOccurs="1"/>
</xsd:choice>
</xsd:extension>
</xsd:complexContent>
</xsd:complexType>
 */

object TimerEventDefinition {



}

case class TimerEventDefinition(
  id: Option[EventDefinitionRef]
) extends EventDefinition
