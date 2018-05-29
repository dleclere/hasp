package com.github.dleclere.hasp.bpmn

import com.github.dleclere.hasp.bpmn.Gateway.{EventBasedGatewayType, GatewayDirection}

object Gateway extends ElementCompanion {


  object GatewayDirection extends Enumeration {

    val Unspecified, Converging, Diverging, Mixed = Value

    def apply(str: String): Value =
      values.find(_.toString.toLowerCase == str.toLowerCase).getOrElse(Unspecified)

  }

  object EventBasedGatewayType extends Enumeration {

    val Parallel, Exclusive = Value

    def get(str: String): Option[Value] =
      values.find(_.toString.toLowerCase == str.toLowerCase)

  }

  override val builder: ElementBuilder =
    buildFlowNode {
      matching(nodeLabelEndsWith("Gateway")) {
        factoryWithFlowNodeInfo {

          case FlowNodeInfo(name, incoming, outgoing) => info =>
            Some {
              SequenceFlowGateway(
                id = info.id,
                name = name,
                direction = info.attributes
                  .get("gatewayDirection")
                  .fold(GatewayDirection.Unspecified)(GatewayDirection(_)),
                incoming = incoming,
                outgoing = outgoing,
                stereotype = info.label.replaceAllLiterally("Gateway", "")
              )
            }
        }
      }
    } | buildFlowNode {
      matching(nodeLabelWhiteList("eventBasedGateway")) {
        factoryWithFlowNodeInfo {

          case FlowNodeInfo(name, incoming, outgoing) => info =>
            Some {
              EventBasedGateway(
                id = info.id,
                name = name,
                direction = info.attributes
                  .get("gatewayDirection")
                  .fold(GatewayDirection.Unspecified)(GatewayDirection(_)),
                instantiate = info.attributes.get("instantiate").contains("true"),
                eventGatewayType = info.attributes
                  .get("eventGatewayType")
                  .flatMap(EventBasedGatewayType.get)
                  .getOrElse(EventBasedGatewayType.Exclusive),
                incoming = incoming,
                outgoing = outgoing
              )
            }
        }
      }
    }

}

trait Gateway extends FlowNode {

  val direction: GatewayDirection.Value

}

case class SequenceFlowGateway(
  id: Option[FlowNodeRef],
  name: Option[String],
  direction: GatewayDirection.Value,
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef],
  stereotype: String
) extends Gateway

case class EventBasedGateway(
  id: Option[FlowNodeRef],
  name: Option[String],
  direction: GatewayDirection.Value,
  instantiate: Boolean,
  eventGatewayType: EventBasedGatewayType.Value,
  incoming: Seq[SequenceFlowRef],
  outgoing: Seq[SequenceFlowRef]
) extends Gateway

