package com.github.dleclere.hasp.bpmn

case class LaneRef(id: String) extends BaseRef


object Lane extends ElementCompanion {

  override val builder: ElementBuilder =
    build(LaneRef) {
      matching(nodeLabelWhiteList("lane")) {
        factory { info =>
          Some {
            Lane(
              id = info.id,
              name = info.attributes.get("name"),
              partitionElement = None,
              partitionElementRef = None,
              childLaneSets = info.childElements.collect {

                case child: LaneSet =>
                  child

              },
              flowNodeRefs = info.node.child
                .filter(_.label.toLowerCase == "flowNodeRef".toLowerCase)
                .map(_.text)
                .map(FlowNodeRef)
            )
          }
        }
      }
    }
}

case class Lane(
  id: Option[LaneRef],
  name: Option[String],
  partitionElement: Option[BaseElement],
  partitionElementRef: Option[BaseRef],
  childLaneSets: Seq[LaneSet],
  flowNodeRefs: Seq[FlowNodeRef]
) extends BaseElement
