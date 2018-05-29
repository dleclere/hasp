package com.github.dleclere.hasp.bpmn

case class LaneSetRef(id: String) extends BaseRef


object LaneSet extends ElementCompanion {

  override val builder: ElementBuilder =
    build(LaneSetRef) {
      matching(nodeLabelWhiteList("laneSet")) {
        factory { info =>
          Some {
            LaneSet(
              id = info.id,
              name = info.attributes.get("name"),
              lanes = info.childElements.collect {

                case child: Lane =>
                  child

              }
            )
          }
        }
      }
    }
}


case class LaneSet(
  id: Option[LaneSetRef],
  name: Option[String],
  lanes: Seq[Lane]
) extends BaseElement
