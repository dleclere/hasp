package com.github.dleclere.hasp.bpmn

case class ProcessRef(id: String) extends BaseRef

object Process extends ElementCompanion {


  implicit class ProcessFlowElementContainerBuilder(val container: Process) extends FlowElementContainerBuilder[Process] {

    def withFlowElements(elements: FlowElementsCollection): Process =
      container.copy(flowElements = elements)

    def withLaneSets(laneSets: Seq[LaneSet]): Process =
      container.copy(laneSets = laneSets)

  }

  override def builder: ElementBuilder =
    build(ProcessRef) {
      matching(nodeLabelWhiteList("process")) { factory {
        info =>
          Some {
            info.childElements.foldLeft(
              Process(
                id = info.id,
                flowElements = FlowElementsCollection.empty,
                laneSets = Seq.empty
              )
            )(FlowElementsContainer.withElement[Process](_, _))
          }
        }
      }
    }
}



case class Process(
  id: Option[ProcessRef],
  flowElements: FlowElementsCollection,
  laneSets: Seq[LaneSet]
) extends FlowElementsContainer