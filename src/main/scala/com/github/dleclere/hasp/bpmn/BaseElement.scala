package com.github.dleclere.hasp.bpmn

trait BaseRef {

  val id: String

}


trait ElementCompanion {

//  type Ref <: BaseRef
//
//  def Ref(id: String): Ref
//
//  val nodeLabelFilter: String => Boolean

  def builder: ElementBuilder


}

trait BaseElement {
  val id: Option[BaseRef]
  //val documentation: Seq[Documentation]
}

trait MixedContentElement {
  this: BaseElement =>



}