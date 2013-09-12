package org.capnp.gen.schema

import org.capnp.model._

case class Enumerant() extends Struct(0x978a7cebdc549a4dL, 8, 2) {
  def name = textField(0)
  def name_=(v: String) = textField_=(0, v)
  
  def codeOrder = uint16Field(0)
  def codeOrder_=(v: Int) = uint16Field_=(0, v)
  
  def annotations = structSeq[Annotation](1, Annotation)
  def annotations_=(v: Seq[Annotation]) = structSeq_=(1, v)
}
object Enumerant extends StructObject[Enumerant]