package org.capnp.gen.schema

import org.capnp.model._

case class Method() extends Struct(0x9500cce23b334d80L, 8, 4) {
  def name = textField(0)
  def name_=(v: String) = textField_=(0, v)
  
  def codeOrder = uint16Field(0)
  def codeOrder_=(v: Int) = uint16Field_=(0, v)

  def params = structSeq[Method.Param](1, Method.Param)
  def params_=(v: Seq[Method.Param]) = structSeq_=(1, v)
  
  def requiredParamCount = uint16Field(16)
  def requiredParamCount_=(v: Int) = uint16Field_=(16, v)
    
  def returnType = structField[Type](2, Type)
  def returnType_=(v: Option[Type]) = structField_=(2, v)
  
  def annotations = structSeq[Annotation](3, Annotation)
  def annotations_=(v: Seq[Annotation]) = structSeq_=(3, v)
}
object Method extends StructObject[Method] {
  case class Param() extends Struct(0xcbc0c86dae91fcf6L, 0, 4) {
    def name = textField(0)
    def name_=(v: String) = textField_=(0, v)
    
    def `type` = structField[Type](1, Type)
    def type_=(v: Option[Type]) = structField_=(1, v)
    
    def defaultValue = structField[Value](2, Value)
    def defaultValue_=(v: Option[Value]) = structField_=(2, v) 
  
    def annotations = structSeq[Annotation](3, Annotation)
    def annotations_=(v: Seq[Annotation]) = structSeq_=(3, v)
  }
  object Param extends StructObject[Param]
}