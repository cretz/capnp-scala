package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

case class Method extends Struct(0x9500cce23b334d80L) {
  def name = textField(0)
  def name_=(v: String) = textField_=(0, v)
  
  def codeOrder = uint16Field(0)
  def codeOrder_=(v: Int) = uint16Field_=(0, v)

  def params = seqField[Method.Param](1)
  def params_=(v: Seq[Method.Param]) = seqField_=(1, v)
  
  def requiredParamCount = uint16Field(16)
  def requiredParamCount_=(v: Int) = uint16Field_=(16, v)
    
  def returnType = ptrField[Type](2)
  def returnType_=(v: Type) = ptrField_=(2, v)
  
  def annotations = seqField[Annotation](3)
  def annotations_=(v: Seq[Annotation]) = seqField_=(3, v)
}
object Method {
  case class Param extends Struct(0xcbc0c86dae91fcf6L) {
    def name = textField(0)
    def name_=(v: String) = textField_=(0, v)
    
    def `type` = ptrField[Type](1)
    def type_=(v: Type) = ptrField_=(1, v)
    
    def defaultValue = ptrField[Value](2)
    def defaultValue_=(v: Value) = ptrField_=(2, v) 
  
    def annotations = seqField[Annotation](3)
    def annotations_=(v: Seq[Annotation]) = seqField_=(3, v)
  }
}