package org.capnp.gen.schema

import org.capnp.model._

case class Annotation(ptr: StructPtr) extends Struct {
  def id = uint64Field(0)
  def id_=(v: BigInt) = uint64Field_=(0, v)
  
  def value = structField[Value](0, Value)
  def value_=(v: Option[Value]) = structField_=(0, v)
}

object Annotation extends StructObject[Annotation](8, 1)