package org.capnp.gen.schema

import org.capnp.model
import org.capnp.model._

case class Annotation extends Struct(0xf1c8950dab257542L) {
  def id = uint64Field(0)
  def id_=(v: BigInt) = uint64Field_=(0, v)
  
  def value = ptrField[Value](0)
  def value_=(v: Value) = ptrField_=(0, v)
}