package org.capnp.gen.schema

object ElementSize extends Enumeration {
  val empty = Value(0)
  val bit = Value(1)
  val byte = Value(2)
  val twoBytes = Value(3)
  val fourBytes = Value(4)
  val eightBytes = Value(5)
  val pointer = Value(6)
  val inlineComposite = Value(7)
}