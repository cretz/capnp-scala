package org.capnp.model

import scala.collection.immutable.BitSet
import scala.collection.mutable.ListBuffer

object Packer {
  def unpack(bytes: Iterator[Byte]): Iterator[Byte] = {
    def nextByte = if (bytes.hasNext) Some(bytes.next) else None
    def nextVal = nextByte map(_ & 0xff)
    def unpackTag: Option[Seq[Byte]] = nextVal map {
      case 0x00 => Seq.fill(8 + nextVal.get * 8)(0x00.toByte)
      case 0xff => (0 until 8 map(_ => nextByte.get)) ++
        (0 until nextVal.get * 8 map(_ => nextByte.get))
      case b =>
        val s = BitSet.fromBitMask(Array(b))
        0 until 8 map(i => if (s(i)) nextByte.get else 0x00.toByte)
    }
    ((Iterator continually(unpackTag) takeWhile(_.isDefined)).flatten).flatten
  }
}