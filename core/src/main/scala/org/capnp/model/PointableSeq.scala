package org.capnp.model

import scala.collection.IndexedSeq
import scala.collection.IndexedSeqLike
import scala.collection.mutable.Builder
import scala.collection.immutable.VectorBuilder
import scala.collection.generic.CanBuildFrom

abstract class PointableSeq[T] extends IndexedSeq[T]
    with IndexedSeqLike[T, PointableSeq[T]] with Pointable {
  override protected[this] def newBuilder: Builder[T, PointableSeq[T]] = PointableSeq.newBuilder
}

object PointableSeq {
  def apply[T](vals: T*) = fromSeq(vals)
 
  def fromSeq[T](seq: Seq[T]) = new LiteralPointableSeq[T](seq)
 
  def newBuilder[T]: Builder[T, PointableSeq[T]] =
    new VectorBuilder mapResult fromSeq
 
  implicit def canBuildFrom[T <: Struct, From]: CanBuildFrom[PointableSeq[_], T, PointableSeq[T]] =
    new CanBuildFrom[PointableSeq[_], T, PointableSeq[T]] {
      def apply(): Builder[T, PointableSeq[T]] = newBuilder
      def apply(from: PointableSeq[_]): Builder[T, PointableSeq[T]] = newBuilder
  }
}

class LiteralPointableSeq[T] private[model] (vals: Seq[T]) extends PointableSeq[T] {
  def apply(idx: Int): T = vals(idx)
 
  def length = vals.length
}

class PrimitiveSeq[T] private[model] (
    seg: Message#Segment, buf: ByteBuf, count: Int, elemSizeType: Byte, elemType: Type.Value)
    extends PointableSeq[T] {
 
  lazy val stepBits = elemSizeType match {
    case 0 => 0
    case 1 => 1
    case 2 => 8
    case 3 => 16
    case 4 => 32
    case 5 => 64
    case 6 => 64
    case _ => throw new Exception("Unrecognized element type")
  }
  
  lazy val readFunc: (Long) => Any = elemType match {
    case Type.Bool => buf.readBool
    case Type.Int8 => buf.readInt8
    case Type.Int16 => buf.readInt16
    case Type.Int32 => buf.readInt32
    case Type.Int64 => buf.readInt64
    case Type.UInt8 => buf.readUInt8
    case Type.UInt16 => buf.readUInt16
    case Type.UInt32 => buf.readUInt32
    case Type.UInt64 => buf.readUInt64
    case Type.Float32 => buf.readFloat32
    case Type.Float64 => buf.readFloat64
    case _ => ???
  }
  
  def apply(idx: Int): T = {
    if (idx < 0 || count <= idx) throw new IndexOutOfBoundsException
    readFunc(idx * stepBits).asInstanceOf[T]
  }
 
  def length = count
}

class CompositeSeq[T <: Struct] private[model] (
    seg: Message#Segment, buf: ByteBuf, count: Int, stepWords: Int, obj: StructObject[T])
    extends PointableSeq[T] {
 
  def apply(idx: Int): T = {
    if (idx < 0 || count <= idx) throw new IndexOutOfBoundsException
    val s = obj()
    s.buf = Some(buf.slice(idx * (stepWords * 64L)))
    s.seg = Some(seg)
    s
  }
 
  def length = count
}