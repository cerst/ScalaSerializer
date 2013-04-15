package de.ifag.scalaserializer

import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import java.io.{DataInputStream, ByteArrayInputStream, DataOutputStream, ByteArrayOutputStream}

class Serializer {

  private val typeMirror: Mirror = runtimeMirror(getClass.getClassLoader)

  def serialize(arg: Any): Array[Byte] = {
    //prepare output
    val buffer = new ByteArrayOutputStream()
    val out = new DataOutputStream(buffer)

    marshal(arg, out)
    buffer.toByteArray
  }

  def marshal(arg: Any, out: DataOutputStream): Unit = {
    //access to specific object
    val instanceMirror = typeMirror reflect arg
    //all members of the object (variables & methods)
    val fields = fieldsSortedByName(instanceMirror)
    def fieldValue(symbol: Symbol) = instanceMirror reflectField symbol.asTerm get
    //start with class name
    val className = instanceMirror.symbol.fullName
    out writeInt className.length
    out writeBytes className
    for (field <- fields)
      writeFieldToStream(field.typeSignature.erasure, fieldValue(field), out)

  }

  def deserialize(arg: Array[Byte]): Any = {
    val in = new DataInputStream(new ByteArrayInputStream(arg))

    unmarshal(in)

  }

  def unmarshal(in: DataInputStream): Any = {
    //read class name from input
    val classNameLength = in.readInt
    val nameBuffer = new Array[Byte](classNameLength)
    in read nameBuffer

    //create object instance
    val toolBox = typeMirror.mkToolBox()
    val obj = toolBox.eval(toolBox.parse("new " + new String(nameBuffer)))

    //prepare field access
    val instanceMirror = typeMirror reflect obj
    val fields = fieldsSortedByName(instanceMirror)
    def fieldMirror(symbol: Symbol) = instanceMirror.reflectField(symbol.asTerm)

    //set fields
    for (field <- fields)
      setFieldFromStream(fieldMirror(field), in)

    obj
  }

  def fieldsSortedByName(instanceMirror: InstanceMirror): Seq[Symbol] = {
    instanceMirror.symbol.typeSignature.members.filterNot(_.isMethod).toSeq.sortBy(_.name.decoded)
  }

  def setFieldFromStream(fieldMirror: FieldMirror, in: DataInputStream): Unit = {
    val fieldType = fieldMirror.symbol.typeSignature.erasure

    if (fieldType =:= typeOf[Short])
      fieldMirror set in.readShort
    else if (fieldType =:= typeOf[Int])
      fieldMirror set in.readInt
    else if (fieldType =:= typeOf[Long])
      fieldMirror set in.readLong
    else if (fieldType =:= typeOf[Float])
      fieldMirror set in.readFloat
    else if (fieldType =:= typeOf[Double])
      fieldMirror set in.readDouble
    else if (fieldType =:= typeOf[Byte])
      fieldMirror set in.readByte
    else if (fieldType =:= typeOf[Boolean])
      fieldMirror set in.readBoolean
    else if (fieldType =:= typeOf[Char])
      fieldMirror set in.readChar
    else if (fieldType =:= typeOf[String]) {
      //read string byte length, then read bytes and convert to string
      val wordBuffer = new Array[Byte](in.readInt)
      in read wordBuffer
      fieldMirror set new String(wordBuffer)
    } else {
      //here we assume the type to be another class (thus ignoring sets, array etc.)
      //therefore we have to start to unmarshal this object for itself
      fieldMirror set unmarshal(in)
    }

  }

  def writeFieldToStream(fieldType: Type, fieldValue: Any, out: DataOutputStream): Unit = {
    if (fieldType =:= typeOf[Short])
      out writeShort fieldValue.asInstanceOf[Short]
    else if (fieldType =:= typeOf[Int])
      out writeInt fieldValue.asInstanceOf[Int]
    else if (fieldType =:= typeOf[Long])
      out writeLong fieldValue.asInstanceOf[Long]
    else if (fieldType =:= typeOf[Float])
      out writeFloat fieldValue.asInstanceOf[Float]
    else if (fieldType =:= typeOf[Double])
      out writeDouble fieldValue.asInstanceOf[Double]
    else if (fieldType =:= typeOf[Byte])
      out writeByte fieldValue.asInstanceOf[Byte]
    else if (fieldType =:= typeOf[Boolean])
      out writeBoolean fieldValue.asInstanceOf[Boolean]
    else if (fieldType =:= typeOf[Char])
      out writeChar fieldValue.asInstanceOf[Char]
    else if (fieldType =:= typeOf[String]) {
      val word = fieldValue.asInstanceOf[String]
      out writeInt word.length
      out writeBytes word
    } else {
      //here we assume the type to be another class (thus ignoring sets, array etc.)
      //therefore we have to start to unmarshal this object for itself
      marshal(fieldValue, out)
    }
  }


}
