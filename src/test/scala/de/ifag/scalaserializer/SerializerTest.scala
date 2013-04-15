package de.ifag.scalaserializer

import org.junit.{Ignore, Before, Test}
import org.fest.assertions.api.Assertions._
import java.io.{ByteArrayInputStream, DataInputStream, DataOutputStream, ByteArrayOutputStream}


class SerializerTest {

  var testPerson: Person = _
  var testWrappedPerson : WrappedPerson = _
  var serializer: Serializer = _

  @Before
  def setUp(): Unit = {
    serializer = new Serializer()

    testPerson = new Person
    testPerson.socialSecNr = 123
    testPerson.firstName = "testFirst"
    testPerson.lastName = "testLast"

    testWrappedPerson = new WrappedPerson
    testWrappedPerson.person = testPerson
    testWrappedPerson.age = 100
  }

  @Test
  //@Ignore
  def simpleTest(): Unit = {
    val className = "de.ifag.scalaserializer.Person"
    val classNameLength = 30

    val buffer = new ByteArrayOutputStream()
    val out = new DataOutputStream(buffer)

    out writeInt classNameLength
    out writeBytes className

    val in = new DataInputStream(new ByteArrayInputStream(buffer.toByteArray))

    val resClassNameLength = in.readInt()
    val resClassNameBuffer = new Array[Byte](resClassNameLength)
    in.read(resClassNameBuffer)

    val resClassName = new String(resClassNameBuffer)

    assertThat(resClassNameLength).isEqualTo(classNameLength)
    assertThat(resClassName).isEqualTo(className)


  }

  @Test
  //@Ignore
  def testHandlePerson(): Unit = {

    val res = serializer.serialize(testPerson)
    val resObject = serializer.deserialize(res)
    val resPerson = resObject.asInstanceOf[Person]

    assertThat(resPerson.socialSecNr).isEqualTo(123)
    assertThat(resPerson.firstName).isEqualTo("testFirst")
    assertThat(resPerson.lastName).isEqualTo("testLast")

  }

  @Test
  def handleWrappedPerson() : Unit = {
    val res = serializer.serialize(testWrappedPerson)
    val resObject = serializer.deserialize(res)
    val resPerson = resObject.asInstanceOf[WrappedPerson]

    assertThat(resPerson.age).isEqualTo(100)
    assertThat(resPerson.person.socialSecNr).isEqualTo(123)
    assertThat(resPerson.person.firstName).isEqualTo("testFirst")
    assertThat(resPerson.person.lastName).isEqualTo("testLast")
  }

}
