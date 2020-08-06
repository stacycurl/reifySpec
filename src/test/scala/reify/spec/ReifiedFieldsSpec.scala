package reify.spec

import echo.spec.{EchoSpec, FileContents}
import reify.Reified.{RCaseClass, RInt, RList, RLong, RMap, RPrimitive, RString}
import reify.ReifiedFields


class ReifiedFieldsSpec extends EchoSpec {
  "create" in {
    ReifiedFields.create(new ClassWithReifiableFields) <=> ReifiedFields(
      Map(
        RInt(123) -> "anInt",
        RString("foo") -> "aString",
        RCaseClass("FileContents", "beep-boop") -> "aFile",
        RString("private") -> "privateString"
      )
    )
  }

  "namesShorterThanDefinitions" in {
    ReifiedFields(
      Map(
        RInt(123) -> "anInt",
        RLong(12345678900L) -> "aLong",
        RString("foo") -> "aString",
        RCaseClass("FileContents", "beep-boop") -> "aFile",
        RString("private") -> "privateString"
      )
    ).namesShorterThanDefinitions <=> ReifiedFields(
      Map(RCaseClass("FileContents", "beep-boop") -> "aFile", RLong(12345678900L) -> "aLong")
    )
  }

  "reify" in {
    val reifiedFields = ReifiedFields(Map(RInt(123) -> "i", RString("foo") -> "foo"))

    reifiedFields.reify(123) <=> RPrimitive("i")

    reifiedFields.reify(List(123, 456)) <=> RList(List(RPrimitive("i"), RInt(456)))

    reifiedFields.reify(Map("foo" -> 123)) <=> RMap(Map(RPrimitive("foo") -> RPrimitive("i")))

    reifiedFields.reify(FileContents("foo")) <=> RCaseClass("FileContents", RPrimitive("foo"))
  }
}

class ClassWithReifiableFields {
  val anInt: Int = 123

  val aString: String = "foo"

  val someFile: FileContents = FileContents("beep-boop")

  private val privateString: String = "private"
}
