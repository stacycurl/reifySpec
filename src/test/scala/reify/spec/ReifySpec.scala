package reify.spec

import echo.spec.{EchoSpec, FileContents, ScalaSrc}
import reify.Reify
import scalaz.deriving


class ReifySpec extends EchoSpec {
  "foo" in {
    assert(roundTrip(Foo(123)) === Some(Foo(123)))
  }

  "bar" in {
    assert(roundTrip(Bar(123, 456)) === Some(Bar(123, 456)))
  }

//  "useAsArgumentsFor" in {
//    assert(roundTrip(Oof(123)) === Some(Oof(123)))
//  }
//
  "find" - {
    "built ins" in {
      assert(Reify.find(123)   === Some(Reify.of[Int]))
      assert(Reify.find("foo") === Some(Reify.of[String]))
    }

    "custom" in {
      assert(Reify.find(FileContents("boom"))   === Some(Reify.of[FileContents]))
      assert(Reify.find(ScalaSrc("123")) === Some(Reify.of[ScalaSrc]))
    }
  }

  private def roundTrip[A: Reify](value: A): Option[A] =
    Reify.reflect(Reify.reify(value))
}


@deriving(Reify)
case class Foo(value: Int) {
  def toOof: Oof = Oof(value)
}

object Oof {
//  implicit val oofReify: Reify[Oof] =
//    Reify.of[Foo].useAsArgumentsFor[Oof]("Oof", _.toOof, _.toFoo)
}

case class Oof(value: Int) {
  def toFoo: Foo = Foo(value)
}

@deriving(Reify)
case class Bar(i: Int, anotherInt: Int)