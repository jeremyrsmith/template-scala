package show

import org.scalatest.{FreeSpec, Matchers}

class TemplateTest extends FreeSpec with Matchers {

  "show" in {
    implicitly[Show[Foo]]
  }

}
