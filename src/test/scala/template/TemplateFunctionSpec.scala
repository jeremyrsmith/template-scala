package template

import org.scalatest.{FreeSpec, Matchers}

class TemplateFunctionSpec extends FreeSpec with Matchers {

  "TemplateFunction0" - {

    val fn = TemplateFunction {
      () => 22
    }

    "inline and apply" in {
      fn.inline() shouldEqual 22
      fn() shouldEqual 22
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Int) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline() shouldEqual 44
      fn3() shouldEqual 44
    }
  }

  "TemplateFunction1" - {

    val fn = TemplateFunction {
      (a: Int) => a * 22
    }

    "inline and apply" in {
      fn.inline(2) shouldEqual 44
      fn(2) shouldEqual 44
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Int) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2) shouldEqual 88
      fn3(2) shouldEqual 88
    }

  }

  "TemplateFunction2" - {

    val fn = TemplateFunction {
      (a: Int, b: String) => a * b.length
    }

    "inline and apply" in {
      fn.inline(2, "foo") shouldEqual 6
      fn(2, "foo") shouldEqual 6
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Int) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo") shouldEqual 12
      fn3(2, "foo") shouldEqual 12
    }

  }

  "TemplateFunction3" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float) => int * str.length * float
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f) shouldEqual 12.0f
      fn(2, "foo", 2.0f) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f) shouldEqual 24.0f
      fn3(2, "foo", 2.0f) shouldEqual 24.0f
    }
  }

  "TemplateFunction4" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean) => int * str.length * (if(b) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true) shouldEqual 24.0f
    }
  }

  "TemplateFunction5" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean, b2: Boolean) => int * str.length * (if(b && b2) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true, true) shouldEqual 24.0f
    }
  }

  "TemplateFunction6" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean, b2: Boolean, b3: Boolean) => int * str.length * (if(b && b2 && b3) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true, true, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true, true, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true, true, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true, true, true) shouldEqual 24.0f
    }
  }

  "TemplateFunction7" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean, b2: Boolean, b3: Boolean, b4: Boolean) => int * str.length * (if(b && b2 && b3 && b4) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true, true, true, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true, true, true, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true, true, true, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true, true, true, true) shouldEqual 24.0f
    }
  }

  "TemplateFunction8" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean, b2: Boolean, b3: Boolean, b4: Boolean, b5: Boolean) => int * str.length * (if(b && b2 && b3 && b4 && b5) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true, true, true, true, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true, true, true, true, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true, true, true, true, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true, true, true, true, true) shouldEqual 24.0f
    }
  }

  "TemplateFunction9" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean, b2: Boolean, b3: Boolean, b4: Boolean, b5: Boolean, b6: Boolean) => int * str.length * (if(b && b2 && b3 && b4 && b5 && b6) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true, true, true, true, true, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true, true, true, true, true, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true, true, true, true, true, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true, true, true, true, true, true) shouldEqual 24.0f
    }
  }

  "TemplateFunction10" - {

    val fn = TemplateFunction {
      (int: Int, str: String, float: Float, b: Boolean, b2: Boolean, b3: Boolean, b4: Boolean, b5: Boolean, b6: Boolean, b7: Boolean) => int * str.length * (if(b && b2 && b3 && b4 && b5 && b6 && b7) float else 1.0f)
    }

    "inline and apply" in {
      fn.inline(2, "foo", 2.0f, true, true, true, true, true, true, true) shouldEqual 12.0f
      fn(2, "foo", 2.0f, true, true, true, true, true, true, true) shouldEqual 12.0f
    }

    "andThen" in {
      val fn2 = TemplateFunction {
        (i: Float) => i * 2
      }

      val fn3 = fn andThen fn2

      fn3.inline(2, "foo", 2.0f, true, true, true, true, true, true, true) shouldEqual 24.0f
      fn3(2, "foo", 2.0f, true, true, true, true, true, true, true) shouldEqual 24.0f
    }
  }

}
