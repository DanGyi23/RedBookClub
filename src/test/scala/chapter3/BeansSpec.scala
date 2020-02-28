package chapter3

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BeansSpec extends AnyFreeSpec with Matchers {

  "x" - {
    "equal 3" in {
      Beans.x shouldBe 3
    }
  }
}
