package chapter3

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BeansSpec extends AnyFreeSpec with Matchers {

  "Beans" - {
    "equal 3" in {
      Beans.x shouldBe 3
    }

    "head(1,2,3) should be 1" in {
      Beans.head(Beans(1, 2, 3)) shouldBe Some(1)
    }

    "head empty Beans should be None" in {
      Beans.head(Beans()) shouldBe None
    }


    "tail(1,2,3) should be (2,3)" in {
      Beans.tail(Beans(1, 2, 3)) shouldBe Some(Beans(2, 3))
    }

    "tail empty Beans should be None" in {
      Beans.tail(Beans()) shouldBe None
    }

    "drop (Beans(1,2,3,4,5), 2) should be Beans(3,4,5)" in {
      Beans.drop(Beans(1,2,3,4,5), 2) shouldBe Beans(3,4,5)
    }
  }
}
