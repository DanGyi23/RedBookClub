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

    "setHead should change the first value in a list" in {
      Beans.setHead(Beans(5,6,7,8,9), 69) shouldBe Beans(69,6,7,8,9)
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

    "dropWhile (Beans(10,2,3,4,5), (x: Int) => x % 5 == 0) should be Beans(2,3,4,5)" in {
      Beans.dropWhile(Beans(10,2,3,4,5))((x: Int) => x % 5 == 0) shouldBe Beans(2,3,4,5)
    }

    "length Beans(_*) should be appropriate length" in {
      Beans.length(Beans(10,24,35,41,52)) shouldBe 5
      Beans.length(Beans("dogs", "cats", "bananas")) shouldBe 3
    }

    "foldLeft(1,2,3,4)(x,y => x + y) should be 10" in {
      Beans.foldLeft(Beans(1,2,3,4), 0)((x, y) => x + y) shouldBe 10
    }

    "sumFl should be sum of Beans" in {
      val list = Beans(1,2,3,4)
      Beans.sumFl(list) shouldBe 10
    }

    "productFl should be product of Beans" in {
      val list = Beans(1,2,3,4)
      Beans.productFl(list) shouldBe 24
    }

    "lengthFl should return appropriate length for Beans" in {
      Beans.lengthFl(Beans(1,2,3,4,5,6,7)) shouldBe 7
    }
  }
}
