package chapter2

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IsSortedSpec extends AnyFreeSpec with Matchers {

  def orderedInt(val1: Int, val2: Int): Boolean = {
    val1 < val2
  }

  def orderedString(str1: String, str2: String): Boolean = {
    str1 < str2
  }

  "IsSorted" - {

    "should return true for IsSorted.isSorted(Array[1,2], orderedInt(1,2))" in {

      IsSorted.isSorted[Int](Array(1,2), orderedInt) shouldBe true
    }

    "should return false for IsSorted.isSorted(Array[1,2], orderedInt(1,2))" in {
      IsSorted.isSorted[Int](Array(2,1), orderedInt) shouldBe false
    }

    "should return true for IsSorted.isSorted(Array['banana','boat'], orderedString))" in {
      IsSorted.isSorted[String](Array("banana", "boat"), orderedString) shouldBe true
    }

    "should return true for IsSorted.isSorted(Array['boat','banana'], orderedString))" in {
      IsSorted.isSorted[String](Array("boat", "banana"), orderedString) shouldBe false
    }

    "should return true for sorted integer array with length > 2" in {
      IsSorted.isSorted[Int](Array(1,3,5), orderedInt) shouldBe true
    }

    "should return false for unsorted int array with length > 2" in {
      IsSorted.isSorted[Int](Array(1,6,4,2,7,9), orderedInt) shouldBe false
    }

    "should return true for sorted str array with length > 2" in {
      IsSorted.isSorted[String](Array("eggs", "sausage roll", "xylophone"), orderedString) shouldBe true
    }
    "should return false for unsorted str array with length > 2" in {
      IsSorted.isSorted[String](Array("pie", "eggs", "xylophone"), orderedString) shouldBe false
    }
  }
}
