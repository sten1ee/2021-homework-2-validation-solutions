package stj

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChainTest extends AnyFlatSpec with Matchers {
  val chain12 = Append(Singleton(1), Singleton(2))
  val chain34 = Append(Singleton(3), Singleton(4))
  val chain1234 = Append(chain12, chain34)

  "++" should "append two chains" in {
    chain12 ++ chain34 shouldBe chain1234
  }

  "==" should "test for values-and-their-order equivalence, not structural equivalence" in {
    chain1234 shouldBe chain1234.listify
    chain1234.listify should not equal Append(chain1234, Singleton(5)).listify
    chain1234 shouldBe Append(Append(Append(Singleton(1), Singleton(2)), Singleton(3)), Singleton(4))
  }

  "+:" should "prepend value to a Chain" in {
    0 +: chain12 shouldBe Append(Singleton(0), chain12)

    1 +: 2 +: Singleton(3) shouldBe Append(Singleton(1), Append(Singleton(2), Singleton(3)))
  }

  "+:" should "append value to a Chain" in {
    chain12 :+ 3 shouldBe Append(chain12, Singleton(3))
  }

  "foldLeft/Right" should "work as expected" in {
    chain1234.foldLeft[List[Int], Int](List.empty)((list: List[Int], el: Int) => el +: list) shouldBe List(4, 3, 2, 1)
    chain1234.foldRight[Int, List[Int]](List.empty)((el: Int, list: List[Int]) => el +: list) shouldBe List(1, 2, 3, 4)
  }

  "map" should "work as expected" in {
    chain1234.map(n => n.toString) shouldBe Append(Append(Singleton("1"), Singleton("2")), Append(Singleton("3"), Singleton("4")))
  }

  "flatMap" should "work as expected" in {
    chain34.flatMap(n => Append(Singleton(n), Singleton(n*n))) shouldBe Append(Append(Singleton(3), Singleton(9)), Append(Singleton(4), Singleton(16)))
  }
}
