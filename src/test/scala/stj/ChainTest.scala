package stj

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChainTest extends AnyFlatSpec with Matchers {
  "++" should "append two chains" in {
    val chain12 = Append(Singleton(1), Singleton(2))
    val chain34 = Append(Singleton(3), Singleton(4))
    val chain1234 = Append(chain12, chain34)
    //(Append(1, 2) ++ Append(3, 4)) shouldBe Chain(1, 2, 3, 4)
    0 +: chain12 shouldBe Append(Singleton(0), chain12)

    chain12 :+ 3 shouldBe Append(chain12, Singleton(3))

    1 +: 2 +: Singleton(3) shouldBe Append(Singleton(1), Append(Singleton(2), Singleton(3)))

    chain1234.foldLeft[List[Int], Int](List.empty)((list: List[Int], el: Int) => el +: list) shouldBe List(4, 3, 2, 1)
    chain1234.foldRight[Int, List[Int]](List.empty)((el: Int, list: List[Int]) => el +: list) shouldBe List(1, 2 , 3, 4)

    chain1234.map(n => n.toString) shouldBe Append(Append(Singleton("1"), Singleton("2")), Append(Singleton("3"), Singleton("4")))

    chain34.flatMap(n => Append(Singleton(n), Singleton(n*n))) shouldBe Append(Append(Singleton(3), Singleton(9)), Append(Singleton(4), Singleton(16)))
  }
}
