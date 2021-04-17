package homework2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ChainTest extends AnyFlatSpec with Matchers {
  "++" should "append two chains" in {
    (Chain(1, 2) ++ Chain(3, 4)) shouldBe Chain(1, 2, 3, 4)
  }
}
