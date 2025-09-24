import org.scalatest.wordspec.AnyWordSpec

class Day1ReportRepairTest extends AnyWordSpec {


  "The porovided numbers and the target sum" when {
    val numbers: Vector[Int] = Utils
      .readLinesFromFile("src/main/resources/day1/input.txt")
      .getOrElse(Vector())
      .map(_.toInt)
    val target: Int = 2020
    "passed to the pair finder" should {
      val pair: Option[(Int, Int)] = Day1ReportRepair.getTargetPair(numbers, target)
      "find the desired pair" in {
        assert(pair.isDefined)
      }
      "return a pair whose sum is indeed equal to the target" in {
        val isEqual: Boolean = pair
          .map { case (a, b) =>
            a + b
          }
          .exists { sum: Int =>
            sum.equals(target)
          }
        assert(isEqual)
      }
      val (a, b) = pair.get
      println(s"The pair is ($a, $b) and the product is ${a * b}")
    }
    "passed to the triple finder" should {
      val triple: Option[(Int, Int, Int)] = Day1ReportRepair.getTargetTriple(numbers, target)
      "find the desired triple" in {
        assert(triple.isDefined)
      }
      "return a triple whose sum is indeed equal to the target" in {
        val isEqual: Boolean = triple
          .map { case (a, b, c) =>
            a + b + c
          }
          .exists { sum: Int =>
            sum.equals(target)
          }
        assert(isEqual)
      }
      val (a, b, c) = triple.get
      println(s"The triple is ($a, $b, $c) and the product is ${a * b * c}")
    }
  }
}
