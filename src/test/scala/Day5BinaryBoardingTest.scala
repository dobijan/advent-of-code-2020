import Day5BinaryBoarding.BoardingPass
import org.scalatest.wordspec.AnyWordSpec

import org.scalatest.Assertions._

class Day5BinaryBoardingTest extends AnyWordSpec {

  "The test example" when {

    "interpreted as a boarding pass" should {

      "calculate the id correctly" in {
        val example: String = "FBFBBFFRLR"

        val pass: Option[BoardingPass] = BoardingPass.parse(example)

        assert {
          pass.isDefined
        }

        assertResult(357) {
          pass.get.id
        }
      }

      "fail to parse the pass" in {
        val example: String = "FOOBAR"

        assert {
          BoardingPass.parse(example).isEmpty
        }
      }
    }
  }

  "The input puzzle" when {

    "interpreted as a list of boarding passes" should {

      val passes: Vector[BoardingPass] = Utils
        .readLinesFromFile("src/main/resources/day5/input.txt")
        .getOrElse(Vector())
        .flatMap(BoardingPass.parse)

      "have one ID as the largest" in {

        val largest: BoardingPass = passes.maxBy(_.id)

        println(s"The largest passport is: $largest with id ${largest.id}")
      }

      "have a missing id in the middle" in {

        val missingId: Int = passes
          .sortBy(_.id)
          .foldLeft((0, 1)) { case ((candidateId, previousId), observedPass) =>
            (
              if (observedPass.id - previousId > 1)
                previousId + 1
              else
                candidateId,
              observedPass.id
            )
          }
          ._1

        println(s"The missing ID is $missingId")
      }
    }
  }
}
