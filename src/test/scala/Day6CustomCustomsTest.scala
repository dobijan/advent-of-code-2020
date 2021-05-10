import Day6CustomCustoms.PassengerGroup
import org.scalatest.wordspec.AnyWordSpec

class Day6CustomCustomsTest extends AnyWordSpec {

  "The puzzle input" when {

    "interpreted as a list of passenger groups" should {

      val passengerGroups: Seq[PassengerGroup] = Utils
        .readFile("src/main/resources/day6/input.txt")
        .getOrElse("")
        .split("\n\n")
        .map(_.split("\n"))
        .map(PassengerGroup.parse(_))

      "sum the any affirmative answer count for all groups" in {

        val sum: Int = passengerGroups
          .map(_.anyAffirmativeCount)
          .sum

        println(s"Sum of any affirmative counts: $sum")
      }

      "sum the all affirmative count for all groups" in {
        val sum: Int = passengerGroups
          .map(_.allAffirmativeCount)
          .sum

        println(s"Sum of all affirmative counts: $sum")
      }
    }
  }
}
