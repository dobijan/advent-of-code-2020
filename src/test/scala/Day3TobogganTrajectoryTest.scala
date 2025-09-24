import org.scalatest.wordspec.AnyWordSpec
import Day3TobogganTrajectory.{Track, TrackTemplate}

class Day3TobogganTrajectoryTest extends AnyWordSpec {

  "The input" when {
    "interpreted as a track" should {
      val deltaX = 1
      val deltaY = 3
      val template: TrackTemplate = TrackTemplate("src/main/resources/day3/input.txt")
      val track: Track = Track(
        template = template,
        deltaX = deltaX,
        deltaY = deltaY
      )
      "be at least as wide as it is long adjusted for step size" in {
        assert(track.xLen * deltaY <= track.yLen * deltaX)
      }

      "calculate the trajectory cost without error" in {
        val trajectoryCost: Int = track.trajectoryCost()
        println(s"The trajectory cost for deltaX = $deltaX and deltaY= $deltaY is $trajectoryCost")
      }
    }
  }

  "The input" when {
    "interpreted as a track" should {
      val template: TrackTemplate = TrackTemplate("src/main/resources/day3/input.txt")
      "calculate the product of costs for 5 slopes" in {
        val slopes: Seq[(Int, Int)] = Seq(
          (1, 1),
          (1, 3),
          (1, 5),
          (1, 7),
          (2, 1)
        )

        val product: Long = slopes
          .map { case (deltaX, deltaY) =>
            Track(
              template = template,
              deltaX = deltaX,
              deltaY = deltaY
            )
          }
          .map { track: Track =>
            val cost: Long = track.trajectoryCost()
            println(s"The trajectory cost for deltaX = ${track.deltaX} and deltaY= ${track.deltaY} is $cost")
            cost
          }
          .product

        println(s"The cost product of trajectories is $product")
      }
    }
  }
}
