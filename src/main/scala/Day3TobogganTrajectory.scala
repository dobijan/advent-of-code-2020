import cats.data.State

import scala.annotation.tailrec

object Day3TobogganTrajectory {

  object Tile {
    def apply(char: Char): Option[Tile] =
      if (char.equals('.')) Some(Free)
      else if (char.equals('#')) Some(Tree)
      else None
  }

  sealed trait Tile {
    def isFree: Boolean
  }

  case object Tree extends Tile {
    override def isFree: Boolean = false
  }

  case object Free extends Tile {
    override def isFree: Boolean = true
  }

  case class Track(
    coordinates: Array[Array[Tile]],
    xLen: Int,
    yLen: Int,
    deltaX: Int,
    deltaY: Int
  ) {
    def isFree(x: Int, y: Int): Boolean =
      if (x >= xLen || y >= yLen) true
      else coordinates(x)(y).isFree

    def trajectoryCost(): Int = {
      type TerminalState = Boolean
      type TrajectoryState = State[Trajectory, TerminalState]

      case class Trajectory(x: Int = 0, y: Int = 0, obstacles: Int = 0) {
        def isTerminal: Boolean = x > xLen
      }

      def step(): TrajectoryState = State { state: Trajectory =>
        val newX: Int = state.x + deltaX
        val newY: Int = state.y + deltaY
        val newState: Trajectory = Trajectory(
          x = newX,
          y = newY,
          obstacles = state.obstacles + (if (isFree(newX, newY)) 0 else 1)
        )
        (newState, newState.isTerminal)
      }

      def calculateTrajectory(next: TrajectoryState = step()): TrajectoryState = {
        next.flatMap { isTerminal: Boolean =>
          if (isTerminal) next
          else calculateTrajectory(next)
        }
      }

      calculateTrajectory()
        .run(Trajectory())
        .value
        ._1
        .obstacles
    }
  }

  object Track {

    private def repeat(tiles: Array[Tile], horizontalRepetitionFactor: Int): Array[Tile] = {
      @tailrec
      def r(acc: Array[Tile], original: Array[Tile], times: Int): Array[Tile] =
        if (times <= 0) acc
        else r(Array.concat(acc, original), original, times - 1)

      r(tiles, tiles, horizontalRepetitionFactor)
    }

    def apply(
      template: TrackTemplate,
      deltaX: Int,
      deltaY: Int
    ): Track = {
      val horizontalRepetitionFactor: Int =
        ((template.tiles.length.toDouble / template.tiles(0).length.toDouble).ceil * (deltaY.toDouble / deltaX.toDouble).ceil).toInt
      val track: Array[Array[Tile]] = template.tiles.map(repeat(_, horizontalRepetitionFactor)).toArray
      Track(
        coordinates = track,
        xLen = track.length,
        yLen = track(0).length,
        deltaX = deltaX,
        deltaY = deltaY
      )
    }
  }

  case class TrackTemplate(
    tiles: Vector[Array[Tile]]
  )

  object TrackTemplate {
    private def parseLine(line: String): Array[Tile] =
      line.map(Tile(_)).filter(_.isDefined).map(_.get).toArray

    def apply(path: String): TrackTemplate =
      TrackTemplate(
        Utils
          .readLinesFromFile(path)
          .getOrElse(Vector())
          .map(parseLine)
      )
  }

}
