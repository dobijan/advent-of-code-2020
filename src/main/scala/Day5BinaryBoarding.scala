import scala.util.matching.Regex
import scala.collection.immutable.Range

object Day5BinaryBoarding {

  case class BoardingPass(
    row: Int,
    column: Int
  ) {
    val id: Int = row * 8 + column
  }

  object BoardingPass {
    def parse(text: String): Option[BoardingPass] = {
      "^(?<row>[FB]{7})(?<column>[LR]{3})$"
        .r
        .findFirstMatchIn(text.strip())
        .map { pass: Regex.Match =>
          BoardingPass(
            row = pass.group("row"),
            column = pass.group("column")
          )
        }
    }

    private def apply(row: String, column: String): BoardingPass =
      BoardingPass(
        row = findInRange(row, Range(0, 128)),
        column = findInRange(column, Range(0, 8))
      )

    private def findInRange(input: String, range: Range): Int =
      input.foldLeft(range) { (range, c) =>
        val newLength: Int = range.length / 2
        c match {
          case 'L' | 'F' => range.dropRight(newLength)
          case 'R' | 'B' => range.drop(newLength)
        }
      }.start
  }

}
