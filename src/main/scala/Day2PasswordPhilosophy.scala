import scala.util.matching.Regex

object Day2PasswordPhilosophy {

  case class Password(
    text: String,
    policy: Policy
  ) {
    def isValid: Boolean = policy.range.contains(text.count(_.equals(policy.letter)))

    def isValidAlternative: Boolean = text.charAt(policy.range.start - 1).equals(policy.letter) ^ text.charAt(policy.range.end - 1).equals(policy.letter)
  }

  case class Policy(
    letter: Char,
    range: Range
  )

  def parsePassword(password: String): Option[Password] = {
    val pattern: Regex = "^(?<lowerBound>\\d+)-(?<upperBound>\\d+) (?<letter>.): (?<password>.*)$".r
    pattern.findFirstMatchIn(password).map { (m: Regex.Match) =>
      Password(
        text = m.group("password"),
        policy = Policy(
          letter = m.group("letter").charAt(0),
          range = Range.inclusive(
            start = m.group("lowerBound").toInt,
            end = m.group("upperBound").toInt
          )
        )
      )
    }
  }
}
