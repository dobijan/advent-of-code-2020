import org.scalatest.wordspec.AnyWordSpec

class Day2PasswordPhilosophyTest extends AnyWordSpec {

  "The provided input" when {
    val lines: Vector[String] = Utils
      .readLinesFromFile("src/main/resources/day2/input.txt")
      .getOrElse(Vector())
    "interpreted as passwords" should {
      val passwords: Vector[Day2PasswordPhilosophy.Password] =
        lines
          .map(Day2PasswordPhilosophy.parsePassword)
          .filter(_.isDefined)
          .map(_.get)
      "should contain as many passwords as lines" in {
        assert(lines.size.equals(passwords.size))
      }
      val validPasswords: Vector[Day2PasswordPhilosophy.Password] = passwords.filter(_.isValid)
      println(s"Of ${passwords.size} passwords ${validPasswords.size} are valid.")

      val alternativeValidPasswords: Vector[Day2PasswordPhilosophy.Password] = passwords.filter(_.isValidAlternative)
      println(s"Of ${passwords.size} passwords ${alternativeValidPasswords.size} are alternative valid.")
    }
  }
}
