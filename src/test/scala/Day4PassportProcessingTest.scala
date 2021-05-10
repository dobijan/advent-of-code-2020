import org.scalatest.wordspec.AnyWordSpec
import Day4PassportProcessing.Passport.PassportValidation

class Day4PassportProcessingTest extends AnyWordSpec {


  "The input" when {
    "interpreted as a list of passports with permissive validation" should {
      import PassportFieldValidators.PermissiveValidators._
      val validations: Seq[PassportValidation] = Utils
        .readFile("src/main/resources/day4/input.txt")
        .getOrElse("")
        .split("\\n\\n")
        .map(Day4PassportProcessing.Passport(_))
      "contain valid and invalid passports" in {
        val invalidPassports: Seq[PassportValidation] = validations.filter(_.isInvalid)
        val validPassports: Seq[PassportValidation] = validations.filter(_.isValid)

        println(s"The file contains ${invalidPassports.size} leniently invalid passports.")
        println(s"The file contains ${validPassports.size} leniently valid passports.")
      }
    }

    "interpreted as a list of passports with strict validation" should {
      import PassportFieldValidators.StrictValidators._
      val validations: Seq[PassportValidation] = Utils
        .readFile("src/main/resources/day4/input.txt")
        .getOrElse("")
        .split("\\n\\n")
        .map(Day4PassportProcessing.Passport(_))
      "contain valid and invalid passports" in {
        val invalidPassports: Seq[PassportValidation] = validations.filter(_.isInvalid)
        val validPassports: Seq[PassportValidation] = validations.filter(_.isValid)

        println(s"The file contains ${invalidPassports.size} strictly invalid passports.")
        println(s"The file contains ${validPassports.size} strictly valid passports.")
      }
    }
  }
}
