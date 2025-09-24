import cats.data.{Validated, ValidatedNec}
import Day4PassportProcessing.Passport.{BirthYear, CountryId, ExpirationYear, EyeColor, HairColor, Height, IssueYear, PassportId, _}
import cats.Apply
import cats.syntax.option._
import Day4PassportProcessing.{Passport, PassportFieldValidationError}
import PassportFieldValidators.BirthYearValidators.{BirthYearValidator, PermissiveBirthYearValidator, StrictBirthYearValidator}
import PassportFieldValidators.CountryIdValidators.{CountryIdValidator, PermissiveCountryIdValidator}
import PassportFieldValidators.ExpirationYearValidators.{ExpirationYearValidator, PermissiveExpirationYearValidator, StrictExpirationYearValidator}
import PassportFieldValidators.EyeColorValidators.{EyeColorValidator, PermissiveEyeColorValidator, StrictEyeColorValidator}
import PassportFieldValidators.HairColorValidators.{HairColorValidator, PermissiveHairColorValidator, StrictHairColorValidator}
import PassportFieldValidators.HeightValidators.{HeightValidator, PermissiveHeightValidator, StrictHeightValidator}
import PassportFieldValidators.IssueYearValidators.{IssueYearValidator, PermissiveIssueYearValidator, StrictIssueYearValidator}
import PassportFieldValidators.PassportIdValidators.{PassportIdValidator, PermissivePassportIdValidator, StrictPassportIdValidator}

import scala.util.matching.Regex

object Day4PassportProcessing {

  /**
   * Represents a valid passport.
   */
  case class Passport(
    birthYear: BirthYear,
    issueYear: IssueYear,
    expirationYear: ExpirationYear,
    height: Height,
    hairColor: HairColor,
    eyeColor: EyeColor,
    passportId: PassportId,
    countryId: CountryId
  )

  object Passport {

    /**
     * Represents a validation for a passport field. It is either a non-empty-chain (NEC) of validation errors or a valid field.
     */
    type PassportFieldValidation[A <: PassportField[_, _]] = ValidatedNec[PassportFieldValidationError, A]

    /**
     * Represents a field of a passport.
     *
     * @tparam T Self type.
     * @tparam A Inner value type held by the field.
     */
    sealed trait PassportField[T <: PassportField[T, A], A] {

      /**
       * Returns the inner value held by this field.
       *
       * @return Value held by the field.
       */
      def value: A

      /**
       * Returns a human readable description of this field.
       *
       * @return human readable description of this field
       */
      def description: String
    }

    /**
     * Common trait extended by companion objects of passport fields.
     * These companion objects know what is the shortcode associated with their companion classes.
     *
     * @tparam A Type of the companion class of the object extending this trait.
     */
    sealed trait ShortCodeAware[A <: PassportField[_, _]] {
      def shortCode: String
    }

    /**
     * Represents the year of birth field of a passport.
     *
     * @param value They year of birth value.
     */
    case class BirthYear(value: Int) extends PassportField[BirthYear, Int] {
      override def description: String = "year of birth"
    }

    object BirthYear extends ShortCodeAware[BirthYear] {
      override val shortCode: String = "byr"
    }

    /**
     * Represents the year of issuance field of a passport.
     *
     * @param value They year of issuance value.
     */
    case class IssueYear(value: Int) extends PassportField[IssueYear, Int] {
      override def description: String = "year of issuance"
    }

    object IssueYear extends ShortCodeAware[IssueYear] {
      override val shortCode: String = "iyr"
    }

    /**
     * Represents the year of expiry field of a passport.
     *
     * @param value They year of expiry value.
     */
    case class ExpirationYear(value: Int) extends PassportField[ExpirationYear, Int] {
      override def description: String = "year of expiry"
    }

    object ExpirationYear extends ShortCodeAware[ExpirationYear] {
      override val shortCode: String = "eyr"
    }

    /**
     * Represents the height field of a passport.
     *
     * @param value They height value.
     */
    case class Height(value: String) extends PassportField[Height, String] {
      override def description: String = "height"
    }

    object Height extends ShortCodeAware[Height] {
      override val shortCode: String = "hgt"
    }

    /**
     * Represents the hair color field of a passport.
     *
     * @param value They hair color value.
     */
    case class HairColor(value: String) extends PassportField[HairColor, String] {
      override def description: String = "hair color"
    }

    object HairColor extends ShortCodeAware[HairColor] {
      override val shortCode: String = "hcl"
    }

    /**
     * Represents the eye color field of a passport.
     *
     * @param value They eye color value.
     */
    case class EyeColor(value: String) extends PassportField[EyeColor, String] {
      override def description: String = "eye color"
    }

    object EyeColor extends ShortCodeAware[EyeColor] {
      override val shortCode: String = "ecl"
    }

    /**
     * Represents the id field of a passport.
     *
     * @param value They id value.
     */
    case class PassportId(value: String) extends PassportField[PassportId, String] {
      override def description: String = "passport ID"
    }

    object PassportId extends ShortCodeAware[PassportId] {
      override val shortCode: String = "pid"
    }

    /**
     * Represents the country id field of a passport.
     *
     * @param value They country id value.
     */
    case class CountryId(value: Option[String]) extends PassportField[CountryId, Option[String]] {
      override def description: String = "country ID"
    }

    object CountryId extends ShortCodeAware[CountryId] {
      override val shortCode: String = "cid"
    }

    /**
     * Represents the validation of a whole passport. It is either a NEC of field validation errors or a valid passport.
     */
    type PassportValidation = ValidatedNec[PassportFieldValidationError, Passport]

    /**
     * Constructs a passport validation from the raw text representation of a passport.
     *
     * @param text Raw text representation of a passport: space separated shortcode:value pairs.
     * @return The passport validation.
     */
    def apply(text: String)(
      implicit
      birthYearValidator: BirthYearValidator,
      issueYearValidator: IssueYearValidator,
      expirationYearValidator: ExpirationYearValidator,
      heightValidator: HeightValidator,
      hairColorValidator: HairColorValidator,
      eyeColorValidator: EyeColorValidator,
      passportIdValidator: PassportIdValidator,
      countryIdValidator: CountryIdValidator
    ): PassportValidation = {
      val fieldMap: Map[String, String] = text
        .split("\\s")
        .map(_.split(':'))
        .map((elems: Array[String]) => (elems(0), elems(1)))
        .toMap

      Apply[PassportFieldValidation[*]]
        .map8(
          birthYearValidator(fieldMap),
          issueYearValidator(fieldMap),
          expirationYearValidator(fieldMap),
          heightValidator(fieldMap),
          hairColorValidator(fieldMap),
          eyeColorValidator(fieldMap),
          passportIdValidator(fieldMap),
          countryIdValidator(fieldMap)
        ) {
          Passport.apply
        }
    }
  }

  /**
   * Trait representing a passport field validation error.
   */
  sealed trait PassportFieldValidationError {
    def getErrorMessage: String
  }

  object PassportFieldValidationError {
    def missingField(field: String): PassportFieldValidationError = MissingField(field)

    def parseError(field: String, reason: String): PassportFieldValidationError = ParseError(field, reason)

    def unknownShortcode(code: String): PassportFieldValidationError = UnknownShortcode(code)
  }

  /**
   * Validation error for a missing field.
   *
   * @param field Shortcode that was missing.
   */
  case class MissingField(field: String) extends PassportFieldValidationError {
    override def getErrorMessage: String = s"Passport field '$field' is missing!"
  }

  /**
   * Validation error for being unable to construct the field value from the raw value.
   *
   * @param field  Affected shortcode.
   * @param reason Reason for failure.
   */
  case class ParseError(field: String, reason: String) extends PassportFieldValidationError {
    override def getErrorMessage: String = s"Passport field '$field' is invalid due to: $reason"
  }

  /**
   * Validation error for an unknown shortcode being encountered.
   *
   * @param code The unknown shortcode.
   */
  case class UnknownShortcode(code: String) extends PassportFieldValidationError {
    override def getErrorMessage: String = s"Shortcode '$code' is unknown!"
  }

}

/**
 * Object holding implicit validators for passport fields.
 */
object PassportFieldValidators {

  sealed trait PassportFieldValidator[A <: PassportField[A, _]] {

    implicit val shortCodeProvider: ShortCodeAware[A]

    def apply(value: String): PassportFieldValidation[A]

    def apply(input: Map[String, String]): PassportFieldValidation[A] =
      input
        .get(shortCodeProvider.shortCode)
        .map(this.apply)
        .getOrElse(this.missing)

    /**
     * Defines whether a missing field is valid or invalid for a particular shortcode.
     * The default implementation assumes an invalid field.
     *
     * @return A field validation
     */
    def missing: PassportFieldValidation[A] =
      Validated.invalidNec(PassportFieldValidationError.missingField(shortCodeProvider.shortCode))
  }

  object BirthYearValidators {

    trait BirthYearValidator extends PassportFieldValidator[BirthYear] {
      override implicit val shortCodeProvider: ShortCodeAware[BirthYear] = Passport.BirthYear
    }

    object PermissiveBirthYearValidator extends BirthYearValidator {
      override def apply(value: String): PassportFieldValidation[BirthYear] =
        value
          .toIntOption
          .map(Passport.BirthYear(_))
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "birth year must be an integer",
              field = shortCodeProvider.shortCode
            )
          }
    }

    object StrictBirthYearValidator extends BirthYearValidator {
      override def apply(value: String): PassportFieldValidation[BirthYear] =
        value
          .toIntOption
          .filter((y: Int) => y <= 2002 && y >= 1920)
          .map(Passport.BirthYear(_))
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "birth year must be a valid integer between 1920 and 2002",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object IssueYearValidators {

    trait IssueYearValidator extends PassportFieldValidator[IssueYear] {
      override implicit val shortCodeProvider: ShortCodeAware[IssueYear] = Passport.IssueYear
    }

    object PermissiveIssueYearValidator extends IssueYearValidator {
      override def apply(value: String): PassportFieldValidation[IssueYear] =
        value
          .toIntOption
          .map(Passport.IssueYear(_))
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "birth year must be an integer",
              field = shortCodeProvider.shortCode
            )
          }
    }

    object StrictIssueYearValidator extends IssueYearValidator {
      override def apply(value: String): PassportFieldValidation[IssueYear] =
        value
          .toIntOption
          .filter((y: Int) => y <= 2020 && y >= 2010)
          .map(Passport.IssueYear(_))
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "issue year must be a valid integer between 2010 and 2020",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object ExpirationYearValidators {

    trait ExpirationYearValidator extends PassportFieldValidator[ExpirationYear] {
      override implicit val shortCodeProvider: ShortCodeAware[ExpirationYear] = Passport.ExpirationYear
    }

    object PermissiveExpirationYearValidator extends ExpirationYearValidator {
      override def apply(value: String): PassportFieldValidation[ExpirationYear] =
        value
          .toIntOption
          .map(Passport.ExpirationYear(_))
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "expiration year must be an integer",
              field = shortCodeProvider.shortCode
            )
          }
    }

    object StrictExpirationYearValidator extends ExpirationYearValidator {
      override def apply(value: String): PassportFieldValidation[ExpirationYear] =
        value
          .toIntOption
          .filter((y: Int) => y <= 2030 && y >= 2020)
          .map(Passport.ExpirationYear(_))
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "expiration year must be a valid integer between 2020 and 2030",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object HeightValidators {

    trait HeightValidator extends PassportFieldValidator[Height] {
      override implicit val shortCodeProvider: ShortCodeAware[Height] = Passport.Height
    }

    object PermissiveHeightValidator extends HeightValidator {
      override def apply(value: String): PassportFieldValidation[Height] = Validated.validNec(Passport.Height(value))
    }

    object StrictHeightValidator extends HeightValidator {
      override def apply(value: String): PassportFieldValidation[Height] =
        "^(?<quantity>[1-9]\\d*)(?<unit>(in)|(cm))$"
          .r
          .findFirstMatchIn(value)
          .filter { m: Regex.Match =>
            val quantity: Int = m.group("quantity").toInt
            if (m.group("unit").equals("in")) quantity >= 59 && quantity <= 76
            else quantity >= 150 && quantity <= 193
          }
          .map { m: Regex.Match =>
            Passport.Height(m.group(0))
          }
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "height must be either an inch amount of [59, 76] or cm amount of [150, 193]",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object HairColorValidators {

    trait HairColorValidator extends PassportFieldValidator[HairColor] {
      override implicit val shortCodeProvider: ShortCodeAware[HairColor] = Passport.HairColor
    }

    object PermissiveHairColorValidator extends HairColorValidator {
      override def apply(value: String): PassportFieldValidation[HairColor] = Validated.validNec(Passport.HairColor(value))
    }

    object StrictHairColorValidator extends HairColorValidator {
      override def apply(value: String): PassportFieldValidation[HairColor] =
        "^#[0-9a-f]{6}$"
          .r
          .findFirstMatchIn(value)
          .map { _: Regex.Match =>
            Passport.HairColor(value)
          }
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "hair color must be of the format #[0-9a-f]{6}",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object EyeColorValidators {

    trait EyeColorValidator extends PassportFieldValidator[EyeColor] {
      override implicit val shortCodeProvider: ShortCodeAware[EyeColor] = Passport.EyeColor
    }

    object PermissiveEyeColorValidator extends EyeColorValidator {
      override def apply(value: String): PassportFieldValidation[EyeColor] = Validated.validNec(Passport.EyeColor(value))
    }

    object StrictEyeColorValidator extends EyeColorValidator {
      override def apply(value: String): PassportFieldValidation[EyeColor] =
        "^(amb)|(blu)|(brn)|(gry)|(grn)|(hzl)|(oth)$"
          .r
          .findFirstMatchIn(value)
          .map { _: Regex.Match =>
            Passport.EyeColor(value)
          }
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "eye color must be one of: amb, blu, brn, gry, grn, hzl, oth",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object PassportIdValidators {

    trait PassportIdValidator extends PassportFieldValidator[PassportId] {
      override implicit val shortCodeProvider: ShortCodeAware[PassportId] = Passport.PassportId
    }

    object PermissivePassportIdValidator extends PassportIdValidator {
      override def apply(value: String): PassportFieldValidation[PassportId] = Validated.validNec(Passport.PassportId(value))
    }

    object StrictPassportIdValidator extends PassportIdValidator {
      override def apply(value: String): PassportFieldValidation[PassportId] =
        "^\\d{9}$"
          .r
          .findFirstMatchIn(value)
          .map { _: Regex.Match =>
            Passport.PassportId(value)
          }
          .toValidNec {
            PassportFieldValidationError.parseError(
              reason = "passport ID must be a 9 digit number (leading zeroes permitted)",
              field = shortCodeProvider.shortCode
            )
          }
    }

  }

  object CountryIdValidators {

    trait CountryIdValidator extends PassportFieldValidator[CountryId] {
      override implicit val shortCodeProvider: ShortCodeAware[CountryId] = Passport.CountryId
    }

    object PermissiveCountryIdValidator extends CountryIdValidator {
      override def apply(value: String): PassportFieldValidation[CountryId] = Validated.validNec(Passport.CountryId(Some(value)))

      /**
       * For country ID a missing value is considered valid.
       *
       * @return Valid CountryId validation
       */
      override def missing: PassportFieldValidation[CountryId] = Validated.validNec(Passport.CountryId(None))
    }

    // no strict validator for country ID
  }

  // to import all strict or permissive validators with one import
  trait PermissiveValidatorInstances {
    implicit def birthYearValidator: BirthYearValidator = PermissiveBirthYearValidator

    implicit def issueYearValidator: IssueYearValidator = PermissiveIssueYearValidator

    implicit def expiryYearValidator: ExpirationYearValidator = PermissiveExpirationYearValidator

    implicit def heightValidator: HeightValidator = PermissiveHeightValidator

    implicit def hairColorValidator: HairColorValidator = PermissiveHairColorValidator

    implicit def eyeColorValidator: EyeColorValidator = PermissiveEyeColorValidator

    implicit def passportIdValidator: PassportIdValidator = PermissivePassportIdValidator

    implicit def countryIdValidator: CountryIdValidator = PermissiveCountryIdValidator
  }

  trait StrictValidatorInstances {
    implicit def birthYearValidator: BirthYearValidator = StrictBirthYearValidator

    implicit def issueYearValidator: IssueYearValidator = StrictIssueYearValidator

    implicit def expiryYearValidator: ExpirationYearValidator = StrictExpirationYearValidator

    implicit def heightValidator: HeightValidator = StrictHeightValidator

    implicit def hairColorValidator: HairColorValidator = StrictHairColorValidator

    implicit def eyeColorValidator: EyeColorValidator = StrictEyeColorValidator

    implicit def passportIdValidator: PassportIdValidator = StrictPassportIdValidator

    implicit def countryIdValidator: CountryIdValidator = PermissiveCountryIdValidator
  }

  object PermissiveValidators extends PermissiveValidatorInstances

  object StrictValidators extends StrictValidatorInstances

}