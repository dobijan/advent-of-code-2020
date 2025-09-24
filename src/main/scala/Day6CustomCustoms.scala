object Day6CustomCustoms {

  case class Passenger(answers: String) {
    val affirmatives: Set[Char] = Set.from(answers)
  }

  case class PassengerGroup(passengers: Seq[Passenger]) {
    val anyAffirmativeCount: Int = reduceWith(_ union _)
    val allAffirmativeCount: Int = reduceWith(_ intersect _)

    private def reduceWith(operation: (Set[Char], Set[Char]) => Set[Char]): Int =
      passengers.map(_.affirmatives).reduce(operation).size
  }

  object PassengerGroup {
    def parse(lines: Seq[String]): PassengerGroup =
      PassengerGroup(lines.map(Passenger))
  }

}
