

object Day1ReportRepair {

  def getTargetPair(numbers: Vector[Int], target: Int): Option[(Int, Int)] = {
    for (first <- numbers.indices.dropRight(1)) {
      for (second <- numbers.indices.drop(1)) {
        val pair @ (a: Int, b: Int) = (numbers(first), numbers(second))
        if ((a + b).equals(target)) return Some(pair)
      }
    }
    None
  }

  def getTargetTriple(numbers: Vector[Int], target: Int): Option[(Int, Int, Int)] = {
    for (first <- numbers.indices.dropRight(2)) {
      for (second <- numbers.indices.drop(1).dropRight(1)) {
        for (third <- numbers.indices.drop(2)) {
          val triple @ (a: Int, b: Int, c: Int) = (numbers(first), numbers(second), numbers(third))
          if ((a + b + c).equals(target)) return Some(triple)
        }
      }
    }
    None
  }
}
