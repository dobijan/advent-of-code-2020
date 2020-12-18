import scala.io.{BufferedSource, Source}
import scala.language.reflectiveCalls
import scala.util.Try

object Utils {

  def readLinesFromFile(path: String): Try[Vector[String]] = {
    Try {
      using(Source.fromFile(path, "UTF-8")) { source: BufferedSource =>
        source.getLines().toVector
      }
    }
  }

  def readFile(path: String): Try[String] = {
    Try {
      using(Source.fromFile(path, "UTF-8")) { source: BufferedSource =>
        source.mkString
      }
    }
  }

  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B): B =
    try {
      f(resource)
    } finally {
      resource.close()
    }
}
