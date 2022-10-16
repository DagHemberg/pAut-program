package paut

import java.util.Locale

package object aoc {
  implicit class ExampleSolution[A](private [aoc] val sol: A)
    /** Used in the constructor of a subclass to the abstract Problem class 
    * in order to skip evaluation of the example input */
  case object Skip extends ExampleSolution[Any](None)

  private [aoc] val resources = os.home / ".paut" / "aoc"
  private [aoc] def fmt(time: Double) = String.format(Locale.US, "%.6f", time.asInstanceOf[java.lang.Double])
}
