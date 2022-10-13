package paut.aoc

trait Example[A] { val solution: A }

case object Skip extends Example[Any] { val solution = None }
case class PrimaryEx[A](solution: A) extends Example[A]
case class SecondaryEx[A](solution: A) extends Example[A]

/** A simple wrapper class that includes the result of an evaluation and the time (in seconds) it took to evaluate it
 * @param result The final evaluation
 * @param time Time elapsed while evaluating, in seconds
 */
case class TimedEval[A](duration: Double, result: A)
object TimedEval {
  /** Times the evaluation of a block of code */
  def time[A](block: => A): TimedEval[A] = {
    val start = System.nanoTime()
    val result = block
    val duration = (System.nanoTime() - start) / 1E9
    TimedEval(duration, result)
  }

  def logTime[A](block: => A) = {
    val t = time(block)
    println(t.duration)
    t.result
  }
}

object Testing {
  def read(folder: String, year: String, day: String) = {
    os.read.lines(os.home / ".paut" / "aoc" / "inputs" / folder / year / s"$day.txt").toList
  }
}