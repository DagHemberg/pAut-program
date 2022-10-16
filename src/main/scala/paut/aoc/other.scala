package paut.aoc

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
}

object Testing {
  def read(folder: String, year: String, day: String) = 
    os.read.lines(resources / "inputs" / folder / year / s"$day.txt").toList
}