package paut.aoc

import java.time.LocalDate
import scala.util.Try

import Console._

trait Part { def toInt: Int }
case object NA  extends Part { val toInt = 0 }
case object Pt1 extends Part { val toInt = 1 }
case object Pt2 extends Part { val toInt = 2 }

/** An abstract class for solving a problem from Advent of Code. 
  * @param year the year the problem is from
  * @param day the day of the year the problem is from
  * @param part one or two
  * @param exampleSolution the expected solution for the example input
  * @param i what example file input to use, defaults to 1
  */
abstract class Problem[A]
  (val day: Int, val year: Int, val part: Part)
  (val exampleSolution: ExampleSolution[A], val i: Int = 1) 
  extends App with ProblemOps[A] {

  private val date = LocalDate.of(year, 12, day)

  def name: String
  def solve(data: List[String]): A
  
  override def toString = s"Day $day: $name ($year)"
  
  private def exampleInput(i: Int) = readFile("examples", year, f"$day%02d-$i.txt")
  private lazy val puzzleInput = readFile("puzzles", year, f"$day%02d.txt")

  private def timeSolve(data: Option[List[String]]) = data.map(d => TimedEval.time(solve(d)))

  private def solveExample = {
    Try(timeSolve(exampleInput(i))).fold(
      err => solvingError("example", err),
      opt => opt flatMap { exampleEval => 
        if (exampleEval.result == exampleSolution.sol) solvingSuccess("example", exampleEval)
        else solvingFail("example", exampleEval)
      }
    )
  }

  private def solvePuzzle = {
    println(info("Evaluating puzzle input..."))
    Try(timeSolve(puzzleInput)).fold(
      err => solvingError("puzzle", err),
      opt => opt.flatMap(puzzleEval => solvingSuccess("puzzle", puzzleEval))
    )
  }

  private def writeResult(eval: TimedEval[A]): Unit = {
    case object NoResult extends Throwable
    val resultsFile = resources / "results.csv"
    val newRes = Result(year, day, part.toInt, eval.result.toString, eval.duration)
    val now = java.time.LocalDateTime.now()

    def canUpdate(oldRes: Result) = 
      oldRes.solution == eval.result.toString && oldRes.time > eval.duration

    def updateFile(or: Result, nr: Result) = 
      os.write.over(resultsFile, os.read(resultsFile).replace(or.raw, nr.raw))

    if (!os.exists(resultsFile)) os.write(resultsFile, "", createFolders = true)

    val maybeRes = os
      .read.lines(resultsFile)
      .find(_.startsWith(s"$year;$day;$part"))
      .toRight(left = NoResult)
      .flatMap(str => Result.tryParse(str.trim).toEither)

    maybeRes match {
      case Left(NoResult) => os.write.append(resultsFile, s"${newRes.raw}\n")
      case Left(err) => 
        println(s"""|${error(s"when updating the results file:")}
                    |    ${err}""".stripMargin)
      case Right(oldRes) =>
        if (oldRes.submitted) 
          if (canUpdate(oldRes)) 
            updateFile(oldRes, oldRes.copy(time = eval.duration, timestamp = now))
        else
          updateFile(oldRes, newRes)
    }
  }

  /** Attempts to solve the given problem using the given data. 
    * @return The solution (if it was found), along with the time it took to solve it, or None if no solution was found or if the test case didn't pass, given that an example was provided.
    */
  def execute = {
    for (i <- 1 to 100) println()
    println(info(toString))

    val result = if (exampleSolution == Skip) {
      println(info("No example provided, skipping..."))
      solvePuzzle
    } else {
      println(info("Evaluating example input..."))
      solveExample.flatMap(_ => solvePuzzle)      
    }

    result.foreach(writeResult)
    result
  }

  val result = puzzleInput.flatMap(_ => execute)
}
