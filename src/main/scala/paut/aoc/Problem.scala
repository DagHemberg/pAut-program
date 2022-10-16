package paut.aoc

import java.time.LocalDate
import java.util.Locale
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import Console._

/** An abstract class for solving a problem from Advent of Code. 
  * @param year the year the problem is from
  * @param day the day of the year the problem is from
  * @param part one or two
  * @param expectedExampleSolution the expected solution for the example input
  */
abstract class Problem[A]
  (val year: Int, val day: Int)
  (val part: Int)
  (val exampleSolution: ExampleSolution[A], i: Int = 1) extends App {

  def name: String
  def solve(data: List[String]): A
  val result = puzzleInput.flatMap(_ => execute)

  private val date = LocalDate.of(year, 12, day)

  private def timeSolve(data: Option[List[String]]) = data.map(d => TimedEval.time(solve(d)))
  
  override def toString = s"Day $day: $name ($year)"
  
  private def error(msg: String, trim: Boolean = false) =
    s"[${RED}!${RESET}] ${if (!trim) s"${RED}Something went wrong${RESET} " else ""}$msg"

  private def success(msg: String) =
    s"[${GREEN}o${RESET}] ${GREEN}$msg${RESET}"

  private def info(msg: String) =
    s"[${CYAN}+${RESET}] $msg"

  private def tinyStack(e: Throwable) =
    s"""|[${RED}!${RESET}] ${e.getClass.getSimpleName}: ${e.getMessage}
        |${e.getStackTrace.toList
            .dropWhile(!_.toString.startsWith("aoc"))
            .takeWhile(!_.toString.startsWith("paut.aoc.Problem"))
            .init
            .map(s => s"      $s")
            .mkString("\n")}""".stripMargin

  private def solvingError(name: String, exception: Throwable): Option[TimedEval[A]] = {
    println(s"""|${error(s"when solving the $name problem:")}
                |${tinyStack(exception)}""".stripMargin)
    None
  }

  private def solvingFail(name: String, eval: TimedEval[A]): Option[TimedEval[A]] = {
    println(s"""|${error(s"${RED}Example failed!${RESET}", trim = true)}
                |    Expected: ${CYAN}${exampleSolution.sol}${RESET}
                |    Actual:   ${YELLOW}${eval.result}${RESET}
                |    Time: ${fmt(eval.duration)} s""".stripMargin)
    None
  }

  private def solvingSuccess(name: String, eval: TimedEval[A]): Option[TimedEval[A]] = {
    println(s"""|${success(s"${name.capitalize} solution found!")}
                |    Output: ${YELLOW}${eval.result}${RESET}
                |    Time: ${fmt(eval.duration)} s\n""".stripMargin)
    Some(eval)
  }

  private def readFile(folder: String, year: Int, file: String) = {
    Try(os.read.lines(resources / "inputs" / folder / year.toString / file).toList) match {
      case Success(lines) => Some(lines)
      case Failure(err) => 
        println(s"""|${error(s"when reading $file in $folder/$year")}:
                    |    ${err}""".stripMargin)
        None
    }
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
  
  private def exampleInput(i: Int) = readFile("examples", year, f"$day%02d-$i.txt")
  private lazy val puzzleInput = readFile("puzzles", year, s"$day.txt")

  /** Attempts to solve the given problem using the given data. 
    * @return The solution (if it was found), along with the time it took to solve it, or None if no solution was found or if the test case didn't pass, given that an example was provided.
    */
  def execute = {
    for (i <- 1 to 100) println()
    println(info(toString))

    def solveExample(example: Option[List[String]], solution: A) = {
      Try(timeSolve(example)).fold(
        err => solvingError("example", err),
        opt => opt flatMap { exampleEval => 
          if (exampleEval.result == solution) solvingSuccess("example", exampleEval)
          else solvingFail("example", exampleEval)
        }
      )
    }

    def solvePuzzle = {
      println(info("Evaluating puzzle input..."))
      Try(timeSolve(puzzleInput)).fold(
        err => solvingError("puzzle", err),
        opt => opt flatMap { puzzleEval => solvingSuccess("puzzle", puzzleEval) }
      )
    }

    val result = exampleSolution match {
      case _: Skip.type => 
        println(info("No example provided, evaluating puzzle input..."))
        solvePuzzle
      case other => 
        println(info("Evaluating example input..."))
        solveExample(exampleInput(i), exampleSolution.sol).flatMap(_ => solvePuzzle)
    }

    result.foreach(writeResult)
    result
  }
}
