package paut.aoc

import scala.util.{Try, Success, Failure}
import Console._
import java.time.LocalDate

/** An abstract class for solving a problem from Advent of Code. 
  * @param year the year the problem is from
  * @param day the day of the year the problem is from
  * @param expectedExampleSolution the expected solution for the example input
  */
abstract class Problem[A]
  (val year: Int, val day: Int, val part: Int)
  (val example: Example[A]) extends App {

  private val date = LocalDate.of(year, 12, day)
  def name: String
  def solve(data: List[String]): A
  
  private def timeSolve(data: Option[List[String]]) = data.map(d => TimedEval.time(solve(d)))
  override def toString = s"Day $day: $name ($year)"

  private def printlln(x: Any = "")(implicit printResult: Boolean) = 
    if (printResult) println(x)
  
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

  private def solvingError(name: String, exception: Throwable)(implicit b: Boolean): Option[TimedEval[A]] = {
    printlln(s"""|${error(s"when solving the $name problem:")}
                 |${tinyStack(exception)}""".stripMargin)
    None
  }

  private def solvingFail(name: String, eval: TimedEval[A])(implicit b: Boolean): Option[TimedEval[A]] = {
    printlln(f"""|${error(s"${RED}Example failed!${RESET}", trim = true)}
                 |    Expected: ${CYAN}${example.solution}${RESET}
                 |    Actual:   ${YELLOW}${eval.result}${RESET}
                 |    Time: ${eval.duration}%2.6f s""".stripMargin)
    None
  }

  private def solvingSuccess(name: String, eval: TimedEval[A])(implicit b: Boolean): Option[TimedEval[A]] = {
    printlln(f"""|${success(s"${name.capitalize} solution found!")}
                 |    Output: ${YELLOW}${eval.result}${RESET}
                 |    Time: ${eval.duration}%2.6f s%n""".stripMargin)
    Some(eval)
  }

  private val resources = os.pwd / "src" / "main" / "resources"

  private def readFile(folder: String, year: Int, file: String) = {
    Try(os.read.lines(resources / "input" / folder / year.toString / file).toList) match {
      case Success(lines) => Some(lines)
      case Failure(err) => {
        printlln(s"""|${error(s"when reading $file in $folder/$year")}:
                     |    ${err}""".stripMargin)(true)
        None
      }
    }
  }

  private def writeResult(eval: TimedEval[A])(implicit printresult: Boolean): Unit = {
    val resultsFile = os.home / ".paut" / "aoc" / "results.csv"
    val newRes = Result(year, day, part, eval.result.toString, eval.duration)
    val now = java.time.LocalDateTime.now()

    if (!os.exists(resultsFile)) os.write(resultsFile, "", createFolders = true)

    os.read.lines(resultsFile).toList
      .find(_.startsWith(s"$year;$day;$part"))
      .map(str => Result.parse(str.trim)) match {
        case None => os.write.append(resultsFile, newRes.raw)
        case Some(result) => {
          os.write.over(
            resultsFile, 
            os.read(resultsFile).replace(result.raw, 
              if (result.submitted && result.solution == eval.result.toString && result.time > eval.duration) {
                result.copy(timestamp = now, time = eval.duration).raw
              } else {
                newRes.raw
              }
            )
          )
        }
      }
  }

  private val primaryExampleInput = readFile("examples", year, s"$day-primary.txt")
  private val secondaryExampleInput = readFile("examples", year, s"$day-secondary.txt")
  private val puzzleInput = readFile("puzzles", year, s"$day.txt")

  /** Attempts to solve the given problem using the given data. 
    * @param printResult Given boolean that decides whether to print the result or not. Defaults to true. 
    * @return The solution (if it was found), along with the time it took to solve it, or None if no solution was found or if the test case didn't pass, given that an example was provided.
    */
  def execute(implicit printResult: Boolean = true) = {
    for (i <- 1 to 100) printlln()
    printlln(info(toString))

    def solveExample(example: Option[List[String]], solution: A) = {
      Try(timeSolve(example)).fold(
        err => solvingError("example", err),
        opt => opt flatMap {
          exampleEval => {
            if (exampleEval.result == solution) solvingSuccess("example", exampleEval)
            else solvingFail("example", exampleEval)
          }
        }
      )
    }

    def solvePuzzle = {
      printlln(info("Evaluating puzzle input..."))
      Try(timeSolve(puzzleInput)).fold(
        err => solvingError("puzzle", err),
        opt => opt flatMap { puzzleEval => solvingSuccess("puzzle", puzzleEval) }
      )
    }

    val result = example match {
      case skip: Skip.type => {
        printlln(info("No example provided, evaluating puzzle input..."))
        solvePuzzle
      }
      case ex => {
        printlln(info("Evaluating example input..."))
        val (exampleInput, solution) = ex match {
          case PrimaryEx(solution) => (primaryExampleInput, solution)
          case SecondaryEx(solution) => (secondaryExampleInput, solution)
        }
        solveExample(exampleInput, solution).flatMap(_ => solvePuzzle)
      }
    }

    result.foreach(writeResult)
    result
  }

  val result = puzzleInput.flatMap(_ => execute)
}
