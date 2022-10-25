package paut.aoc

import java.time.LocalDate

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import Console._

trait ProblemOps[A] {
  val exampleSolution: ExampleSolution[A]

  protected def error(msg: String, trim: Boolean = false) =
    s"[${RED}!${RESET}] ${if (!trim) s"${RED}Something went wrong${RESET} " else ""}$msg"

  protected def success(msg: String) =
    s"[${GREEN}o${RESET}] ${GREEN}$msg${RESET}"

  protected def info(msg: String) =
    s"[${CYAN}+${RESET}] $msg"

  protected def tinyStack(e: Throwable) =
    s"""|[${RED}!${RESET}] ${e.getClass.getSimpleName}: ${e.getMessage}
        |${e.getStackTrace.toList
            .dropWhile(!_.toString.startsWith("aoc"))
            .takeWhile(!_.toString.startsWith("paut.aoc.Problem"))
            .init
            .map(s => s"      $s")
            .mkString("\n")}""".stripMargin

  protected def solvingError(name: String, exception: Throwable): Option[TimedEval[A]] = {
    println(s"""|${error(s"when solving the $name problem:")}
                |${tinyStack(exception)}""".stripMargin)
    None
  }

  protected def solvingFail(name: String, eval: TimedEval[A]): Option[TimedEval[A]] = {
    println(s"""|${error(s"${RED}Example failed!${RESET}", trim = true)}
                |    Expected: ${CYAN}${exampleSolution.sol}${RESET}
                |    Got:      ${YELLOW}${eval.result}${RESET}
                |    Time: ${fmt(eval.duration)} s""".stripMargin)
    None
  }

  protected def solvingSuccess(name: String, eval: TimedEval[A]): Option[TimedEval[A]] = {
    println(s"""|${success(s"${name.capitalize} solution found!")}
                |    Output: ${YELLOW}${eval.result}${RESET}
                |    Time: ${fmt(eval.duration)} s\n""".stripMargin)
    Some(eval)
  }

  protected def readFile(folder: String, year: Int, file: String) = {
    Try(os.read.lines(resources / "inputs" / folder / year.toString / file).toList) match {
      case Success(lines) => Some(lines)
      case Failure(err) => 
        println(s"""|${error(s"when reading $file in $folder/$year")}:
                    |    ${err}""".stripMargin)
        None
    }
  }
}