package paut.aoc

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

import Console._

/** A class for representing the result of solving, or an attempt at solving, a problem from Advent of Code.
 * @param year the year which the problem is from
 * @param day the day in december which the problem is from
 * @param part one or two
 * @param solution a solution to the problem
 * @param time the time it took to solve the problem
 * @param timestamp the time the problem was solved
 * @param submitted whether the solution was submitted to and verified on the Advent of Code website
 */
case class Result(
    year: Int,
    day: Int,
    part: Int,
    solution: String,
    time: Double,
    timestamp: LocalDateTime = LocalDateTime.now(),
    submitted: Boolean = false
) {
  override def toString = {
    val from = s"AoC $year day $day - Part $part"
    val latest = s"Latest run: ${timestamp.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}"
    val dur = s"Fastest run: ${YELLOW}${fmt(time)}${RESET}"
    val sol = s"Solution: $solution"
    val stat = s"Status: ${if (submitted) s"${GREEN}Submitted${RESET}" else s"${RED}Not submitted${RESET}"}"
    
    s"""|--- $from ---
        |$latest
        |$dur
        |$sol
        |$stat
        |""".stripMargin

  }
  def raw = s"$year;$day;$part;$solution;${fmt(time)};$timestamp;$submitted"
}


object Result {
  /** Parses a string in the format `year;day;part;solution;time;timestamp;submitted` to a `Result` */
  def parse(str: String) = {
    val parts = str.split(";").toVector
    Result(
      year = parts(0).toInt,
      day = parts(1).toInt,
      part = parts(2).toInt,
      solution = parts(3),
      time = parts(4).toDouble,
      timestamp = LocalDateTime.parse(parts(5), DateTimeFormatter.ISO_LOCAL_DATE_TIME),
      submitted = parts(6).toBoolean
    )
  }

  /** Parses a string in the format `year;day;part;solution;time;timestamp;submitted` to a `Try[Result]`
   * @return `Success(Result)` if the string was successfully parsed, `Failure(e)` otherwise
   */
  def tryParse(str: String) = Try(parse(str))
}
