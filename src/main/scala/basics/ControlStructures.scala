package basics

import basics.ControlStructures.Command.{Average, Divide, Max, Min, Sum}
import basics.ControlStructures.Result.{AverageResult, DivideResult, MaxResult, MinResult, SumResult}

import java.text.DecimalFormat
import scala.io.Source
import scala.util.Try

object ControlStructures {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  type ErrorMessage = String

  sealed trait Result
  object Result {

    private val numberFormatter = new DecimalFormat("0.###")

    private def formatNumber(number: Double) = numberFormatter.format(number)

    private def formatNumbers(numbers: List[Double]): List[String] = numbers.map(formatNumber)

    final case class DivideResult(command: Divide, result: Double) extends Result {
      override def toString: String =
        s"${formatNumber(command.dividend)} divided by ${formatNumber(command.divisor)} is ${formatNumber(result)}"
    }

    final case class SumResult(command: Sum, result: Double) extends Result {
      override def toString: String = s"the sum of ${formatNumbers(command.numbers).mkString(" ")} is ${formatNumber(result)}"
    }

    final case class AverageResult(command: Average, result: Double) extends Result {
      override def toString: String = s"the average of ${formatNumbers(command.numbers).mkString(" ")} is ${formatNumber(result)}"
    }

    final case class MinResult(command: Min, result: Double) extends Result {
      override def toString: String = s"the minimum of ${formatNumbers(command.numbers).mkString(" ")} is ${formatNumber(result)}"
    }

    final case class MaxResult(command: Max, result: Double) extends Result {
      override def toString: String = s"the maximum of ${formatNumbers(command.numbers).mkString(" ")} is ${formatNumber(result)}"
    }
  }

  private def allElementsAreNumbers(elements: List[String]): Boolean =
    elements.forall { element => Try(element.toDouble).isSuccess }

  def parseCommand(x: String): Either[ErrorMessage, Command] = x.split("\\s+").toList match {
    case "divide" :: numbers =>
      if (numbers.size == 2 && allElementsAreNumbers(numbers)) Right(Divide(numbers.head.toDouble, numbers(1).toDouble))
      else Left(s"Unexpected params for divide command - ${numbers.mkString(" ")}")
    case "sum" :: numbers =>
      if (allElementsAreNumbers(numbers)) Right(Sum(numbers.map(_.toDouble)))
      else Left(s"Unexpected params for sum command - $numbers")
    case "average" :: numbers =>
      if (allElementsAreNumbers(numbers)) Right(Average(numbers.map(_.toDouble)))
      else Left(s"Unexpected params for average command - $numbers")
    case "max" :: numbers =>
      if (allElementsAreNumbers(numbers)) Right(Max(numbers.map(_.toDouble)))
      else Left(s"Unexpected params for max command - $numbers")
    case "min" :: numbers =>
      if (allElementsAreNumbers(numbers)) Right(Min(numbers.map(_.toDouble)))
      else Left(s"Unexpected input: $x")
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case Divide(_, 0) => Left("Division by zero")
    case x@Divide(dividend, divisor) => Right(DivideResult(x, dividend / divisor))
    case x@Sum(numbers) => Right(SumResult(x, numbers.sum))
    case x@Average(numbers) => Right(AverageResult(x, numbers.sum / numbers.length))
    case x@Max(numbers) => Right(MaxResult(x, numbers.max))
    case x@Min(numbers) => Right(MinResult(x, numbers.min))
  }

  def renderResult(x: Result): String = x.toString

  def process(x: String): String = (
    for {
      command <- parseCommand(x)
      calculateResult <- calculate(command)
    } yield calculateResult) match {
    case Right(result) => renderResult(result)
    case Left(message) => s"Error: $message"
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
