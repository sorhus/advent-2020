package _2

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .map(Line.apply)
      .filter(valid)
      .map(_ => 1)
      .reduce(_ + _)
  }

  def valid(line: Line): Boolean = {
    val count = line.password.count(_ == line.char)
    count >= line.min && count <= line.max
  }

}

case class Line(min: Int, max: Int, char: Char, password: String)

object Line {
  def apply(s: String): Line = {
    val Array(range, char, password) = s.split(" ")
    val Array(min, max) = range.split("-")
    Line(min.toInt, max.toInt, char.charAt(0), password)
  }
}

