package _2

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .map(Line.apply)
      .filter(valid)
      .map(_ => 1)
      .reduce(_ + _)
  }

  def valid(line: Line): Boolean = {
    val count = List(line.min, line.max)
      .map(i => line.password.charAt(i - 1))
      .count(_ == line.char)
    count == 1
  }
}
