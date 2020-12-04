package _2

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .map(Line.apply)
      .fold(0){ case(count, line) =>
        if(valid(line)) count + 1 else count
      }
  }

  def valid(line: Line): Boolean = {
    List(line.min, line.max)
      .map(i => line.password.charAt(i - 1))
      .count(_ == line.char) == 1
  }
}
