package _6

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString).split(_.isEmpty).map { c =>
      c.foldLeft(Set.empty[Char])((all, e) => all ++ e.toCharArray).size
    }.reduce(_ + _)
  }
}
