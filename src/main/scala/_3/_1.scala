package _3

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .map(s => Stream.emits(s).repeat)
      .zipWithIndex
      .tail
      .flatMap{ case (s, i) =>
        s.drop(3 * i)
          .zipWith(Stream.emit('#'))(_ == _)
    }.fold(0){ case (count, b) =>
      if (b) count + 1 else count
    }
  }
}
