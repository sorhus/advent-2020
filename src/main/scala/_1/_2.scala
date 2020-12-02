package _1

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    val lines = input.through(toInt)
    lines.zipWithIndex.flatMap { case (x, i) =>
      lines.zipWithIndex.drop(i + 1).flatMap { case (y, j) =>
        lines.drop(j + 1).filter(_ + x + y == 2020).map(_ * x * y)
      }
    }
  }
}
