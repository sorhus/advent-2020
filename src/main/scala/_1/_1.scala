package _1

import cats.effect.IO
import common.{App, StreamUtils}
import cats.implicits._
import fs2.Stream

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    val lines = input.through(toInt)
    lines.zipWithIndex.flatMap{ case (x, i) =>
      lines.drop(i + 1).filter(_ + x == 2020).map(_ * x)
    }
  }
}
