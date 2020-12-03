package _3

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _2 extends App[Long] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Long] = {
    val data = input.through(toString)
      .map(s => Stream.emits(s).repeat)
      .zipWithIndex
      .tail
    Stream.emits(List((1,1),(3,1),(5,1),(7,1),(1,2)))
      .flatMap{case (right, down) =>
        slope(data, right, down)
    }.map(_.toLong)
      .reduce(_ * _)
  }

  def slope(data: Stream[IO, (Stream[IO, Char], Long)], right: Int, down: Int): Stream[IO, Int] = {
    data.filter{ case (_, i) =>
      i % down == 0
    }.flatMap{ case (s, i) =>
      s.drop(right * i / down)
        .zipWith(Stream.emit('#').head)(_ == _)
    }.fold(0){ case (count, b) =>
      if (b) count + 1 else count
    }
  }
}
