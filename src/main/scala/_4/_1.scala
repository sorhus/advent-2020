package _4

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .split(_.isEmpty)
      .map(c => Stream.chunk[IO, String](c).reduce(_ + " " + _))
      .flatMap(_.map{ s =>
          val passports = s.split(" ")
          passports.size == 8 || (passports.size == 7 && !passports.exists(_.split(":")(0) == "cid"))
      }).fold(0){ case (count, b) =>
        if(b) count + 1 else count
      }
  }
}
