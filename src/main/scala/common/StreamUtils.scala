package common

import cats.effect.IO
import fs2.{Stream, text}

trait StreamUtils {

  def toString(input: Stream[IO, Byte]): Stream[IO, String] =
    input.through(text.utf8Decode).through(text.lines)

  def toInt(input: Stream[IO, Byte]): Stream[IO, Int] = toString(input).map(_.toInt)

}
