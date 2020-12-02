package template

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _2 extends App[String] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, String] = ???
}
