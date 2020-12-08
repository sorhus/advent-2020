package _6

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString).split(_.isEmpty).map { c =>
      val all = c.foldLeft(Set.empty[String])((all, e) => all + e)
      all.map(_.toCharArray)
        .reduce(_ ++ _)
        .toSet
        .count(c => all.forall(p => p.contains(c)))
    }.reduce(_ + _)
  }
}
