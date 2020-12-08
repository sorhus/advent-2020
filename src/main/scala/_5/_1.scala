package _5

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

import scala.annotation.tailrec

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .map(s => row(s) * 8 + col(s))
      .reduce(math.max)
  }

  def row(s: String): Int = find(s.take(7), 'F', 'B', 1, 128, 64)
  def col(s: String): Int = find(s.drop(7), 'L', 'R', 1, 8, 4)

  @tailrec def find(s: String, l: Char, h: Char, low: Int, high: Int, x: Int): Int = {
    s match {
      case "" => low -1
      case _ => s.codePointAt(0) match {
        case `l` => find(s.drop(1), l, h, low, high - x, x/2)
        case `h` => find(s.drop(1), l, h, low + x, high, x/2)
      }
    }
  }
}
