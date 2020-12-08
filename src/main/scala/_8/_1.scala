package _8

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

import scala.annotation.tailrec

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .fold(List.empty[String])((l, e) => l ++ List(e))
      .map(l => rec(l))
  }

  @tailrec def rec(elements: List[String], visited: Set[Int] = Set(), i: Int = 0, acc: Int = 0): Int = {
    if(visited.contains(i)) {
      acc
    } else {
      elements(i).split(" ") match {
        case Array("nop", _) => rec(elements, visited + i, i + 1, acc)
        case Array("jmp", d) => rec(elements, visited + i, i + d.toInt, acc)
        case Array("acc", d) => rec(elements, visited + i, i + 1, acc + d.toInt)
      }
    }
  }
}
