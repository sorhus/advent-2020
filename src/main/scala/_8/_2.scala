package _8

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

import scala.annotation.tailrec

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    val lines = input.through(toString)
      .zipWithIndex

    lines.flatMap { case (_, i) =>
      (
        lines.take(i) ++
        lines.drop(i).head.map{ case(e, _) =>
          e.split(" ") match {
            case Array("nop", v) => (s"jmp $v", -1)
            case Array("jmp", v) => (s"nop $v", -1)
            case _ => (e, -1)
          }
        } ++
        lines.drop(i + 1)
      ).map(_._1)
      .fold(List.empty[String])((l, e) => l ++ List(e))
      .flatMap(l => rec(l))
    }
  }

  @tailrec def rec(elements: List[String], visited: Set[Int] = Set(), i: Int = 0, acc: Int = 0): Stream[IO, Int] = {
    if(visited.contains(i)) {
      Stream.empty
    } else if(i == elements.size) {
      Stream.emit(acc)
    } else {
      elements(i).split(" ") match {
        case Array("nop", _) => rec(elements, visited + i, i + 1, acc)
        case Array("jmp", d) => rec(elements, visited + i, i + d.toInt, acc)
        case Array("acc", d) => rec(elements, visited + i, i + 1, acc + d.toInt)
      }
    }
  }
}
