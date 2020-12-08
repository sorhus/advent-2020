package _7

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.{Pure, Stream}

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString).flatMap{ s =>
      val Array(first, rest) = s.split("contain")
      val Array(c11, c12, _) = first.split(" ")
      Stream.emits{
        rest.split(",").flatMap{ s =>
          val Array(count, c21, c22, _*) = s.trim.split(" ")
          if(c21 != "other") {
            Some(s"$c11 $c12" -> (s"$c21 $c22" -> count.toInt))
          } else {
            None
          }
        }
      }
    }.fold(Map.empty[String, Map[String, Int]]){ case (all, (x,y)) =>
      all.updated(x, if(all.contains(x)) all(x) + y else Map(y))
    }.map(reachable(_)() - 1)
  }

  def reachable(s: Map[String, Map[String, Int]])(name: String = "shiny gold", count: Int = 1): Int = {
    if(!s.contains(name)) {
      count
    } else {
      count + count * s(name).map(Function.tupled(reachable(s))).sum
    }
  }

}
