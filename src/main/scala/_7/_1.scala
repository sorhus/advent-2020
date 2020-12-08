package _7

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

import scala.annotation.tailrec

object _1 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .flatMap{ s =>
        val Array(first, rest) = s.split("contain")
        val Array(c11, c12, _) = first.split(" ")
        Stream.emits{
          rest.split(",").flatMap { s =>
            val Array(_, c21, c22, _*) = s.trim.split(" ")
            if (c21 != "other")
              List(s"$c11 $c12" -> s"$c21 $c22")
            else
              Nil
          }
        }
      }.fold(Map.empty[String, Set[String]]){ case (all, (x,y)) =>
        all.updated(x, if(all.contains(x)) all(x) + y else Set(y) )
      }.map(s => s.keys.count(x => canReach(s, x)))
  }

  def canReach(s: Map[String, Set[String]], x: String): Boolean = {
    s.contains(x) && (s(x).contains("shiny gold") || s(x).exists(y => canReach(s, y)))
  }
}
