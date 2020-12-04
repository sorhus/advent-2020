package _4

import cats.effect.IO
import common.{App, StreamUtils}
import fs2.Stream

import scala.util.Try

object _2 extends App[Int] with StreamUtils {

  override def process(input: Stream[IO, Byte]): Stream[IO, Int] = {
    input.through(toString)
      .split(_.isEmpty)
      .map(c => Stream.chunk[IO, String](c).reduce(_ + " " + _))
      .flatMap(_.map{ s =>
          val passports = s.split(" ").map{ e =>
            val Array(k, v) = e.split(":")
            (k, v)
          }
        val required = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
        passports.map(_._1).toSet.intersect(required).size == 7 &&
        passports.map(_._1).toSet.union(required + "cid").size == 8 &&
          passports.forall{
            case ("byr",v) => v.toInt >= 1920 && v.toInt <= 2002
            case ("iyr",v) => v.toInt >= 2010 && v.toInt <= 2020
            case ("eyr",v) => v.toInt >= 2020 && v.toInt <= 2030
            case ("hgt",v) => v.takeRight(2) match {
              case "cm" => v.dropRight(2).toInt >= 150 && v.dropRight(2).toInt <= 193
              case "in" => v.dropRight(2).toInt >= 59 && v.dropRight(2).toInt <= 76
              case _ => false
            }
            case ("hcl",v) => v.startsWith("#") && v.length == 7 && Try(Integer.parseInt(v.drop(1), 16)).isSuccess
            case ("ecl",v) => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(v)
            case ("pid",v) => v.length == 9 && Try(Integer.parseInt(v)).isSuccess
            case ("cid",v) => true
            case _ => false
          }
      }).fold(0){ case (count, b) =>
        if(b) count + 1 else count
      }
  }
}
