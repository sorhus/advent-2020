package common

import java.nio.file.Paths

import cats.Show
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.{Stream, io}

abstract class App[O: Show] extends IOApp {

  private val bufferSize = 4096

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use(b =>
      getInput(args, b)
        .through(process)
        .showLinesStdOutAsync(b)
        .compile
        .drain
        .as(ExitCode.Success)
    )
  }

  def getInput(args: List[String], blocker: Blocker): Stream[IO, Byte] = {
    args.headOption match {
      case Some(file) =>
        io.file.readAll[IO](Paths.get(file), blocker, bufferSize)
      case None =>
        val input = s"/${getClass.getName.split("\\.").head}.txt"
        io.readInputStream[IO](IO(getClass.getResourceAsStream(input)), bufferSize, blocker)
    }
  }

  def process(input: Stream[IO, Byte]): Stream[IO, O]

}