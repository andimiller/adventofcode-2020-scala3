import cats.data.NonEmptyList
import cats.implicits._
import cats.parse._
import fs2._
import cats.effect._

import java.nio.file.{Path, Paths}

object Helpers:
  def readFile[F[_]: Sync: ContextShift](name: String)(implicit b: Blocker): F[String] =
    fs2.io.file
      .readAll(Path.of(s"./src/main/resources/$name"), b, 1024)
      .through(fs2.text.utf8Decode[F])
      .compile
      .foldMonoid

object Day1 extends IOApp:
  val parser: Parser1[NonEmptyList[BigInt]] = 
    Parser.rep1Sep(Numbers.bigInt, 1, Parser.string("\r\n"))
  
  @inline val combinations: Int = 3
  
  def program[F[_]: Sync: ContextShift](implicit b: Blocker) =
    for {
      data    <- Helpers.readFile("input1.txt")
      ints    <- Sync[F].fromEither(parser.parseAll(data).leftMap(e => new Throwable(e.toString)))
      result  = ints.toList.combinations(combinations).find(_.sum === BigInt("2020"))
      _       <- Sync[F].delay { println(s"result: $result")}
      result2 = result.map(ints => ints.reduce(_ * _))
      _       <- Sync[F].delay { println(s"result2: $result2")}
    } yield ()
  

  override def run(args: List[String]): IO[ExitCode] = 
    Blocker[IO].use { implicit _ =>
      program[IO].as(ExitCode.Success)
    }
    
  