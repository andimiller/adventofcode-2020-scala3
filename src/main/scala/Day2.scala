import cats.data.NonEmptyList
import cats.implicits._
import cats.parse._
import fs2._
import cats.effect._

object Day2 extends IOApp:
  val int: Parser1[Int] = Numbers.digits1.map(_.toInt)
  val letter: Parser1[Char] = Parser.charIn('a' to 'z')

  implicit class StringSyntax(s: String):
    def charAtOption(i: Int): Option[Char] =
      try
        s.charAt(i).some
      catch
        case _: IndexOutOfBoundsException => none

  case class PasswordLine(lower: Int, upper: Int, char: Char, input: String):
    def isValid: Boolean =
      val occurances = input.count(_ == char)
      (lower <= occurances) && (occurances <= upper)
    def isValidPartTwo =
      (input.charAtOption(lower-1), input.charAtOption(upper-1)) match
        case (Some(c1), Some(c2))
          if c1 == char && c2 == char  => false
        case (Some(c), _) if c == char => true
        case (_, Some(c)) if c == char => true
        case _                         => false


  val passwordLine: Parser1[PasswordLine] =
    for {
      lower <- int
      _     <- Parser.char('-')
      upper <- int
      _     <- Parser.char(' ')
      char  <- letter
      _     <- Parser.string(": ")
      input <- letter.rep1.map(_.mkString_(""))
    } yield PasswordLine(lower, upper, char, input)

  val passwordFile: Parser1[NonEmptyList[PasswordLine]] =
    (passwordLine <* Parser.string("\r\n").?).rep1

  def program[F[_]: Sync: ContextShift](implicit b: Blocker) = for {
    data       <- Helpers.readFile("input2.txt")
    lines      <- Sync[F].fromEither(passwordFile.parseAll(data).leftMap(e => new Throwable(e.toString)))
    validCount = lines.count(_.isValid)
    _          <- Sync[F].delay { println(s"$validCount are valid") }
    validCount2 = lines.count(_.isValidPartTwo)
    _          <- Sync[F].delay { println(s"$validCount2 are valid") }
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { implicit _ =>
      program[IO].as(ExitCode.Success)
    }

