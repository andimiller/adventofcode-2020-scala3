import cats.effect._

trait Console[F[_]]:
  def println(s: String): F[Unit]
end Console

object Console:
  def apply[F[_]](using Console[F]): Console[F] = implicitly
  def println[F[_]](s:String)(using Console[F]): F[Unit] = implicitly[Console[F]].println(s)
  given [F[_]](using Sync[F]) as Console[F]:
    override def println(s: String): F[Unit] = Sync[F].delay { Predef.println(s) }
end Console
