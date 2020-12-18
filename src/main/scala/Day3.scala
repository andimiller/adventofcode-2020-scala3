import cats._
import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp, Sync}
import cats.implicits._
import cats.parse._
import cats.data.{IndexedReaderWriterStateT, IndexedStateT, NonEmptyList, OptionT, RWST, StateT, WriterT}

import Predef._
import Console.println
import cats.mtl._
import cats.mtl.implicits._
import higherkindness.droste._
import higherkindness.droste.data.list.ConsF

import scala.util.Try

object Day3 extends IOApp:
  
  enum CellType:
    case Empty
    case Tree
  end CellType
  
  case class Grid(width: Int, contents: List[Cell]):
    def root: Option[Cell] = get(0, 0)
    def get(x: Int, y: Int): Option[Cell] =
      contents.get(
        x*width + (y % width)
      )
    def get(p: Position): Option[Cell] =
      get(p.line, p.row)
  end Grid
  object Grid:
    def fromList(cs: List[Cell]): Grid =
      val width = cs.map(_.p.row).max + 1
      Grid(width, cs)
  end Grid

  case class Cell(contents: CellType, p: Position):
    def x(using Grid): Int = p.line
    def y(using Grid): Int = p.row
    def moveVector(v: Position)(using Grid): Option[Cell] = implicitly[Grid].get(p combine v)
  end Cell
  
  case class Position(line: Int, row: Int)
  object Position:
    given Monoid[Position]:
      def empty = Position(0, 0)
      def combine(x: Position, y: Position): Position = 
        Position(x.line + y.line, x.row + y.row)
    end given 
  end Position
  
  object Parsers:
    val tree: Parser1[CellType] = Parser.char('#').as(CellType.Tree)
    val empty: Parser1[CellType] = Parser.char('.').as(CellType.Empty)
    val cell: Parser1[CellType] = tree orElse1 empty
    
    // row parser which keeps Position with State and builds up a List[Cell] with a Writer
    def indexedCellRow[F[_]: Monad](parse: F[Option[CellType]])(using s: Stateful[F, Position], l: Listen[F, List[Cell]]): F[Option[Cell]] =
      for
        index  <- s.get
        result <- parse
        _      <- result.traverse { _ => s.modify(p => p.copy(row = p.row+1)) }
        cell   = result.map { r => Cell(r, index) }
        _      <- cell.traverse { c => l.tell(List(c)) }
      yield
        cell  
        
    // document parser which keeps a position with State and knows how to read newlines, parsing the rows with the row parser
    def indexedCellDocument[F[_]: Monad](row: F[Unit], newline: F[Unit])(using s: Stateful[F, Position]): F[Boolean] =
      for 
        _        <- row
        _        <- newline
        position <- s.get
        lineWasEmpty = position.row == 0
        _        <- s.modify { p => p.copy(line = p.line + 1, row = 0) }
      yield 
        lineWasEmpty
    
    val documentRWST: RWST[Parser, Unit, List[Cell], Position, Boolean] = indexedCellDocument(
      indexedCellRow(
        RWST.liftF[Parser, Unit, List[Cell], Position, Option[CellType]](cell.?)
      ).iterateUntil(_.isEmpty).void,
      RWST.liftF[Parser, Unit, List[Cell], Position, Unit]((Parser.char('\r').? ~ Parser.char('\n').?).void)
    )

    val document: Parser[Grid] = documentRWST.iterateUntil(identity).runEmptyL(()).map(Grid.fromList)
  end Parsers
  
  // stateful traversal, we keep track of where we've been inside the Writer, and keep track of where we are in the State
  def traversal[F[_]: Monad](run: Cell => F[Option[Cell]])(using s: Stateful[F, Cell], l: Listen[F, List[Cell]]): F[Boolean] =
    for
      c  <- s.get
      ro <- run(c)
      _  <- ro.traverse { r => l.tell(List(r)) }
      _  <- ro.traverse(s.set)
    yield 
      ro.isDefined // return a boolean saying if we've successfully traversed, this is used to stop recursion 
      
  // implement the traversal using `moveVector` on the Cell class and pick our concrete monad transformer of RWST over Eval
  def travel(vector: Position)(using Grid): RWST[Eval, Unit, List[Cell], Cell, Boolean] = {
    traversal(
      { case (c: Cell) =>
        RWST.liftF[Eval, Unit, List[Cell], Cell, Option[Cell]](
          Eval.later { 
            c.moveVector(vector)
          }
        )
      }
    ).iterateWhile(identity) // just run it forever until the boolean is false
  }

  def program[F[_]: Monad: Console: Sync: ContextShift](using Blocker) = {
    for 
      data <- Helpers.readFile[F]("input3.txt")
      grid = Parsers.document.parseAll(
        data
      ).toOption.get
      root   = grid.root.get 
      vector = Position(1, 3) // this was defined in the input problem
      result <- Sync[F].delay {
        given Grid = grid
        travel(vector).runL((), root).value // let's go
      }
      trees = result.count(_.contents == CellType.Tree) // count how many trees we hit
      _     <- println(s"we hit $trees trees for problem 1")
      // and section 2
      allVectors = NonEmptyList.of(
        Position(1, 1),
        Position(1, 3),
        Position(1, 5),
        Position(1, 7),
        Position(2, 1)
      )
      results2 <- Sync[F].delay {
        allVectors.traverse { v =>
          given Grid = grid
          travel(v).runL((), root).map(_.count(_.contents == CellType.Tree).toLong)
        }.value
      }
      _ <- println(results2.toString)
      multSum = results2.reduce(_ * _)
      _ <- println(s"the multiplied total of these is $multSum")
    yield 
      ExitCode.Success
  }
  
  override def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { implicit _ =>
      program[IO].as(ExitCode.Success)
    }

end Day3
