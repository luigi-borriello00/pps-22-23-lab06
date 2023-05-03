package u06lab.code

object Solution:

  type Position = (Int, Int)
  type Solution = Seq[Position]

  case class Board(w: Int, h: Int):
    val positions: Seq[Position] = for x <- 0 until w; y <- 0 until h yield (x, y)
    def center: Position = (w / 2, h / 2)

  def placeMarks(board: Board)(n: Int): Seq[Solution] = n match
    case 1 => Seq(Seq(board.center))
    case _ =>
      for
        solution <- placeMarks(board)(n - 1)
        pos <- board.positions
        if !solution.contains(pos) && isLegal(solution, pos)
      yield pos +: solution

  private def isLegal(s: Solution, p: Position): Boolean =
    s match
      case Nil => true
      case last :: _ =>
        (last._1 - p._1).abs + (last._2 - p._2).abs == 2

object Solitaire extends App:
  import Solution.*
  private val board = Board(5, 5)
  private val init = (board.w / 2, board.h / 2)

  def render(solution: Solution, width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")


  private val solutions = placeMarks(board)(13)
  solutions.foreach(s => println(render(s, board.w, board.h) + "\n"))
  println(s"Found ${solutions.size} solutions")