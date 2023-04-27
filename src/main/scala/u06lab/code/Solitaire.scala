package u06lab.code

object Solution:

  type Solution = Seq[Position]

  def placeMarks(board: Board)(init: Position): Iterable[Solution] =
    def placeMarkR(board: Board, n: Int): Iterable[Solution] = n match
      case 1 => Seq(Seq(init)).view
      case _ =>
        for
          sol <- placeMarkR(board, n - 1)
          pos <- board.positions
          if board.isLegal(pos, sol.head) && !sol.contains(pos)
        yield pos +: sol
    placeMarkR(board, board.positions.size)
  case class Position(x: Int, y: Int)

  case class Board(w: Int, h: Int):
    val positions: Seq[Position] = for x <- 0 until w; y <- 0 until h yield Position(x, y)
    // is legal if the new pos is distant 2 vertically or horizontally from the last pos or 1 diagonally
    def isLegal(p1: Position, p2: Position): Boolean =
      (math.abs(p1._1 - p2._1) == 2 && math.abs(p1._2 - p2._2) == 2)
        || (math.abs(p1._1 - p2._1) == 3 && math.abs(p1._2 - p2._2) == 0)
        || (math.abs(p1._1 - p2._1) == 0 && math.abs(p1._2 - p2._2) == 3)



/*
* Ad ogni iterazione, la funzione deve:
*   - prendere la posizione dell'ultima mossa per ogni Board Precedente
*   - calcolare le mosse possibili per ogni posizione

* */
object Solitaire extends App:
  import Solution.*
  private val board = Board(5, 5)
  private val init = Position(board.w / 2, board.h / 2)

  def render(solution: Seq[Position], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
                    number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")


  private val solutions = placeMarks(board)(init)
  println(solutions.size)   // 13272
  for s <- solutions
    do println(render(solution = s, width = 5, height = 7))