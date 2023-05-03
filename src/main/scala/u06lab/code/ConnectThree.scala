package u06lab.code

import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] =
    board.find(d => d.x == x && d.y == y).map(_.player)

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    if board.count(_.x == x) > 3 then Option.empty else Option(board.count(_.x == x))

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board, x)
    yield board :+ Disk(x, y, player)

  def isThereVictory(board:Board, player: Player): Boolean =
    val disks = board.filter(_.player == player)
    val disksX = disks.map(_.x)
    val disksY = disks.map(_.y)
    val disksXY = disks.map(d => d.x + d.y)
    val disksX_Y = disks.map(d => d.x - d.y)
    disksX.distinct.size == 1 || disksY.distinct.size == 1 || disksXY.distinct.size == 1 || disksX_Y.distinct.size == 1

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match
    case 1 => LazyList(List())
    case _ =>
      for
        game <- computeAnyGame(player.other, moves - 1)
        board <- if game.nonEmpty then placeAnyDisk(game.head, player) else placeAnyDisk(List(), player)
        win = if game.nonEmpty then isThereVictory(game.last, player) else false
      yield if win then game else board +: game

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  println("EX 3: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4).foreach { g =>
    printBoards(g)
    println()
  }

  println(computeAnyGame(O, 4).size)

//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...


// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
