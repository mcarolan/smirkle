package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain.{Colour, Shape}

import scala.annotation.tailrec

case class Tile(shape: Shape, colour: Colour)

case class Position(x: Int, y: Int)
object Direction {
  def left(position: Position): Position =
    position.copy(x = position.x - 1, y = position.y)
  def right(position: Position): Position =
    position.copy(x = position.x + 1, y = position.y)
  def below(position: Position): Position =
    position.copy(x = position.x, y = position.y - 1)
  def above(position: Position): Position =
    position.copy(x = position.x, y = position.y + 1)
}

case class TileGrid(elements: Map[Position, Tile]) {

  private def neighbours(position: Position, direction: Position => Position): List[Tile] = {
    @tailrec def inner(p: Position, acc: List[Tile]): List[Tile] =
      elements.get(p) match {
        case Some(tile) =>
          inner(direction(p), acc :+ tile)
        case None =>
          acc
      }

    inner(direction(position), Nil)
  }

  private def valid(tiles: List[Tile]): Boolean = {
    val distinctColours = tiles.map(_.colour).toSet.size
    val distinctShapes = tiles.map(_.shape).toSet.size

    if (distinctColours == 1)
      distinctShapes == tiles.size
    else if (distinctShapes == 1)
      distinctColours == tiles.size
    else
      false
  }

  def place(tile: Tile, position: Position): Option[TileGrid] = {
    if (position == Position(0, 0) && elements.isEmpty)
      Some(TileGrid(elements = elements + (position -> tile)))
    else {
      val allNeighbours = List(
        neighbours(position, Direction.left),
        neighbours(position, Direction.right),
        neighbours(position, Direction.below),
        neighbours(position, Direction.above)
      )

      val totalNeighbourHoods = allNeighbours.count(_.nonEmpty)
      val validNeighbours = allNeighbours.filter(_.nonEmpty).count(neighbourhood => valid(tile :: neighbourhood))

      if (totalNeighbourHoods > 0 && totalNeighbourHoods == validNeighbours)
        Some(TileGrid(elements = elements + (position -> tile)))
      else
        None
    }
  }

  def at(position: Position): Option[Tile] =
    elements.get(position)

  val size: Int = elements.size
}

object TileGrid {
  val Initial: TileGrid = TileGrid(elements = Map.empty)
}
