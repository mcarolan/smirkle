package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain.{Colour, Shape}

import scala.annotation.tailrec

case class Tile(shape: Shape, colour: Colour)

case class Position(x: Int, y: Int) {
  def left: Position = Position(x - 1, y)
}

case class TileGrid(elements: Map[Position, Tile]) {

  private def left(position: Position): List[Tile] = {
    @tailrec def inner(p: Position, acc: List[Tile]): List[Tile] =
      elements.get(p) match {
        case Some(tile) =>
          inner(p.left, acc :+ tile)
        case None =>
          acc
      }

    inner(position.left, Nil)
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
    val leftNeighbours = left(position)
    leftNeighbours match {
      case Nil if position != Position(0, 0) =>
        None
      case neighbours if valid(tile :: neighbours) =>
        Some(TileGrid(elements = elements + (position -> tile)))
      case _ =>
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
