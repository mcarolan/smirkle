package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain.{Colour, Shape}

case class Tile(shape: Shape, colour: Colour)

case class Position(x: Int, y: Int)

case class TileGrid(elements: Map[Position, Tile]) {

  private def left(position: Position): Option[Tile] =
    elements.get(Position(position.x - 1, position.y))

  def place(tile: Tile, position: Position): Option[TileGrid] = {
    left(position) match {
      case Some(Tile(s, c)) if s == tile.shape && c == tile.colour =>
        None
      case Some(Tile(s, c)) if s != tile.shape && c != tile.colour =>
        None
      case None if position != Position(0, 0) =>
        None
      case _ =>
        Some(TileGrid(elements = elements + (position -> tile)))
    }
  }

  def at(position: Position): Option[Tile] =
    elements.get(position)

  val size: Int = elements.size
}

object TileGrid {
  val Initial: TileGrid = TileGrid(elements = Map.empty)
}
