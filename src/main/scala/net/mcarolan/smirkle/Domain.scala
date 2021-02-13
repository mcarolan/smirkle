package net.mcarolan.smirkle
import cats.data.NonEmptyList
import enumeratum._

object Domain {

  sealed trait Colour extends EnumEntry

  object Colour extends Enum[Colour] {
    case object Red extends Colour
    case object Blue extends Colour
    case object Green extends Colour
    case object Yellow extends Colour
    case object Purple extends Colour
    case object Orange extends Colour

    override def values: IndexedSeq[Colour] = findValues
  }

  sealed trait Shape extends EnumEntry

  object Shape extends Enum[Shape] {
    case object One extends Shape
    case object Two extends Shape
    case object Three extends Shape
    case object Four extends Shape
    case object Five extends Shape
    case object Six extends Shape

    override def values: IndexedSeq[Shape] = findValues
  }

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

  case class PositionedTile(position: Position, tile: Tile)

  sealed trait InvalidPlacementsReason
  case object EmptyGridMustIncludeOrigin extends InvalidPlacementsReason
  case class PlacingOverCurrentlyPlacedTiles(tiles: NonEmptyList[PositionedTile]) extends InvalidPlacementsReason
  case class DuplicatePlacement(tiles: NonEmptyList[PositionedTile]) extends InvalidPlacementsReason
  case class CreatesInvalidLines(lines: NonEmptyList[NonEmptyList[PositionedTile]]) extends InvalidPlacementsReason
  case object AllPlacedTilesMustBeInALine extends InvalidPlacementsReason

}
