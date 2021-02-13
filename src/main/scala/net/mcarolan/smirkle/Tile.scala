package net.mcarolan.smirkle

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, NonEmptySet, Validated, ValidatedNel}
import net.mcarolan.smirkle.Domain.{Colour, Shape}
import cats.implicits._

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

  private def neighbours(grid: TileGrid, position: Position, direction: Position => Position): List[(Position, Tile)] = {
    @tailrec def inner(p: Position, acc: List[(Position, Tile)]): List[(Position, Tile)] =
      grid.at(p) match {
        case Some(tile) =>
          inner(direction(p), acc :+ p -> tile)
        case None =>
          acc
      }

    inner(direction(position), Nil)
  }

  private def isValidLine(line: NonEmptyList[(Position, Tile)]): Boolean = {
    val tiles = line.map { case (_, tile) => tile }
    val distinctColours = tiles.map(_.colour).toList.toSet.size
    val distinctShapes = tiles.map(_.shape).toList.toSet.size

    if (distinctColours == 1)
      distinctShapes == tiles.size
    else if (distinctShapes == 1)
      distinctColours == tiles.size
    else
      false
  }

  sealed trait InvalidPlacementsReason

  case object EmptyGridMustIncludeOrigin extends InvalidPlacementsReason

  case class PlacingOverCurrentlyPlacedTiles(tiles: NonEmptyList[(Position, Tile)]) extends InvalidPlacementsReason

  case class DuplicatePlacement(tiles: NonEmptyList[(Position, Tile)]) extends InvalidPlacementsReason

  case class CreatesInvalidLines(lines: NonEmptyList[NonEmptyList[(Position, Tile)]]) extends InvalidPlacementsReason

  case object AllPlacedTilesMustBeInALine extends InvalidPlacementsReason

  type PlacementResult[T] = ValidatedNel[InvalidPlacementsReason, T]

  private def buildLine(before: List[(Position, Tile)],
                        element: (Position, Tile),
                        after: List[(Position, Tile)]): Option[NonEmptyList[(Position, Tile)]] =
    (NonEmptyList.fromList(before), NonEmptyList.fromList(after)) match {
      case (Some(left), right) =>
        Some((left :+ element) ++ right.fold[List[(Position, Tile)]](List.empty)(_.toList))
      case (None, Some(right)) =>
        Some(right.prepend(element))
      case _ =>
        None
    }

  def place(placements: NonEmptyList[(Position, Tile)]): PlacementResult[TileGrid] = {
    val placementPositions = placements.map { case (pos, _) => pos }.toList

    val overlapping: PlacementResult[Unit] =
      NonEmptyList.fromList(placements.filter { case (pos, _) => elements.contains(pos) }).map(PlacingOverCurrentlyPlacedTiles).toInvalidNel(())

    val emptyAtOrigin: PlacementResult[Unit] =
      if (size == 0 && !placementPositions.contains(Position(0, 0)))
        Invalid(NonEmptyList.of(EmptyGridMustIncludeOrigin))
      else
        Valid(())

    val duplicatePlacement: PlacementResult[Unit] =
      NonEmptyList.fromList(placements.filter(p => placements.count(_ == p) > 1)).map(DuplicatePlacement).toInvalidNel(())

    (overlapping, emptyAtOrigin, duplicatePlacement).tupled.andThen { _ =>
      val result = TileGrid(elements ++ placements.toList)
      val allLines: Set[NonEmptyList[(Position, Tile)]] =
        placements.toList.flatMap {
          case placement@(position, _) =>
            val left = neighbours(result, position, Direction.left)
            val right = neighbours(result, position, Direction.right)
            val above = neighbours(result, position, Direction.above)
            val below = neighbours(result, position, Direction.below)

            List(
              buildLine(left, placement, right),
              buildLine(above, placement, below)
            ).flatten
        }.toSet

      val invalidLines: PlacementResult[Unit] =
        NonEmptyList.fromList(allLines.toList.filter(line => !isValidLine(line))).map(CreatesInvalidLines.apply).toInvalidNel(())

      val singleTilePlacementOnEmpty = size == 0 && placements.size == 1
      val allPlacedTilesMustBeInALine: PlacementResult[Unit] =
        if (!allLines.exists(line => placements.forall(line.toList.contains)) && !singleTilePlacementOnEmpty)
          Invalid(NonEmptyList.of(AllPlacedTilesMustBeInALine))
        else
          Valid(())

      (invalidLines, allPlacedTilesMustBeInALine).tupled.as(result)
    }
  }

  def at(position: Position): Option[Tile] =
    elements.get(position)

  val size: Int = elements.size
}

object TileGrid {
  val Initial: TileGrid = TileGrid(elements = Map.empty)
}
