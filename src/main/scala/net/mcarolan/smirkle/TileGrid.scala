package net.mcarolan.smirkle

import cats.data.Validated.{Invalid, Valid}
import cats.data.NonEmptyList
import net.mcarolan.smirkle.Domain._
import cats.implicits._

import scala.annotation.tailrec

case class TileGrid(elements: Map[Position, PositionedTile]) {

  import TileGrid._

  def place(placements: NonEmptyList[PositionedTile]): PlacementResult[TileGrid] = {
    val overlapping: PlacementResult[Unit] =
      NonEmptyList.fromList(placements.filter(pt => elements.contains(pt.position))).map(PlacingOverCurrentlyPlacedTiles).toInvalidNel(())

    val emptyAtOrigin: PlacementResult[Unit] =
      if (size == 0 && !placements.exists(_.position == Position(0, 0)))
        Invalid(NonEmptyList.of(EmptyGridMustIncludeOrigin))
      else
        Valid(())

    val duplicatePlacement: PlacementResult[Unit] =
      NonEmptyList.fromList(placements.filter(p => placements.count(_.position == p.position) > 1)).map(DuplicatePlacement).toInvalidNel(())

    (overlapping, emptyAtOrigin, duplicatePlacement).tupled.andThen { _ =>
      val result = TileGrid(elements ++ placements.map(pt => pt.position -> pt).toList)
      val lines = allLines(result, placements)

      val invalidLines: PlacementResult[Unit] =
        NonEmptyList.fromList(lines.toList.filter(line => !isValidLine(line))).map(CreatesInvalidLines.apply).toInvalidNel(())

      val singleTilePlacementOnEmpty = size == 0 && placements.size == 1
      val allPlacedTilesMustBeInALine: PlacementResult[Unit] =
        if (!lines.exists(line => placements.forall(line.toList.contains)) && !singleTilePlacementOnEmpty)
          Invalid(NonEmptyList.of(AllPlacedTilesMustBeInALine))
        else
          Valid(())

      (invalidLines, allPlacedTilesMustBeInALine).tupled.as(result)
    }
  }

  def at(position: Position): Option[PositionedTile] =
    elements.get(position)

  val size: Int = elements.size

}

object TileGrid {
  val Initial: TileGrid = TileGrid(elements = Map.empty)

  private def neighbours(grid: TileGrid, position: Position, direction: Position => Position): Option[NonEmptyList[PositionedTile]] = {
    @tailrec def inner(p: Position, acc: Option[NonEmptyList[PositionedTile]]): Option[NonEmptyList[PositionedTile]] =
      (grid.at(p), acc) match {
        case (Some(pt), Some(l)) =>
          inner(direction(p), Some(l :+ pt))
        case (Some(pt), None) =>
          inner(direction(p), Some(NonEmptyList.of(pt)))
        case (None, _) =>
          acc
      }

    inner(direction(position), None)
  }

  private def buildLine(before: Option[NonEmptyList[PositionedTile]],
                        element: PositionedTile,
                        after: Option[NonEmptyList[PositionedTile]]): Option[NonEmptyList[PositionedTile]] =
    (before, after) match {
      case (Some(left), right) =>
        Some((left :+ element) ++ right.fold[List[PositionedTile]](List.empty)(_.toList))
      case (None, Some(right)) =>
        Some(right.prepend(element))
      case _ =>
        None
    }

  private def isValidLine(line: NonEmptyList[PositionedTile]): Boolean = {
    val distinctColours = line.map(_.tile.colour).toList.toSet.size
    val distinctShapes = line.map(_.tile.shape).toList.toSet.size

    if (distinctColours == 1)
      distinctShapes == line.size
    else if (distinctShapes == 1)
      distinctColours == line.size
    else
      false
  }

  private def allLines(grid: TileGrid, placements: NonEmptyList[PositionedTile]): Set[NonEmptyList[PositionedTile]] =
    placements.toList.flatMap { placement =>
      val left = neighbours(grid, placement.position, Direction.left)
      val right = neighbours(grid, placement.position, Direction.right)
      val above = neighbours(grid, placement.position, Direction.above)
      val below = neighbours(grid, placement.position, Direction.below)

      List(
        buildLine(left, placement, right),
        buildLine(above, placement, below)
      ).flatten
    }.toSet

}
