package net.mcarolan.smirkle

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import net.mcarolan.smirkle.Domain._
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import cats.implicits._

class TileGridTest extends AnyFreeSpec with Matchers with OptionValues with TableDrivenPropertyChecks {

  "basic" - {
    "can place a tile at Position(0, 0)" in {
      val tile = Tile(Shape.One, Colour.Red)
      val position = Position(0, 0)
      val result = TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position, tile)))

      result shouldBe a[Valid[_]]
      val grid = result.toOption.value
      grid should have size 1
      grid.at(position).value shouldBe PositionedTile(position, tile)
    }

    "cannot place in Position(1, 0) on an empty grid" in {
      val result =
        TileGrid.Initial.place(NonEmptyList.of(PositionedTile(Position(1, 0), Tile(Shape.One, Colour.Red))))
      result shouldBe an[Invalid[_]]
    }
  }

  "neighbourhood size = 2" - {
    val positions =
      Table(
        ("description", "position 2"),
        ("to right", Position(1, 0)),
        ("above", Position(0, 1)),
        ("below", Position(0, -1)),
        ("to left", Position(-1, 0))
      )

    val tests =
      Table(
        ("description", "tile 1", "tile 2", "expect success?"),
        ("same colour different shape", Tile(Shape.One, Colour.Red), Tile(Shape.Two, Colour.Red), true),
        ("different colour same shape", Tile(Shape.One, Colour.Red), Tile(Shape.One, Colour.Blue), true),
        ("same colour same shape",  Tile(Shape.One, Colour.Red), Tile(Shape.One, Colour.Red), false),
        ("different colour different shape", Tile(Shape.One, Colour.Red), Tile(Shape.Two, Colour.Blue), false)
      )

    forAll(positions) {
      case (positionDescription, position2) =>
          forAll(tests) {
            case (testDescription, tile1, tile2, shouldBeSuccessful) =>
              s"3 tile placement - $testDescription - place $positionDescription" in {
                val position1 = Position(0, 0)

                val result = TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2)))

                if (shouldBeSuccessful) {
                  result shouldBe a[Valid[_]]
                  val grid = result.toOption.value
                  grid should have size 2
                  grid.at(position1).value shouldBe PositionedTile(position1, tile1)
                  grid.at(position2).value shouldBe PositionedTile(position2, tile2)
                }
                else
                  result shouldBe an[Invalid[_]]
              }
        }

        s"2 tile placement - place ${positionDescription} - cannot replace a placed tile" in {
          val position1 = Position(0, 0)
          val tile1 = Tile(Shape.One, Colour.Red)

          val tile2 = Tile(Shape.Two, Colour.Red)

          val result =
            TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2))).andThen { first =>
              first.place(NonEmptyList.of(PositionedTile(position2, tile2)))
            }

          result shouldBe an[Invalid[_]]
        }
    }
  }

  "neighbourhood size = 3" - {
    val positions =
      Table(
        ("description", "position 2", "position 3"),
        ("to right", Position(1, 0), Position(2, 0)),
        ("above", Position(0, 1), Position(0, 2)),
        ("below", Position(0, -1), Position(0, -2)),
        ("to left", Position(-1, 0), Position(-2, 0)),
        ("either side", Position(-1, 0), Position(1, 0)),
        ("above below", Position(0, 1), Position(0, -1))
      )

    val tests =
      Table(
        ("description", "tile 1", "tile 2", "tile 3", "expect success?"),
        ("same shape different colours", Tile(Shape.One, Colour.Red), Tile(Shape.One, Colour.Yellow), Tile(Shape.One, Colour.Blue), true),
        ("different shape same colours", Tile(Shape.One, Colour.Red), Tile(Shape.Two, Colour.Red), Tile(Shape.Three, Colour.Red), true),
        ("same shape repeated colour", Tile(Shape.One, Colour.Red), Tile(Shape.One, Colour.Yellow), Tile(Shape.One, Colour.Red), false),
        ("repeated shape same colour", Tile(Shape.One, Colour.Red), Tile(Shape.Two, Colour.Red), Tile(Shape.One, Colour.Red), false)
      )

    forAll(positions) {
      case (positionDescription, position2, position3) =>
          forAll(tests) {
            case (testDescription, tile1, tile2, tile3, shouldBeSuccessful) =>
              s"3 tile placement - $testDescription - place $positionDescription" in {
                val position1 = Position(0, 0)
                val result =
                  TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2))).andThen { first =>
                    first.place(NonEmptyList.of(PositionedTile(position3, tile3)))
                  }

                if (shouldBeSuccessful) {
                  result shouldBe a[Valid[_]]
                  val grid = result.toOption.value
                  grid should have size 3
                  grid.at(position1).value shouldBe PositionedTile(position1, tile1)
                  grid.at(position2).value shouldBe PositionedTile(position2, tile2)
                  grid.at(position3).value shouldBe PositionedTile(position3, tile3)
                }
                else
                  result shouldBe an[Invalid[_]]
              }
          }
    }
  }

  "advanced" - {
    "ensures all neighbourhoods are valid" in {
      val position1 = Position(0, 0)
      val tile1 = Tile(Shape.One, Colour.Red)

      val position2 = Position(1, 0)
      val tile2 = Tile(Shape.Three, Colour.Red)

      val position3 = Position(0, -1)
      val tile3 = Tile(Shape.Two, Colour.Red)

      val position4 = Position(1, -1)
      val tile4 = Tile(Shape.Three, Colour.Red)

      val result =
        TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2))).andThen { first =>
          first.place(NonEmptyList.of(PositionedTile(position3, tile3))).andThen { second =>
            second.place(NonEmptyList.of(PositionedTile(position4, tile4)))
          }
        }

      result shouldBe an[Invalid[_]]
    }

    "cannot 3 tile place with a gap" in {
      val position1 = Position(0, 0)
      val tile1 = Tile(Shape.One, Colour.Red)

      val position2 = Position(1, 0)
      val tile2 = Tile(Shape.Three, Colour.Red)

      val position3 = Position(3, 0)
      val tile3 = Tile(Shape.Four, Colour.Red)

      val result =
        TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2), PositionedTile(position3, tile3)))

      result shouldBe an[Invalid[_]]
    }

    "cannot make second placement not connected" in {
      val position1 = Position(0, 0)
      val tile1 = Tile(Shape.One, Colour.Red)

      val position2 = Position(1, 0)
      val tile2 = Tile(Shape.Three, Colour.Red)

      val position3 = Position(3, 0)
      val tile3 = Tile(Shape.Four, Colour.Red)

      val result =
        TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2))).andThen { first =>
          first.place(NonEmptyList.of(PositionedTile(position3, tile3)))
        }

      result shouldBe an[Invalid[_]]
    }

    "cannot place diagonally" in {
      val position1 = Position(0, 0)
      val tile1 = Tile(Shape.One, Colour.Red)

      val position2 = Position(1, 1)
      val tile2 = Tile(Shape.Three, Colour.Red)

      val result =
        TileGrid.Initial.place(NonEmptyList.of(PositionedTile(position1, tile1), PositionedTile(position2, tile2)))

      result shouldBe an[Invalid[_]]
    }
  }

}
