package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain.{Colour, Shape}
import org.scalatest.OptionValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TileGridTest extends AnyFunSuite with Matchers with OptionValues {

  test("can place a tile at Position(0, 0)") {
    val tile = Tile(Shape.One, Colour.Red)
    val position = Position(0, 0)
    val result = TileGrid.Initial.place(tile, position).value

    result should have size 1
    result.at(position).value shouldBe tile
  }

  test("can place a tile of same colour but different shape to Position(1, 0)") {
    val position1 = Position(0, 0)
    val tile1 = Tile(Shape.One, Colour.Red)

    val position2 = Position(1, 0)
    val tile2 = Tile(Shape.Two, Colour.Red)

    val result = (for {
      first <- TileGrid.Initial.place(tile1, position1)
      second <- first.place(tile2, position2)
    } yield second).value

    result should have size 2
    result.at(position1).value shouldBe tile1
    result.at(position2).value shouldBe tile2
  }

  test("cannot place a tile of same colour and same shape to Position(1, 0)") {
    val position1 = Position(0, 0)
    val tile1 = Tile(Shape.One, Colour.Red)

    val position2 = Position(1, 0)
    val tile2 = Tile(Shape.One, Colour.Red)

    val result = for {
      first <- TileGrid.Initial.place(tile1, position1)
      second <- first.place(tile2, position2)
    } yield second

    result shouldBe None
  }

  test("cannot place a tile of different colour and different shape to Position(1, 0)") {
    val position1 = Position(0, 0)
    val tile1 = Tile(Shape.One, Colour.Red)

    val position2 = Position(1, 0)
    val tile2 = Tile(Shape.Two, Colour.Blue)

    val result = for {
      first <- TileGrid.Initial.place(tile1, position1)
      second <- first.place(tile2, position2)
    } yield second

    result shouldBe None
  }

  test("can place a tile of different colour and same shape to Position(1, 0)") {
    val position1 = Position(0, 0)
    val tile1 = Tile(Shape.One, Colour.Red)

    val position2 = Position(1, 0)
    val tile2 = Tile(Shape.One, Colour.Blue)

    val result = (for {
      first <- TileGrid.Initial.place(tile1, position1)
      second <- first.place(tile2, position2)
    } yield second).value

    result should have size 2
    result.at(position1).value shouldBe tile1
    result.at(position2).value shouldBe tile2
  }

  test("cannot place in Position(1, 0) on an empty grid") {
    val result =
      TileGrid.Initial.place(Tile(Shape.One, Colour.Red), Position(1, 0))
    result shouldBe None
  }

  test("left neighbours can have same shape with different colours") {
    val position1 = Position(0, 0)
    val tile1 = Tile(Shape.One, Colour.Red)

    val position2 = Position(1, 0)
    val tile2 = Tile(Shape.One, Colour.Yellow)

    val position3 = Position(2, 0)
    val tile3 = Tile(Shape.One, Colour.Blue)

    val result = (for {
      first <- TileGrid.Initial.place(tile1, position1)
      second <- first.place(tile2, position2)
      third <- second.place(tile3, position3)
    } yield third).value

    result should have size 3
    result.at(position1).value shouldBe tile1
    result.at(position2).value shouldBe tile2
    result.at(position3).value shouldBe tile3
  }

  test("left neighbours cannot have same shape a repeated colour") {
    val position1 = Position(0, 0)
    val tile1 = Tile(Shape.One, Colour.Red)

    val position2 = Position(1, 0)
    val tile2 = Tile(Shape.One, Colour.Yellow)

    val position3 = Position(2, 0)
    val tile3 = Tile(Shape.One, Colour.Red)

    val result = for {
      first <- TileGrid.Initial.place(tile1, position1)
      second <- first.place(tile2, position2)
      third <- second.place(tile3, position3)
    } yield third

    result shouldBe None
  }

}
