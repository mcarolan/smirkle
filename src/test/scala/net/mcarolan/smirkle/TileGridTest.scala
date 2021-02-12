package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain.{Colour, Shape}
import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TileGridTest extends AnyFreeSpec with Matchers with OptionValues {

  "basic" - {
    "can place a tile at Position(0, 0)" in {
      val tile = Tile(Shape.One, Colour.Red)
      val position = Position(0, 0)
      val result = TileGrid.Initial.place(tile, position).value

      result should have size 1
      result.at(position).value shouldBe tile
    }

    "cannot place in Position(1, 0) on an empty grid" in {
      val result =
        TileGrid.Initial.place(Tile(Shape.One, Colour.Red), Position(1, 0))
      result shouldBe None
    }
  }

  "neighbourhood size = 2" - {
    "place to right" - {
      "can same colour different shape" in {
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

      "can different colour same shape" in {
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

      "cannot same colour same shape" in {
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

      "cannot different colour different shape" in {
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
    }

    "place above" - {
      "can same colour different shape" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.Two, Colour.Red)

        val result = (for {
          first <- TileGrid.Initial.place(tile1, position1)
          second <- first.place(tile2, position2)
        } yield second).value

        result should have size 2
        result.at(position1).value shouldBe tile1
        result.at(position2).value shouldBe tile2
      }

      "can different colour same shape" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.One, Colour.Blue)

        val result = (for {
          first <- TileGrid.Initial.place(tile1, position1)
          second <- first.place(tile2, position2)
        } yield second).value

        result should have size 2
        result.at(position1).value shouldBe tile1
        result.at(position2).value shouldBe tile2
      }

      "cannot same colour same shape" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.One, Colour.Red)

        val result = for {
          first <- TileGrid.Initial.place(tile1, position1)
          second <- first.place(tile2, position2)
        } yield second

        result shouldBe None
      }

      "cannot different colour different shape" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.Two, Colour.Blue)

        val result = for {
          first <- TileGrid.Initial.place(tile1, position1)
          second <- first.place(tile2, position2)
        } yield second

        result shouldBe None
      }
    }
  }

  "neighbourhood size = 3" - {
    "place to right" - {
      "can have same shape different colours" in {
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

      "can have different shape same colours" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(1, 0)
        val tile2 = Tile(Shape.Two, Colour.Red)

        val position3 = Position(2, 0)
        val tile3 = Tile(Shape.Three, Colour.Red)

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

      "cannot have same shape repeated colour" in {
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

      "cannot have same colour repeated shape" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(1, 0)
        val tile2 = Tile(Shape.Two, Colour.Red)

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
    "place above" - {
      "can have same shape different colours" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.One, Colour.Yellow)

        val position3 = Position(0, 2)
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

      "can have different shape same colours" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.Two, Colour.Red)

        val position3 = Position(0, 2)
        val tile3 = Tile(Shape.Three, Colour.Red)

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

      "cannot have same shape repeated colour" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.One, Colour.Yellow)

        val position3 = Position(0, 2)
        val tile3 = Tile(Shape.One, Colour.Red)

        val result = for {
          first <- TileGrid.Initial.place(tile1, position1)
          second <- first.place(tile2, position2)
          third <- second.place(tile3, position3)
        } yield third

        result shouldBe None
      }

      "cannot have same colour repeated shape" in {
        val position1 = Position(0, 0)
        val tile1 = Tile(Shape.One, Colour.Red)

        val position2 = Position(0, 1)
        val tile2 = Tile(Shape.Two, Colour.Red)

        val position3 = Position(0, 2)
        val tile3 = Tile(Shape.One, Colour.Red)

        val result = for {
          first <- TileGrid.Initial.place(tile1, position1)
          second <- first.place(tile2, position2)
          third <- second.place(tile3, position3)
        } yield third

        result shouldBe None
      }
    }
  }

}
