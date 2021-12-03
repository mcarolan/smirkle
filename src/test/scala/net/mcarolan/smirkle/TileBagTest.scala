package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain.Colour._
import net.mcarolan.smirkle.Domain.Shape._
import net.mcarolan.smirkle.Domain.Tile
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TileBagTest extends AnyFreeSpec with Matchers {

  "Initial tile bag" - {
    "Should have 108 tiles" in {
      TileBag.Initial.contents should have size 108
    }
  }

  "Draw" - {
    "With a known seed" in {
      val seed = 123L

      val (hand, remaining) = TileBag.Initial.draw(6, seed)

      hand shouldBe List(Tile(Four, Yellow), Tile(Four, Purple), Tile(Five, Green), Tile(Four, Blue), Tile(Six, Green), Tile(Two, Orange))
      remaining.contents should have size 102
    }
  }

}
