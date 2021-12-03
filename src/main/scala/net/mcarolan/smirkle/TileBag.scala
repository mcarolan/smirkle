package net.mcarolan.smirkle

import net.mcarolan.smirkle.Domain._

import scala.util.Random

case class TileBag(contents: List[Tile]) {
  def draw(number: Int, seed: Long): (List[Tile], TileBag) = {
    val random = new Random(seed)
    val shuffled = random.shuffle(contents)

    (shuffled.take(number), TileBag(shuffled.drop(number)))
  }
}

object TileBag {
  val Initial: TileBag =
    TileBag(Tile.AllTiles ++ Tile.AllTiles ++ Tile.AllTiles)
}
