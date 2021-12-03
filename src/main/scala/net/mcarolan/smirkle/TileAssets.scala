package net.mcarolan.smirkle

import indigo.shared.datatypes.Rectangle
import indigo.{AssetName, AssetPath, AssetType}
import net.mcarolan.smirkle.Domain.Tile

object TileAssets {
  def assetName(tile: Tile): AssetName =
    AssetName(s"${tile.shape.entryName.toLowerCase}-${tile.colour.entryName.toLowerCase}")
  def assetPath(tile: Tile): AssetPath =
    AssetPath(s"assets/${tile.shape.entryName.toLowerCase}-${tile.colour.entryName.toLowerCase}.png")

  val tileRectangle: Rectangle =
    Rectangle(0, 0, 129, 129)

  lazy val assetTypes: Set[AssetType] =
    Tile.AllTiles.map { tile =>
      AssetType.Image(assetName(tile), assetPath(tile))
    }.toSet
}
