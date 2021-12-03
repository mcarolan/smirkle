package net.mcarolan.smirkle

import indigo.shared.scenegraph.SceneGraphNodePrimitive
import indigo.{Graphic, Material, Point}
import net.mcarolan.smirkle.Domain.PositionedTile


case class TileSceneGraph(positionedTiles: List[PositionedTile]) {

  lazy val graphNodes: List[SceneGraphNodePrimitive] =
    positionedTiles.map { positionedTile =>
      Graphic(TileAssets.tileRectangle, 1, Material.Textured(TileAssets.assetName(positionedTile.tile)))
        .withPosition(Point(TileAssets.tileRectangle.width * positionedTile.position.x, TileAssets.tileRectangle.height * positionedTile.position.y))
    }

}
