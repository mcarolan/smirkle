package net.mcarolan.smirkle

import indigo._
import indigo.scenes.{Scene, SceneName}
import indigoextras.subsystems.FPSCounter
import net.mcarolan.smirkle.Domain.{Position, PositionedTile, Tile}

import scala.scalajs.js.annotation.JSExportTopLevel

case class Model(
                  tileBag: TileBag,
                  hand: List[Tile],
                  tileGrid: TileGrid
                )

case class ViewModel
(
  screenDimensions: Rectangle,
  gameSceneViewModel: GameSceneViewModel
)

case class GameSceneViewModel(
                               isMouseDown: Boolean
                             )

case class StartupData(
                      seed: Long,
                      screenDimensions: Rectangle
                      )

final case class BootInformation(assetPath: String, screenDimensions: Rectangle)

@JSExportTopLevel("IndigoGame")
object HelloIndigo extends IndigoGame[BootInformation, StartupData, Model, ViewModel] {

  val magnification = 1

//  override val config: GameConfig =
//    GameConfig
//      .default
//      .withClearColor(RGBA.White)
//      .withMagnification(1)
//      .withViewport(GameViewport.at720p)
//
//  override val assets: Set[AssetType] =
//    TileAssets.assetTypes
//
//  override val fonts: Set[FontInfo] =
//    Set.empty
//
//  override val animations: Set[Animation] =
//    Set.empty
//
//  override def setup(assetCollection: AssetCollection, dice: Dice): Outcome[Startup[StartupData]] =
//    Outcome(Startup.Success(StartupData(dice.seed)))
//
//  override def initialModel(startupData: StartupData): Outcome[Model] = {
//    val (hand, tileBag) = TileBag.Initial.draw(6, startupData.seed)
//    val tileGrid =
//      TileGrid(Tile.AllTiles.zipWithIndex.map { case (tile, i) =>
//        val position = Position(i % 6, i / 6)
//        position -> PositionedTile(position, tile)
//      }.toMap)
//    Outcome(Model(tileBag, hand, tileGrid, 1, config.viewport))
//  }
//
//  override def updateModel(context: FrameContext[StartupData], model: Model): GlobalEvent => Outcome[Model] = {
//    case ViewportResize(gameViewPort) =>
//      Outcome(model.copy(viewport = gameViewPort))
//    case _ =>
//      Outcome(model)
//  }
//
//  override def present(context: FrameContext[StartupData], model: Model): Outcome[SceneUpdateFragment] = {
//    val tileSceneGraph = TileSceneGraph(model.tileGrid.elements.values.toList)
//    val group = Group(tileSceneGraph.graphNodes).moveTo(model.viewport.center).scaleBy(0.5, 0.5)
//    Outcome(SceneUpdateFragment.empty.addGameLayerNodes(group))
//  }
  override def scenes(bootData: BootInformation): NonEmptyList[Scene[StartupData, Model, ViewModel]] =
    NonEmptyList(
      new GameScene
    )

  override def initialScene(bootData: BootInformation): Option[SceneName] = None

  override def eventFilters: EventFilters =
    EventFilters.AllowAll

  override def boot(flags: Map[String, String]): Outcome[BootResult[BootInformation]] =
    Outcome {
      val assetPath: String =
        flags.getOrElse("baseUrl", "")

      val config =
        GameConfig.default
          .withViewport(GameViewport.at720p)
          .withMagnification(2)
          .withClearColor(RGBA.White)

      BootResult(
        config,
        BootInformation(assetPath, config.screenDimensions)
      ).withAssets(TileAssets.assetTypes)
    }

  override def setup(bootData: BootInformation, assetCollection: AssetCollection, dice: Dice): Outcome[Startup[StartupData]] =
    Outcome(Startup.Success(StartupData(dice.seed, bootData.screenDimensions)))

  override def initialModel(startupData: StartupData): Outcome[Model] =
    Outcome {
      val (hand, tileBag) = TileBag.Initial.draw(6, startupData.seed)
      val tileGrid =
            TileGrid(Tile.AllTiles.zipWithIndex.map { case (tile, i) =>
              val position = Position(i % 6, i / 6)
              position -> PositionedTile(position, tile)
            }.toMap)
      Model(tileBag, hand, tileGrid)
    }

  override def initialViewModel(startupData: StartupData, model: Model): Outcome[ViewModel] =
    Outcome(ViewModel(startupData.screenDimensions, isMouseDown = false))

  override def updateModel(context: FrameContext[StartupData], model: Model): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)

  override def updateViewModel(context: FrameContext[StartupData], model: Model, viewModel: ViewModel): GlobalEvent => Outcome[ViewModel] =
    _ => Outcome(viewModel)

  override def present(context: FrameContext[StartupData], model: Model, viewModel: ViewModel): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)
}
