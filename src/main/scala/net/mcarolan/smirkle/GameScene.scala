package net.mcarolan.smirkle

import indigo.{Group, MouseEvent, Rectangle}
import indigo.scenes.{Lens, Scene, SceneName}
import indigo.shared.{FrameContext, Outcome}
import indigo.shared.events.{EventFilters, GlobalEvent, ViewportResize}
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.subsystems.SubSystem


object GameScene {
  val sceneName: SceneName = SceneName("Game")
}
class GameScene extends Scene[StartupData, Model, ViewModel] {
  override type SceneModel = Model
  override type SceneViewModel = GameSceneViewModel

  override def name: SceneName = GameScene.sceneName

  override def modelLens: Lens[Model, Model] = Lens.identity

  override def viewModelLens: Lens[ViewModel, ViewModel] =
    Lens[ViewModel, ViewModel](vm => {
      vm
    }, (vm1, vm2) => {
      vm2
    })

  override def eventFilters: EventFilters = EventFilters.AllowAll

  override def subSystems: Set[SubSystem] = Set.empty

  override def updateModel(context: FrameContext[StartupData], model: Model): GlobalEvent => Outcome[Model] =
    _ => Outcome(model)


  override def updateViewModel(context: FrameContext[StartupData], model: Model, viewModel: GameSceneViewModel): GlobalEvent => Outcome[GameSceneViewModel] = {
    case ViewportResize(viewport) =>
      Outcome(viewModel.copy(screenDimensions = viewport.asRectangle))
    case MouseEvent.MouseDown(_, _) =>
      Outcome(viewModel.copy(isMouseDown = true))
    case MouseEvent.MouseUp(_, _) =>
      Outcome(viewModel.copy(isMouseDown = false))
    case _ =>
      Outcome(viewModel)
  }

  override def present(context: FrameContext[StartupData], model: Model, viewModel: GameSceneViewModel): Outcome[SceneUpdateFragment] = {
        val tileSceneGraph = TileSceneGraph(model.tileGrid.elements.values.toList)
        val group = Group(tileSceneGraph.graphNodes).moveTo(viewModel.screenDimensions.center).scaleBy(0.5, 0.5)

        val fragment =
            if (viewModel.isMouseDown) {
              SceneUpdateFragment.empty
            } else {
              SceneUpdateFragment.empty.addGameLayerNodes(group)
            }
        Outcome(fragment)
      }
}
