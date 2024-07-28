import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scala.util.Random
import javafx.animation.AnimationTimer

object Main extends JFXApp {

  val gridWidth = 100
  val gridHeight = 100
  val cellSize = 5
  val initialTunas = 500
  val initialSharks = 75
  val tunaBreedTime = 3
  val sharkBreedTime = 8
  val sharkInitialEnergy = 5

  sealed trait Cell
  case object Empty extends Cell
  case class Tuna(age: Int) extends Cell
  case class Shark(age: Int, energy: Int) extends Cell

  type Grid = Vector[Vector[Cell]]

  def initializeGrid(): Grid = {
    val emptyGrid = Vector.fill(gridWidth, gridHeight)(Empty: Cell)
    val gridWithTunas = placeEntities(emptyGrid, initialTunas, () => Tuna(0))
    placeEntities(
      gridWithTunas,
      initialSharks,
      () => Shark(0, sharkInitialEnergy)
    )
  }

  def placeEntities(grid: Grid, count: Int, entity: () => Cell): Grid = {
    val random = new Random()
    (1 to count).foldLeft(grid) { (currentGrid, _) =>
      Iterator
        .continually((random.nextInt(gridWidth), random.nextInt(gridHeight)))
        .find { case (x, y) => currentGrid(x)(y) == Empty }
        .map { case (x, y) =>
          currentGrid.updated(x, currentGrid(x).updated(y, entity()))
        }
        .getOrElse(currentGrid)
    }
  }

  def updateGrid(grid: Grid): Grid = {
    def updateCell(updatedGrid: Grid, x: Int, y: Int): Grid = {
      updatedGrid(x)(y) match {
        case Tuna(age)          => updateTuna(x, y, age, updatedGrid)
        case Shark(age, energy) => updateShark(x, y, age, energy, updatedGrid)
        case Empty              => updatedGrid
      }
    }

    (0 until gridWidth).foldLeft(grid) { (currentGrid, x) =>
      (0 until gridHeight).foldLeft(currentGrid) { (updatedGrid, y) =>
        updateCell(updatedGrid, x, y)
      }
    }
  }

  def updateTuna(x: Int, y: Int, age: Int, grid: Grid): Grid = {
    val neighbors = getEmptyNeighbors(x, y, grid)
    val (newX, newY) =
      if (neighbors.nonEmpty) neighbors(Random.nextInt(neighbors.size))
      else (x, y)
    val newGrid = grid.updated(newX, grid(newX).updated(newY, Tuna(age + 1)))
    if (age >= tunaBreedTime) newGrid.updated(x, newGrid(x).updated(y, Tuna(0)))
    else newGrid.updated(x, newGrid(x).updated(y, Empty))
  }

  def updateShark(x: Int, y: Int, age: Int, energy: Int, grid: Grid): Grid = {
    val newEnergy = energy - 1
    if (newEnergy <= 0) {
      grid.updated(x, grid(x).updated(y, Empty))
    } else {
      val neighbors = getNeighbors(x, y)
      val tunaNeighbors = neighbors.filter { case (nx, ny) =>
        grid(nx)(ny).isInstanceOf[Tuna]
      }

      if (tunaNeighbors.nonEmpty) {
        val (newX, newY) = tunaNeighbors(Random.nextInt(tunaNeighbors.size))
        val newGrid = grid.updated(
          newX,
          grid(newX).updated(newY, Shark(age + 1, sharkInitialEnergy))
        )
        if (age >= sharkBreedTime)
          newGrid.updated(x, newGrid(x).updated(y, Shark(0, newEnergy)))
        else newGrid
      } else {
        val (newX, newY) = moveRandomly(x, y, grid)
        val newGrid = grid.updated(
          newX,
          grid(newX).updated(newY, Shark(age + 1, newEnergy))
        )
        if ((newX, newY) != (x, y))
          newGrid.updated(x, newGrid(x).updated(y, Empty))
        else newGrid
      }
    }
  }

  def moveRandomly(x: Int, y: Int, grid: Grid): (Int, Int) = {
    val neighbors = getEmptyNeighbors(x, y, grid)
    if (neighbors.nonEmpty) neighbors(Random.nextInt(neighbors.size))
    else (x, y)
  }

  def getEmptyNeighbors(x: Int, y: Int, grid: Grid): Seq[(Int, Int)] = {
    getNeighbors(x, y).filter { case (nx, ny) => grid(nx)(ny) == Empty }
  }

  def getNeighbors(x: Int, y: Int): Seq[(Int, Int)] = {
    for {
      dx <- -1 to 1
      dy <- -1 to 1
      if dx != 0 || dy != 0
      nx = (x + dx + gridWidth) % gridWidth
      ny = (y + dy + gridHeight) % gridHeight
    } yield (nx, ny)
  }

  def draw(canvas: Canvas, grid: Grid): Unit = {
    val gc = canvas.graphicsContext2D
    gc.fill = Color.Black
    gc.fillRect(0, 0, gridWidth * cellSize, gridHeight * cellSize)

    for (x <- 0 until gridWidth; y <- 0 until gridHeight) {
      grid(x)(y) match {
        case Tuna(_) =>
          gc.fill = Color.Green
          gc.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
        case Shark(_, _) =>
          gc.fill = Color.Red
          gc.fillRect(x * cellSize, y * cellSize, cellSize, cellSize)
        case Empty =>
      }
    }
  }

  def runSimulation(initialGrid: Grid, canvas: Canvas): Unit = {
    def loop(grid: Grid, lastUpdateTime: Long): Unit = {
      new AnimationTimer {
        override def handle(now: Long): Unit = {
          if (now - lastUpdateTime >= 0.25e9) {
            val updatedGrid = updateGrid(grid)
            draw(canvas, updatedGrid)
            stop()
            loop(updatedGrid, now)
          }
        }
      }.start()
    }
    loop(initialGrid, System.nanoTime())
  }

  stage = new PrimaryStage {
    title = "Wator Simulation"
    scene = new Scene(gridWidth * cellSize, gridHeight * cellSize) {
      val canvas = new Canvas(gridWidth * cellSize, gridHeight * cellSize)
      content = canvas
      runSimulation(initializeGrid(), canvas)
    }
  }
}
