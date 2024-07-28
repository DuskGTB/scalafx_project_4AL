import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scala.util.Random
import scalafx.animation.AnimationTimer

object ParticleSimulator extends JFXApp {

  val ParticleRadius = 5
  val Width = 800
  val Height = 600
  val NumParticles = 100

  case class Particle(x: Double, y: Double, direction: (Int, Int), color: Color)

  def randomDirection(): (Int, Int) = {
    val directions =
      List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
    directions(Random.nextInt(directions.length))
  }

  def createRandomParticle(): Particle = {
    val x = Random.nextDouble() * Width
    val y = Random.nextDouble() * Height
    val direction = randomDirection()
    val color =
      Color.color(Random.nextDouble(), Random.nextDouble(), Random.nextDouble())
    Particle(x, y, direction, color)
  }

  val particles = Array.fill(NumParticles)(createRandomParticle())

  val particleCircles = particles.map { particle =>
    new Circle {
      radius = ParticleRadius
      fill = particle.color
      centerX = particle.x
      centerY = particle.y
    }
  }

  stage = new PrimaryStage {
    title = "Particle Simulator"
    scene = new Scene(Width, Height) {
      content = particleCircles
    }
  }

  def updateParticle(
      particle: Particle,
      particles: Array[Particle]
  ): Particle = {
    val newX = (particle.x + particle.direction._1 + Width) % Width
    val newY = (particle.y + particle.direction._2 + Height) % Height

    val newDirection = particles.find { otherParticle =>
      otherParticle != particle &&
      math.sqrt(
        math.pow(newX - otherParticle.x, 2) + math.pow(
          newY - otherParticle.y,
          2
        )
      ) < 2 * ParticleRadius
    } match {
      case Some(_) => randomDirection()
      case None    => particle.direction
    }

    particle.copy(x = newX, y = newY, direction = newDirection)
  }

  val timer = AnimationTimer { _ =>
    val updatedParticles = particles.map(updateParticle(_, particles))
    updatedParticles.zip(particleCircles).foreach { case (particle, circle) =>
      circle.centerX = particle.x
      circle.centerY = particle.y
    }
    System.arraycopy(updatedParticles, 0, particles, 0, particles.length)
  }

  timer.start()
}
