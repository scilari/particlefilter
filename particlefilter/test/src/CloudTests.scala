package com.scilari.particlefilter
import org.scalatest._
import flatspec._
import matchers._

import com.scilari.math.StatsUtils._
import com.scilari.geometry.models.{Float2, AABB}
import com.scilari.particlefilter.mhpf.MHPF
import scala.collection.mutable.ArraySeq

class CloudTests extends AnyFlatSpec with should.Matchers {

  "ParticleFilter" should "move diagonally" in {
    val motionModel = MotionModel(0.1, 0.1)
    val cloud = Cloud(10, motionModel)

    for (t <- 0 until 5) {
      cloud.move(Pose(1f, 0.2f, 0.0f * com.scilari.math.FloatMath.TwoPi))
    }
    val xs = cloud.particles.map { _.pose.position.x.toDouble }
    val ys = cloud.particles.map { _.pose.position.y.toDouble }

    assert(std(xs) > 0f)
    assert(std(ys) > 0f)
    assert(std(xs) < 0.5f)
    assert(std(ys) < 0.2f)
    assert(math.abs(mean(xs) - 5) < 0.2)
    assert(math.abs(mean(ys) - 1) < 0.1)
  }

  it should "move a circle" in {
    val motionModel = MotionModel(0.01, 0.01)
    val cloud = Cloud(10, motionModel)

    for (t <- 0 until 10) {
      cloud.move(Pose(1f, 0f, 0.1f * com.scilari.math.FloatMath.TwoPi))
    }
    val xs = cloud.particles.map { _.pose.position.x.toDouble }
    val ys = cloud.particles.map { _.pose.position.y.toDouble }

    assert(math.abs(mean(xs)) < 0.2f)
    assert(math.abs(mean(ys)) < 0.2f)

  }

  it should "be able to produce sine shape using measurements" in {
    val particleCount = 1000
    val sinDataX = (0 until 100)
    val sinData = sinDataX
      .map { _.toDouble }
      .map { i => Float2(i, 10 * math.sin(i / 10.0)) }

    DataUtils.pointsToFile(sinData, "ground_truth.csv")

    val controlPoints = sinDataX.map { i => Float2(i.toFloat, 0) }

    var currentPosition: Float2 = Float2.zero
    def likelihood(p: Particle[_]): Double = {
      val d2 = p.pose.position.distanceSq(currentPosition)
      val dev = 1.0
      -0.5 * d2 / (dev * dev)
    }

    val mhpf = MHPF[Particle[Any]](
      particleCount,
      Seq(
        (likelihood(_), (p: Particle[_]) => true)
      )
    )

    val motionModel = MotionModel(0.2f, 0.2f, 0.2f, 0.5f)
    val cloud = Cloud[Any](particleCount, motionModel, mhpf)

    val (initialPose, controls) = DataUtils.positionsToControls(controlPoints.toSeq)
    cloud.moveTo(initialPose)

    val estimates = for ((data, control) <- (sinData zip controls)) yield {
      currentPosition = data
      cloud.updateWeights()
      cloud.move(control)
      cloud.resampleIfNeeded()

      Thread.sleep(100)
      DataUtils.pointsToFile(cloud.particles.map { _.pose.position }, "particles.csv")

      val meanPose = CloudStats.meanPose(cloud)
      println(s"Correct: ${currentPosition}")
      println(s"Cloud: $meanPose")
      meanPose.position
    }

    DataUtils.pointsToFile(estimates, "points.csv")

  }

  it should "stay inside box" in {
    val particleCount = 1000
    val bb = AABB.fromMinMax(0, -4, 100, 4)

    DataUtils.pointsToFile(bb.corners, "ground_truth.csv")

    val controlPoints = (0 until 100).map { i => Float2(i.toFloat, 0) }

    var currentPosition: Float2 = Float2.zero
    def likelihood(p: Particle[_]): Double = {
      if (bb.contains(p.pose.position)) 0 else -20
    }

    val mhpf = MHPF[Particle[Any]](
      particleCount,
      Seq(
        (likelihood(_), (p: Particle[_]) => true)
      )
    )

    val motionModel = MotionModel(0.2f, 0.2f, 0.2f, 0.5f)
    val cloud = Cloud[Any](particleCount, motionModel, mhpf)

    val (initialPose, controls) = DataUtils.positionsToControls(controlPoints.toSeq)
    cloud.moveTo(initialPose)

    val estimates = for (control <- controls) yield {
      cloud.updateWeights()
      cloud.move(control)
      cloud.resampleIfNeeded()

      Thread.sleep(100)
      DataUtils.pointsToFile(cloud.particles.map { _.pose.position }, "particles.csv")

      val meanPose = CloudStats.meanPose(cloud)
      println(s"Correct: ${currentPosition}")
      println(s"Cloud: $meanPose")
      meanPose.position

    }

    DataUtils.pointsToFile(estimates, "points.csv")

  }

}
