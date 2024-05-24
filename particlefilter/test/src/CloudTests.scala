package com.scilari.particlefilter
import org.scalatest._
import flatspec._
import matchers._

import doodle.java2d.*
import cats.effect.unsafe.implicits.global
import doodle.syntax.all.*
import doodle.core.*
import doodle.java2d.effect.*

import com.scilari.math.StatsUtils._
import com.scilari.geometry.models.{Float2, AABB}
import com.scilari.particlefilter.mhpf.MHPF
import scala.collection.mutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
import org.scalacheck.Test
import com.scilari.geometry.utils.Float2Utils.linSpace
import scala.util.Random
import com.scilari.ancestry.core.Leaf

class CloudTests extends AnyFlatSpec with should.Matchers {
  val VISUALIZE = false

  class TestParticle(p: Pose = Pose.zero) extends Particle[TestParticle](p) {
    val history = ArrayBuffer[Pose]()

    override def breed = {
      TestParticle(pose)
    }
    override def merge(parent: TestParticle) = {
      parent.history ++= history
      parent
    }
  }

  "ParticleFilter" should "move diagonally" in {
    val motionModel = MotionModel(0.1, 0.1)
    val cloud = Cloud(10, motionModel, mhpf = null, rootParticle = TestParticle())

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

  it should "move in a circle" in {
    val motionModel = MotionModel(0.01, 0.01)
    val cloud = Cloud(10, motionModel, mhpf = null, rootParticle = TestParticle())

    for (t <- 0 until 10) {
      cloud.move(Pose(1f, 0f, 0.1f * com.scilari.math.FloatMath.TwoPi))
    }
    val xs = cloud.particles.map { _.pose.position.x.toDouble }
    val ys = cloud.particles.map { _.pose.position.y.toDouble }

    assert(math.abs(mean(xs)) < 0.2f)
    assert(math.abs(mean(ys)) < 0.2f)

  }

  it should "be able to produce sine shape using measurements" in {

    val canvas = Frame.default
      .withSize(1000, 400)
      .withCenterAtOrigin
      .withClearToBackground
      .canvas()
      .unsafeRunSync()

    val particleCount = 1000
    val sinDataX = (0 until 100)
    val sinData = sinDataX
      .map { _.toDouble }
      .map { i => Float2(i, 10 * math.sin(i / 10.0)) }

    // DataUtils.pointsToFile(sinData, "ground_truth.csv")

    var currentPosition: Float2 = Float2.zero
    def likelihood(leaf: Leaf[TestParticle]): Double = {
      val p = leaf.data
      val d2 = p.pose.position.distanceSq(currentPosition)
      val dev = 2.5
      -0.5 * d2 / (dev * dev)
    }

    val mhpf = MHPF[Leaf[TestParticle]](
      particleCount,
      IndexedSeq((likelihood(_), (p: Leaf[TestParticle]) => true))
    )

    val motionModel = MotionModel(0.2f, 0.2f, 0.2f, 0.5f)
    val cloud = Cloud[TestParticle](
      particleCount,
      motionModel,
      mhpf,
      rootParticle = TestParticle()
    )

    val (initialPose, controls) = TrajectoryUtils.positionsToControls(sinData)
    val noisyControls = controls.map { _.rotated((0.15 * Random.nextGaussian()).toFloat) }
    val noisyPoints = TrajectoryUtils.controlsToPositions(noisyControls, initialPose)

    cloud.moveTo(initialPose)

    val estimates = for ((data, control) <- (sinData zip noisyControls)) yield {
      currentPosition = data
      cloud.particles.foreach { p => p.history += p.pose }
      cloud.updateWeights()
      cloud.move(control)
      cloud.resampleIfNeeded()

      if (VISUALIZE) visualize(canvas, cloud, sinData, noisyPoints)

      // Thread.sleep(100)
      // DataUtils.pointsToFile(cloud.particles.map { _.pose.position }, "particles.csv")
      assert(
        CloudStats.meanPose(cloud).position.distance(data) < 10.0,
        "The cloud should track the path"
      )
      assert(cloud.particleAncestryTree.depth < 25, "The ancestry tree should stay shallow")
    }

    // DataUtils.pointsToFile(estimates, "points.csv")

  }

  def visualize(
      canvas: Canvas,
      cloud: Cloud[TestParticle],
      gt: Seq[Float2],
      pdr: Seq[Float2]
  ): Unit = {

    val offsetX = AABB.fromPoints(gt).width / 2
    val scale = 9.5
    def transform(p: Float2) = (scale * (p - offsetX * Float2.unitX))
    val mean = transform(CloudStats.meanPose(cloud).position)
    var picture = circle(40).at(mean.x, mean.y)

    val transparentBlack = Color.rgba(100, 100, 100, 1)
    val pathStyle = circle(4).fillColor(Color.orangeRed)
    val historyStyle = circle(4).fillColor(Color.green)
    val pdrStyle = circle(3).noStroke.fillColor(Color.black)
    val particleStyle = circle(3).noStroke.fillColor(transparentBlack)

    for (p <- cloud.particles.take(1000)) {
      val p2 = transform(p.pose.position)
      picture = picture.on(particleStyle.at(p2.x, p2.y).asInstanceOf)
    }

    for (p <- gt) {
      val p2 = transform(p)
      picture = picture.on(pathStyle.at(p2.x, p2.y).asInstanceOf)
    }

    val best = cloud.particleAncestryTree.leaves(0)
    val history = best.ancestors.map { _.data.history }.flatten

    for (p <- history) {
      val p2 = transform(p.position)
      picture = picture.on(historyStyle.at(p2.x, p2.y).asInstanceOf)
    }

    for (p <- pdr) {
      val p2 = transform(p.position)
      picture = picture.on(pdrStyle.at(p2.x, p2.y).asInstanceOf)
    }

    picture.drawWithCanvas(canvas)
    Thread.sleep(500)
  }

  ignore should "stay inside box" in {
    val particleCount = 1000
    val bb = AABB.fromMinMax(0, -4, 100, 4)

    FileUtils.pointsToFile(bb.corners.toIndexedSeq, "ground_truth.csv")

    val controlPoints = (0 until 100).map { i => Float2(i.toFloat, 0) }

    var currentPosition: Float2 = Float2.zero
    def likelihood(leaf: Leaf[TestParticle]): Double = {
      if (bb.contains(leaf.data.pose.position)) 0 else -20
    }

    val mhpf = MHPF[Leaf[TestParticle]](
      particleCount,
      IndexedSeq((likelihood(_), (p: Leaf[TestParticle]) => true))
    )

    val motionModel = MotionModel(0.2f, 0.2f, 0.2f, 0.5f)
    val cloud = Cloud[TestParticle](particleCount, motionModel, mhpf, rootParticle = TestParticle())

    val (initialPose, controls) = TrajectoryUtils.positionsToControls(controlPoints.toSeq)
    cloud.moveTo(initialPose)

    val estimates = for (control <- controls) yield {
      cloud.updateWeights()
      cloud.move(control)
      cloud.resampleIfNeeded()

      // Thread.sleep(100)
      FileUtils.pointsToFile(cloud.particles.map { _.pose.position }, "particles.csv")

      val meanPose = CloudStats.meanPose(cloud)
      println(s"Correct: ${currentPosition}")
      println(s"Cloud: $meanPose")
      meanPose.position

    }
  }

}
