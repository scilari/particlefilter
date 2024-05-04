package com.scilari.particlefilter

import org.scalatest._
import flatspec._
import matchers._

import com.scilari.geometry.models.Float2
import com.scilari.math.StatsUtils._
import com.scilari.math.FloatMath.{Pi, TwoPi}

class MotionModelTests extends AnyFlatSpec with should.Matchers {
  val motionModel = MotionModel(
    translationDev = 0.1f,
    rotationDev = 0.1f * TwoPi,
    translationCrossDev = 0.01f,
    translationToRotationDev = 0.1f
  )

  val sampleCount = 10000
  val tolerance = 0.05
  val tolTrans = tolerance * motionModel.translationDev
  val tolRot = tolerance * motionModel.rotationDev * TwoPi

  val initialPose = Pose(Float2.zero, 0f)

  "MotionModel" should "work work on single forward step" in {
    val control = Pose(Float2.unitX, 0f)

    val poses = (0 until sampleCount).map { _ =>
      initialPose + motionModel.sampleControl(control)
    }

    val xs: Seq[Double] = poses.map { _.position.x }

    info(s"Poses x mean = ${mean(xs)} and std = ${std(xs)}")

    assert(math.abs(std(xs) - motionModel.translationDev) < tolTrans)
  }

  it should "work on with smaller forward steps" in {
    val control = Pose(Float2.unitX * 0.1f, 0f)

    val poses = (0 until sampleCount).map { _ =>
      var currentPose = initialPose
      (0 until 10).foreach { _ =>
        currentPose = currentPose.moved(motionModel.sampleControl(control))
      }
      currentPose
    }

    val xs = poses.map { _.position.x.toDouble }

    info(s"Poses x mean = ${mean(xs)} and std = ${std(xs)}")

    assert(math.abs(std(xs) - motionModel.translationDev) < tolTrans)
  }

  it should "work with a single rotating step" in {
    val control = Pose.unnormalized(0, 0, TwoPi)

    val poses = (0 until sampleCount).map { _ =>
      initialPose + motionModel.sampleControl(control)
    }

    val as: Seq[Double] = poses.map { _.heading.value.toDouble }

    info(s"Poses heading mean = ${mean(as)} and std = ${std(as)}")

    assert(math.abs(std(as) - motionModel.rotationDev) < tolRot)
  }

  it should "work with smaller rotating steps" in {
    val control = Pose(Float2.zero, 0.1f * TwoPi)

    val poses = (0 until sampleCount).map { _ =>
      var currentPose = initialPose
      (0 until 10).foreach { _ =>
        currentPose = currentPose.moved(motionModel.sampleControl(control))
      }
      currentPose
    }

    val as: Seq[Double] = poses.map { _.heading.value.toDouble }

    info(s"Poses heading mean = ${mean(as)} and std = ${std(as)}")

    assert(math.abs(std(as) - motionModel.rotationDev) < tolRot)
  }
}
