package com.scilari.particlefilter

import utest._
import com.scilari.geometry.models.Float2
import com.scilari.math.StatsUtils._
import com.scilari.math.FloatMath.{Pi, TwoPi}

object MotionModelTests extends TestSuite {
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

  val tests = Tests {

    test("MotionModel should work on single forward step") {
      val control = Pose(Float2.unitX, 0f)

      val poses = (0 until sampleCount).map { _ =>
        initialPose + motionModel.sampleControl(control)
      }

      val xs: Seq[Double] = poses.map { _.position.x }

      println("Poses x mean: " + mean(xs))
      println("Poses x std: " + std(xs))

      assert(math.abs(std(xs) - motionModel.translationDev) < tolTrans)
    }

    test("MotionModel should work on with smaller forward steps") {
      val control = Pose(Float2.unitX * 0.1f, 0f)

      val poses = (0 until sampleCount).map { _ =>
        var currentPose = initialPose.copy
        (0 until 10).foreach { _ =>
          currentPose.move(motionModel.sampleControl(control))
        }
        currentPose
      }

      val xs = poses.map { _.position.x.toDouble }

      println("Poses x mean: " + mean(xs))
      println("Poses x std: " + std(xs))

      assert(math.abs(std(xs) - motionModel.translationDev) < tolTrans)
    }

    test("MotionModel should work with single rotating step") {
      val control = Pose(Float2.zero, TwoPi)

      val poses = (0 until sampleCount).map { _ =>
        initialPose + motionModel.sampleControl(control)
      }

      val as: Seq[Double] = poses.map { _.heading.value.toDouble }

      println("Poses heading mean: " + mean(as))
      println("Poses heading std: " + std(as))

      assert(math.abs(std(as) - motionModel.rotationDev) < tolRot)
    }

    test("MotionModel should work on with smaller rotating steps") {
      val control = Pose(Float2.zero, 0.1f * TwoPi)

      val poses = (0 until sampleCount).map { _ =>
        var currentPose = initialPose.copy
        (0 until 10).foreach { _ =>
          currentPose.move(motionModel.sampleControl(control))
        }
        currentPose
      }

      val as: Seq[Double] = poses.map { _.heading.value.toDouble }

      println("Poses heading mean: " + mean(as))
      println("Poses heading std: " + std(as))

      assert(math.abs(std(as) - motionModel.rotationDev) < tolRot)
    }

  }

}
