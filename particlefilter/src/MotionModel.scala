package com.scilari.particlefilter

import com.scilari.math.FloatMath._

/** Model for sampling motion
  *
  * Deviations are proportional to 1 meter translation or 2 pi rotation.
  * @param translationDev
  * @param rotationDev
  * @param translationCrossDev
  * @param translationToRotationDev
  * @param rotationToTranslationDev
  */
case class MotionModel(
    translationDev: Float = 0f,
    rotationDev: Float = 0f,
    translationCrossDev: Float = 0f,
    translationToRotationDev: Float = 0f,
    rotationToTranslationDev: Float = 0f
) {
  // Variances precomputed
  private val traVar = translationDev * translationDev
  private val rotVar = rotationDev * rotationDev
  private val trcVar = translationCrossDev * translationCrossDev
  private val t2rVar = translationToRotationDev * translationToRotationDev
  private val r2tVar = rotationToTranslationDev * rotationToTranslationDev

  def sampleControl(control: Pose): Pose = {
    val x = control.position.x
    val y = control.position.y
    val a = control.a
    val trans = control.position.length

    Pose(
      x + e(x, traVar) + e(y, trcVar) + e(a, r2tVar, true),
      y + e(y, traVar) + e(x, trcVar) + e(a, r2tVar, true),
      a + e(a, rotVar, true) + e(trans, t2rVar)
    )
  }

  private val invTwoPi = 1f / TwoPi
  // Gaussian error proportional to error on translation/rotation d with variance v (wrt. 1.0 m or 2*Pi)
  private def e(
      d: Float,
      v: Float,
      normalizeForAngle: Boolean = false
  ): Float = {
    if (v > 0f) {
      val dd = if (normalizeForAngle) d * invTwoPi else d
      val dev = sqrt(abs(dd) * v)
      dev * scala.util.Random.nextGaussian().toFloat
    } else 0f
  }

}

object MotionModel {
  def robot = MotionModel(
    translationDev = 0.015f,
    rotationDev = 0.1f,
    translationCrossDev = 0.001f,
    translationToRotationDev = 0.01f * Pi,
    rotationToTranslationDev = 0f
  )

  def human = MotionModel(
    translationDev = 0.2f,
    rotationDev = 0.1f,
    translationCrossDev = 0.001f,
    translationToRotationDev = 0.005f * Pi,
    rotationToTranslationDev = 0.2f / TwoPi
  )

  def humanSLAM = MotionModel(
    translationDev = 0.02f,
    rotationDev = 0.01f,
    translationCrossDev = 0.001f,
    translationToRotationDev = 0.005f * Pi,
    rotationToTranslationDev = 0.5f / TwoPi
  )

}
