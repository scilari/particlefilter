package com.scilari.particlefilter

import com.scilari.geometry.models.Float2
import scala.collection.mutable.ArrayBuffer
import com.scilari.math.FloatMath

object TrajectoryUtils {
  def angleBetween(p1: Float2, p2: Float2, fallBack: Float = 0f): Float = {
    val diff = p2 - p1
    if (diff.lengthSq > 0)
      FloatMath.atan2(diff.y, diff.x)
    else
      fallBack
  }

  /** Computes relative poses from a set of 2D points. These can be interpreted as piecewise
    * controls, where straight translation is followed by a rotation. Ignores duplicate points.
    * Computes also the initial absolute pose, not included in the controls.
    *
    * @param ps
    *   2D points
    * @return
    *   (absolute initial pose, controls)
    */
  def positionsToControls(ps: Seq[Float2]): (Pose, Seq[Pose]) = {
    val paddedPs = ps.head +: ps :+ ps.last

    val initialHeading =
      ps.tail.find(_ != ps.head).map { p => (p - ps.head).direction }.getOrElse(0f)

    var angleCurr = initialHeading

    val controls = paddedPs
      .sliding(3, 1)
      .map { case Seq(p0, p1, p2) =>
        val d = p0.distance(p1)
        if (d > 0f) {
          val angleNext = angleBetween(p1, p2, angleCurr)
          val control = Pose(d, 0, Angle.angleDiff(angleNext, angleCurr))
          angleCurr = angleNext
          control
        } else Pose.zero
      }
      .toSeq

    val initialPose = Pose(ps.head, initialHeading)
    (initialPose, controls)

  }

  def controlsToPositions(controls: Seq[Pose], initialPose: Pose = Pose.zero): Seq[Float2] = {
    controls.scan(initialPose)((p, c) => p.moved(c)).map { _.position }.drop(1)
  }
}
