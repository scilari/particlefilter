package com.scilari.particlefilter

import com.scilari.geometry.models.Float2
import scala.collection.mutable.ArrayBuffer
import com.scilari.math.FloatMath

object DataUtils {
  def angleBetween(p1: Float2, p2: Float2, fallBack: Float = 0f): Float = {
    val diff = p2 - p1
    if (diff.lengthSq > 0)
      FloatMath.atan2(diff.y, diff.x)
    else
      fallBack
  }

  /**
    * Computes relative poses from a set of 2D points. These can be interpreted
    * as piecewise controls, where straight translation is followed by a rotation.
    * Handles duplicate points. Computes also the initial absolute pose, not included
    * in the controls.
    *
    * @param ps 2D points
    * @return (absolute initial pose, controls)
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
          angleCurr = angleBetween(p0, p1)
          val angleNext = angleBetween(p1, p2)
          Pose(d, 0, Angle.angleDiff(angleNext, angleCurr))
        } else Pose.zero
      }
      .toSeq

    val initialPose = Pose(ps.head, initialHeading)
    (initialPose, controls)

  }

  def controlsToPositions(cs: Seq[Pose], initialPose: Pose = Pose.zero): Seq[Float2] = {
    val pose = initialPose
    cs.map { c =>
      pose.move(c)
      pose.position.copy()
    }
  }

  def pointsToFile(ps: Iterable[Float2], fileName: String): Unit = {
    val wd = os.pwd / "visualization" / "input"
    val header = "x,y\n"
    val content = ps.map { p => s"${p.x},${p.y}\n" }.mkString("")
    os.write.over(wd / fileName, header + content, createFolders = true)
  }
}
