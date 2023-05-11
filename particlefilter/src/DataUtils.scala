package com.scilari.particlefilter

import com.scilari.geometry.models.Float2
import scala.collection.mutable.ArrayBuffer
import com.scilari.math.FloatMath

object DataUtils {
  def angleBetween(p1: Float2, p2: Float2): Float = {
    val diff = p2 - p1
    FloatMath.atan2(diff.y, diff.x)
  }

  // Note: the first control contains the translation and rotation from origo to first position
  def positionsToControls(ps: IndexedSeq[Float2]): (Pose, Seq[Pose]) = {
    var angleCurr = angleBetween(ps(0), ps(1))
    val initialPose = Pose(ps(0), angleCurr)
    val middleControls = ps.sliding(3, 1).map { case Seq(p0, p1, p2) =>
      val angleNext = angleBetween(p1, p2)
      val angleDiff = Angle.angleDiff(angleNext, angleCurr)
      angleCurr = angleNext
      Pose(p0.distance(p1), 0, angleDiff)
    }
    val lastPose = Pose(ps.last.distance(ps(ps.size - 2)), 0f, 0f)
    val controls = Seq(Pose.zero) ++ middleControls ++ Seq(lastPose)

    (initialPose, controls)
  }

  def pointsToFile(ps: Iterable[Float2], fileName: String): Unit = {
    val wd = os.pwd / "visualization" / "input"
    val header = "x,y\n"
    val content = ps.map { p => s"${p.x},${p.y}\n" }.mkString("")
    os.write.over(wd / fileName, header + content, createFolders = true)
  }
}
