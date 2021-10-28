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
  def positionsToControls(ps: Seq[Float2]): (Pose, Seq[Pose]) = {
    var angleCurr = angleBetween(ps(0), ps(1))
    val initialPose = Pose(ps(0), angleCurr)
    val controls = ArrayBuffer[Pose](Pose(0, 0, 0))
    for (i <- 0 until (ps.size - 1)) {
      val pCurr = ps(i)
      val pNext = ps(i + 1)
      val angleNext = angleBetween(pCurr, pNext)
      val angleDiff = Angle.angleDiff(angleNext, angleCurr)
      angleCurr = angleNext
      controls.append(
        Pose(pCurr.distance(pNext), 0, angleDiff)
      )
    }
    (initialPose, controls.toSeq)
  }

  def pointsToFile(ps: IndexedSeq[Float2], fileName: String): Unit = {
    val wd = os.pwd / "visualization" / "input"
    val header = "x,y\n"
    val content = ps.map { p => s"${p.x},${p.y}\n" }.mkString("")
    os.write.over(wd / fileName, header + content, createFolders = true)
  }
}
