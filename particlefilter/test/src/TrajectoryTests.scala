package com.scilari.particlefilter
import org.scalatest._
import flatspec._
import matchers._
import com.scilari.geometry.models.Float2
import com.scilari.geometry.utils.Float2Utils.linSpace
import com.scilari.particlefilter.TrajectoryUtils.positionsToControls
import com.scilari.particlefilter.TrajectoryUtils.controlsToPositions

class TrajectoryTests extends AnyFlatSpec with should.Matchers {

  "Points" should "map to controls and back" in {
    val c1 = Float2(0, -5)
    val c2 = Float2(50, 0)
    val c3 = Float2(50, 30)
    val c4 = Float2(0, 30)

    val e1 = linSpace(c1, c2, 40).dropRight(1)
    val e2 = linSpace(c2, c3, 40).dropRight(1)
    val e3 = linSpace(c3, c4, 40).dropRight(1)
    val e4 = linSpace(c4, c1, 40)
    val traj1 = e1 ++ e2 ++ e3 ++ e4
    val (initPose, controls) = positionsToControls(traj1.toSeq)
    val traj2 = controlsToPositions(controls, initPose)

    // Shape lenght should match to the sum of individual controls
    val totalShapeLength = Seq(c2 - c1, c3 - c2, c4 - c3, c1 - c4).map { _.length }.sum
    val totalControlLength = controls.map { _.position.length }.sum

    info(s"Shape distance vs. control sum: ${totalShapeLength} vs. ${totalControlLength}")
    assert(math.abs(totalShapeLength - totalControlLength) < 0.001f)

    val diff = traj1.zip(traj2).map { (p1, p2) => math.abs(p1.distance(p2)) }
    // info(diff.mkString(" "))
    info(s"Total diff: ${diff.sum}")
    assert(diff.sum < 0.001f)
  }

}
