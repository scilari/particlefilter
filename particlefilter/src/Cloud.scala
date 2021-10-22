package com.scilari.particlefilter

import com.scilari.particlefilter.mhpf.MHPF
import scala.collection.mutable.ArrayBuffer

class Cloud[E](
    val n: Int,
    val motionModel: MotionModel,
    val mhpf: MHPF[Particle[E]] = null,
    initialPose: Pose = Pose(0, 0, 0),
    initialData: Option[E] = None,
    resampleRatio: Double = 0.5
) {
  var particles = Seq.tabulate(n) { id =>
    Particle[E](initialPose.copy, id, 0, initialData)
  }

  def move(control: Pose): Unit = {
    particles.foreach { p => p.pose.move(motionModel.sampleControl(control)) }
  }

  def moveTo(pose: Pose): Unit = particles.foreach { _.pose.moveTo(pose) }

  def weights: Seq[Double] = mhpf.weights

  def updateWeights(): Unit = mhpf.computeWeights(particles)

  def nEff: Double = 1.0 / weights.map { w => w * w }.sum
  def nEffRatio: Double = nEff / n

  def resampleIfNeeded() = {
    if (nEffRatio < resampleRatio) {
      resample(weights)
      mhpf.reset()
    }
  }

  def resample(weights: Seq[Double]) = {
    val step = 1.0 / n
    var wSum = scala.util.Random.between(0, step)
    val cs = com.scilari.math.ArrayUtils.cumsum(weights.toArray)
    var pIx = 0
    val newParticles = ArrayBuffer[Particle[E]]()
    while (wSum < 1.0) {
      if (cs(pIx) >= wSum) {
        newParticles += particles(pIx).copy
        wSum += step
      } else pIx += 1
    }
    particles = newParticles.toSeq
  }

  // TODO: move these to util object
  def meanPose: Pose =
    Pose.weightedMean(particles.map { _.pose }, weights.map { _.toFloat })

  def deviation: Double = {
    val m = meanPose.position
    val ws = weights
    val d2s = for ((p, w) <- particles.map { _.pose.position }.zip(ws)) yield {
      w * p.distanceSq(m)
    }
    math.sqrt(d2s.sum / n).toFloat
  }

  def roughen(pose: Pose): Unit = ???
}
