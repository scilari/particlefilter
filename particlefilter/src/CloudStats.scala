package com.scilari.particlefilter

object CloudStats {

  def meanPose(cloud: Cloud[_]): Pose =
    Pose.weightedMean(cloud.particles.map { _.pose }, cloud.weights.map { _.toFloat })

  def deviation(cloud: Cloud[_]): Double = {
    val m = meanPose(cloud).position
    val ws = cloud.weights
    val d2s = for ((p, w) <- cloud.particles.map { _.pose.position }.zip(ws)) yield {
      w * p.distanceSq(m)
    }
    math.sqrt(d2s.sum).toFloat
  }

}
