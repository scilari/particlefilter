package com.scilari.particlefilter

class Particle[E](
    val pose: Pose,
    val id: Int,
    val time: Long = 0,
    val data: Option[E] = None
) {
  def copy = Particle(this.pose.copy, this.id, this.time, this.data)
  override def toString: String = s"Particle $id at ${pose.toString}"
}
