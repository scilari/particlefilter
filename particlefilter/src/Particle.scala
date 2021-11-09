package com.scilari.particlefilter

import scala.collection.mutable.ArrayBuffer

class Particle[E](
    val pose: Pose = Pose.zero,
    var scale: Float = 1.0f,
    val data: E = null
) {
  val children = ArrayBuffer[Particle[E]]()

  def copy(copyData: E => E = (e: E) => e) =
    Particle(pose.copy, scale, copyData(data))

  override def toString: String = s"Particle at ${pose.toString}"
}

object Particle {
  def breed[E](p: Particle[E]): List[Particle[E]] = {
    val children = p.children.toList
    p.children.clear()
    children
  }

}
