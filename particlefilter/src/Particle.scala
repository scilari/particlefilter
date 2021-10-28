package com.scilari.particlefilter

import scala.collection.mutable.ArrayBuffer

class Particle[E](
    val pose: Pose,
    val data: E = null
) {
  val children = ArrayBuffer[Particle[E]]()

  def copy(copyData: E => E = (e: E) => e) =
    Particle(pose.copy, copyData(data))

  override def toString: String = s"Particle at ${pose.toString}"
}

object Particle {
  def breed[E](p: Particle[E]): Seq[Particle[E]] = {
    val children = p.children.toSeq
    p.children.clear()
    children
  }

}
