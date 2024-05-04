package com.scilari.particlefilter

import scala.collection.mutable.ArrayBuffer

abstract class Particle[T](
    var pose: Pose,
    var scale: Float = 1.0f
) {
  Self: T =>
  val children = ArrayBuffer[T]()

  // Tell how to make copy for the next generation
  def breed: T

  // How to merge particle's data to its parent
  def merge(parent: T): T

  override def toString: String = s"Particle at ${pose.toString}"
}

object Particle {
  def breed[P <: Particle[P]](p: P): List[P] = {
    val children = p.children.toList
    p.children.clear()
    children
  }

  def merge[P <: Particle[P]](child: P, parent: P): P = child.merge(parent)

}
