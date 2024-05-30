package com.scilari.particlefilter

import scala.collection.mutable.ArrayBuffer
import com.scilari.geometry.models.Position
import com.scilari.geometry.models.Float2

abstract class Particle[T](
    var pose: Pose,
    var scale: Float = 1.0f
) extends Position {
  Self: T =>
  val children = ArrayBuffer[T]()

  /** How to make copy for the next generation
    *
    * @return
    *   Copy for the next generation
    */
  def breed: T

  /** How to merge particle's data to its parent
    *
    * @param parent
    * @return
    *   Parent merged with the child
    */
  def merge(parent: T): T

  def position: Float2 = pose.position

  override def toString: String = s"Particle at ${pose.toString}"
}

object Particle {
  def breed[P <: Particle[P]](p: P): List[P] = {
    val children = p.children.toList
    p.children.clear()
    children
  }

  def merge[P <: Particle[P]](parent: P, child: P): P = child.merge(parent)

}
