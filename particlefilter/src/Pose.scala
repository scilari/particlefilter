package com.scilari.particlefilter

import com.scilari.math.ArrayUtils
import com.scilari.math.FloatMath.HalfPi
import com.scilari.geometry.models.{Float2, Position}

final case class Pose(position: Float2, heading: Angle) extends Position {
  def +(that: Pose): Pose = Pose(position + that.position, heading + that.heading)
  def -(that: Pose): Pose = Pose(position - that.position, heading - that.heading)

  def a: Float = heading.value

  def normalized: Pose = Pose(position, heading.normalized)

  def forward(d: Float): Pose = this.copy(position = this.position + Float2.directed(heading, d))

  def strafe(d: Float): Pose =
    this.copy(position = this.position + Float2.directed(heading + HalfPi, d))

  def rotated(a: Float): Pose = this.copy(heading = (this.heading + a).normalized)

  def moved(control: Pose): Pose = {
    this
      .forward(control.position.x)
      .strafe(control.position.y)
      .rotated(control.heading)
  }

  def toArray: Array[Float] = Array(position.x, position.y, heading)

  override def toString: String = "Pose: " + toArray.mkString("[", " ", "]")

}

object Pose {
  def apply(p: Float2, a: Float = 0f): Pose = new Pose(p, Angle(a).normalized)
  def apply(x: Float, y: Float, angle: Float): Pose =
    Pose(Float2(x, y), Angle(angle).normalized)

  def apply(that: Pose): Pose = Pose(that.position, that.heading)

  def zero: Pose = Pose(Float2(0, 0), 0)

  def unnormalized(x: Float, y: Float, angle: Float) = new Pose(Float2(x, y), Angle(angle))

  def weightedMean(ps: Iterable[Pose], ws: Iterable[Float]): Pose = {
    val x = ArrayUtils.weightedMean(ps.map { _.position.x }.toArray, ws.toArray)
    val y = ArrayUtils.weightedMean(ps.map { _.position.y }.toArray, ws.toArray)
    val a = Angle.weightedMean(ps.map { _.heading }, ws)
    Pose(x, y, a)
  }

}
