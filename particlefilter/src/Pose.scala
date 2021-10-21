package com.scilari.particlefilter

import com.scilari.math.ArrayUtils
import com.scilari.math.FloatMath.HalfPi
import com.scilari.geometry.models.{Float2, Position}

class Pose(var position: Float2, var heading: Angle) extends Position {
  def +(that: Pose): Pose = Pose(position + that.position, heading + that.heading)
  def -(that: Pose): Pose = Pose(position - that.position, heading - that.heading)
  def +=(that: Pose): Unit = {
    position += that.position
    heading += that.heading
  }
  def -=(that: Pose): Unit = {
    position -= that.position
    heading -= that.heading
  }

  def normalize(): Unit = heading.normalize()

  def a: Float = heading.value

  def forward(d: Float): Unit = { this.position += Float2.directed(heading, d) }

  def strafe(d: Float): Unit = { this.position += Float2.directed(heading + HalfPi, d) }

  def rotate(a: Float): Unit = { heading += a }

  def move(control: Pose): Unit = {
    forward(control.position.x)
    strafe(control.position.y)
    rotate(control.heading)
  }

  def moved(control: Pose): Pose = {
    val cp = Pose(this)
    cp.move(control)
    cp
  }

  def moveTo(pose: Pose): Unit = {
    position = pose.position
    heading = pose.heading
  }

  def copy: Pose = Pose(this)

  def toArray: Array[Float] = Array(position.x, position.y, heading)

  override def toString: String = "Pose: " + toArray.mkString("[", " ", "]")

}

object Pose {
  def apply(x: Float, y: Float, angle: Float): Pose = new Pose(Float2(x, y), Angle(angle))
  def apply(that: Pose): Pose = Pose(that.position.x, that.position.y, that.heading)
  def apply(p: Float2, a: Float = 0f): Pose = new Pose(p, Angle(a))

  def weightedMean(ps: Seq[Pose], ws: Seq[Float]): Pose = {
    val x = ArrayUtils.weightedMean(ps.map { _.position.x }.toArray, ws.toArray)
    val y = ArrayUtils.weightedMean(ps.map { _.position.y }.toArray, ws.toArray)
    val a = Angle.weightedMean(ps.map { _.heading }, ws)
    Pose(x, y, a)
  }

}
