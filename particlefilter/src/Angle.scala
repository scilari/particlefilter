package com.scilari.particlefilter

import com.scilari.math.ArrayUtils
import com.scilari.math.FloatMath._

class Angle(var value: Float) {
  def +(that: Angle): Angle = Angle(value + that.value)
  def -(that: Angle): Angle = Angle(value - that.value)
  def *(c: Float): Angle = Angle(c * value)
  def normalized: Float = Angle.normalizeAngle(value)
  def normalize(): Unit = { value = Angle.normalizeAngle(value) }
}

object Angle {
  import scala.language.implicitConversions

  def apply(value: Float): Angle = new Angle(value)
  implicit def toFloat(a: Angle): Float = a.value
  implicit def fromFloat(f: Float): Angle = new Angle(f)

  def angleDiff(a1: Float, a2: Float): Float = {
    normalizeAngle(a1 - a2)
  }

  def towardsAngle(original: Float, target: Float, ratio: Float): Float = {
    val diff = angleDiff(target, original)
    original + ratio * diff
  }

  def angleDist(a1: Float, a2: Float): Float = abs(angleDiff(a1, a2))

  def normalizeAngle(a: Float): Float = {
    var aa = a
    while (aa > Pi) aa -= TwoPi
    while (aa < -Pi) aa += TwoPi
    aa
  }

  def weightedMean(angles: Seq[Angle], ws: Seq[Float]): Float = {
    ArrayUtils.weightedMeanAngle(angles.map { toFloat }.toArray, ws.toArray)
  }

}
