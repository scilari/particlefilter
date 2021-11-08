package com.scilari.particlefilter

import scala.collection.mutable.ArrayBuffer

object Resampling {

  // Standard systematic resampling
  def resampleIndices(
      weights: Array[Double],
      weightSum: Double = 1.0,
      copyCount: Int = -1
  ): Array[Int] = {
    val n = if (copyCount < 0) weights.size else copyCount
    val step = weightSum / n
    var wSum = scala.util.Random.between(0, step)
    val cs = weights.scanLeft(0.0)(_ + _) // cumulative sum with dummy zero element
    var pIx = 0
    val newIndices = ArrayBuffer[Int]()
    while (wSum < weightSum) {
      if (cs(pIx + 1) >= wSum) { // skipping the zero element
        newIndices += pIx
        wSum += step
      } else pIx += 1
    }
    newIndices.toArray
  }

}
