package com.scilari.particlefilter

import com.scilari.particlefilter.mhpf.MHPF
import com.scilari.ancestry.AncestryTree
import scala.collection.mutable.ArrayBuffer
import com.scilari.ancestry.core.Tree
import com.scilari.ancestry.core.Leaf

class Cloud[P <: Particle[P]](
    val n: Int,
    val motionModel: MotionModel,
    val mhpf: MHPF[Leaf[P]],
    val rootParticle: P,
    val resampleIndexer: (Cloud[P]) => Array[Int] = Resampling.default[P],
    val resampleRatio: Double = 0.5
) {

  var updateCounter = 0

  var particleAncestryTree: Tree[P] = AncestryTree.fromElements(
    rootParticle,
    List.fill(n)(rootParticle.breed)
  )

  def particles = particleAncestryTree.leavesCached.map { _.data }

  def leaves = particleAncestryTree.leavesCached

  def move(control: Pose): Unit = {
    particles.foreach { p =>
      val scaledControl = Pose(control.position * p.scale, control.a)
      p.pose = p.pose.moved(motionModel.sampleControl(scaledControl))
    }
  }

  def moveTo(pose: Pose): Unit = particles.foreach { _.pose = pose }

  def weights: Array[Double] = mhpf.weights.toArray

  def updateWeights(): Unit = {
    mhpf.computeWeights(leaves)
    updateCounter += 1
  }

  def nEff: Double = 1.0 / weights.map { w => w * w }.sum
  def nEffRatio: Double = nEff / n

  def resampleIfNeeded(): Boolean = {
    if (nEffRatio < resampleRatio) {
      resample(weights)
      mhpf.reset()
      true
    } else false
  }

  def resample(weights: Array[Double]) = {
    val indices = resampleIndexer(this)
    val particles = this.particles
    indices.foreach { i => particles(i).children += particles(i).breed }

    particleAncestryTree = particleAncestryTree
      .nextGeneration(
        Particle.breed(_),
        Particle.merge(_, _)
      )
      .get

  }

}
