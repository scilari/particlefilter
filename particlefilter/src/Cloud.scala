package com.scilari.particlefilter

import com.scilari.particlefilter.mhpf.MHPF
import com.scilari.ancestry.AncestryTree
import scala.collection.mutable.ArrayBuffer
import com.scilari.ancestry.core.Tree

class Cloud[E](
    val n: Int,
    val motionModel: MotionModel,
    val mhpf: MHPF[Particle[E]] = null,
    val rootParticle: Particle[E] = Particle(),
    val breedParticle: Particle[E] => Particle[E] = (p: Particle[E]) => p.copy(),
    val mergeParticles: (Particle[E], Particle[E]) => Particle[E] =
      (a: Particle[E], b: Particle[E]) => a,
    resampleRatio: Double = 0.5
) {

  var particleAncestryTree: Tree[Particle[E]] = AncestryTree.fromElements(
    rootParticle,
    List.fill(n)(breedParticle(rootParticle))
  )

  def particles = particleAncestryTree.leavesCached.view.map { _.data }

  def move(control: Pose): Unit = {
    particles.foreach { p =>
      val scaledControl = Pose(control.position * p.scale, control.a)
      p.pose.move(motionModel.sampleControl(scaledControl))
    }
  }

  def moveTo(pose: Pose): Unit = particles.foreach { _.pose.moveTo(pose) }

  def weights: Array[Double] = mhpf.weights.toArray

  def updateWeights(): Unit = mhpf.computeWeights(particles.toIndexedSeq)

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
    val indices = Resampling.resampleIndices(weights)
    val particles = this.particles
    indices.foreach { i => particles(i).children += breedParticle(particles(i)) }

    particleAncestryTree = particleAncestryTree
      .nextGeneration(
        Particle.breed[E](_),
        mergeParticles(_, _)
      )
      .get

  }

}
