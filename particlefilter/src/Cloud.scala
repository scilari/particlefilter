package com.scilari.particlefilter

import com.scilari.particlefilter.mhpf.MHPF
import com.scilari.ancestry.AncestryTree
import scala.collection.mutable.ArrayBuffer
import com.scilari.ancestry.core.Tree

class Cloud[E](
    val n: Int,
    val motionModel: MotionModel,
    val mhpf: MHPF[Particle[E]] = null,
    val rootParticle: Particle[E] = Particle(Pose.zero, null),
    val breedParticle: Particle[E] => Particle[E] = (a: Particle[E]) => a,
    val mergeParticles: (Particle[E], Particle[E]) => Particle[E] =
      (a: Particle[E], b: Particle[E]) => a,
    resampleRatio: Double = 0.5
) {

  var particleAncestryTree: Tree[Particle[E]] = AncestryTree.fromElements(
    rootParticle,
    Seq.fill(n)(rootParticle.copy())
  )

  def particles = particleAncestryTree.leaves.map { _.data }.toArray

  def move(control: Pose): Unit = {
    particles.foreach { p => p.pose.move(motionModel.sampleControl(control)) }
  }

  def moveTo(pose: Pose): Unit = particles.foreach { _.pose.moveTo(pose) }

  def weights: Array[Double] = mhpf.weights.toArray

  def updateWeights(): Unit = mhpf.computeWeights(particles.toIndexedSeq)

  def nEff: Double = 1.0 / weights.map { w => w * w }.sum
  def nEffRatio: Double = nEff / n

  def resampleIfNeeded() = {
    if (nEffRatio < resampleRatio) {
      resample(weights)
      mhpf.reset()
    }
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

  def roughen(pose: Pose): Unit = ???
}
