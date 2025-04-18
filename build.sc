import mill._, scalalib._
import mill.scalalib.publish._

import $file.spatialsearch.build
import $file.mhpf.build
import $file.ancestry.build

trait BaseModule extends ScalaModule {
  def scalaVersion = "3.3.5"

  def moduleDeps = Seq(
    spatialsearch.build.spatialsearch,
    mhpf.build.mhpf,
    ancestry.build.ancestry
  )

  override def ivyDeps = Agg(ivy"com.lihaoyi::os-lib:0.10.0")

  def publishVersion = "0.0.1"
  def pomSettings = PomSettings(
    description = "Scala Particle Filter library",
    organization = "com.scilari",
    url = "https://github.com/scilari/particlefilter",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("scilari", "particlefilter"),
    developers = Seq(
      Developer("scilari", "Ilari Vallivaara", "https://github.com/scilari")
    )
  )
}

object particlefilter extends BaseModule {
  object test extends ScalaModule with ScalaTests with TestModule.Utest with TestModule.ScalaTest {
    override def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.10",
      ivy"org.scalatest::scalatest:3.2.18",
      ivy"org.scalacheck::scalacheck:1.15.4",
      ivy"org.creativescala::doodle:0.22.0"
    )
  }
}
