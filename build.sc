import mill._, scalalib._
import mill.scalalib.publish._

import $file.spatialsearch_dotty.build
import $file.mhpf.build
import $file.ancestry.build

object particlefilter extends ScalaModule with ScalaJSModule /* with PublishModule */ {
  def scalaVersion = "3.3.3"

  def moduleDeps = Seq(
    spatialsearch_dotty.build.spatialsearch,
    mhpf.build.mhpf,
    ancestry.build.ancestry
  )

  override def ivyDeps = Agg(
    ivy"com.lihaoyi::os-lib:0.7.8"
  )

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

  object test extends ScalaTests with TestModule.Utest with TestModule.ScalaTest {
    override def ivyDeps =
      Agg(
        ivy"com.lihaoyi::utest::0.7.10",
        ivy"org.scalatest::scalatest:3.2.10",
        ivy"org.scalacheck::scalacheck:1.15.4"
      )

  }
}
