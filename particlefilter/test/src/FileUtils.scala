package com.scilari.particlefilter

import com.scilari.geometry.models.Float2

object FileUtils {
  def pointsToFile(ps: Iterable[Float2], fileName: String): Unit = {
    val wd = os.pwd / "visualization" / "input"
    val header = "x,y\n"
    val content = ps.map { p => s"${p.x},${p.y}\n" }.mkString("")
    os.write.over(wd / fileName, header + content, createFolders = true)
  }
}
