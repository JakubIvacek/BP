package downloader

import java.net.URI
import java.nio.file.{Files, Paths, StandardCopyOption}

object HttpDownload {
  /**
   * Downloads a file from the given URL to the specified path.
   */
  def downloadFile(url: String, targetPath: String): Unit = {
    println(s"Downloading $url -> $targetPath")
    val uri = URI.create(url)
    val in = uri.toURL.openStream()
    try {
      val target = Paths.get(targetPath)
      // Make sure parent dirs exist
      val parent = target.getParent
      if (parent != null) Files.createDirectories(parent)
      Files.copy(in, Paths.get(targetPath), StandardCopyOption.REPLACE_EXISTING)
    } finally {
      in.close()
    }
  }
}
