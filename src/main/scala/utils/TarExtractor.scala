package utils

import java.io.{File, FileInputStream, FileOutputStream, BufferedOutputStream}
import org.apache.commons.compress.archivers.tar.{TarArchiveInputStream, TarArchiveEntry}

/**
 * Object for extracting `.tar`
 */
object TarExtractor {
  /**
   * Unzips .tar file to the destination directory.
   *
   * @param tarFilePath    The path to the .tar file to be unzipped.
   * @param destinationDir The directory where the contents will be extracted.
   */
  def unzipTar(tarFilePath: String, destinationDir: String): Unit = {
    val tarFile = new File(tarFilePath)
    val destDir = new File(destinationDir)

    if (!destDir.exists()) {
      destDir.mkdirs()
    }

    val tarInputStream = new TarArchiveInputStream(new FileInputStream(tarFile))

    try {
      var entry: TarArchiveEntry = tarInputStream.getNextTarEntry

      while (entry != null) {
        val destFile = new File(destDir, entry.getName)

        if (entry.isDirectory) {
          destFile.mkdirs()
        } else {
          val outputStream = new BufferedOutputStream(new FileOutputStream(destFile))
          val buffer = new Array[Byte](4096)   // 4 KB buffer
          var bytesRead = 0

          while ({bytesRead = tarInputStream.read(buffer); bytesRead != -1}) {
            outputStream.write(buffer, 0, bytesRead)
          }

          outputStream.close()
        }

        entry = tarInputStream.getNextTarEntry
      }
    } finally {
      println(s"Extracted .tar file - $tarFilePath to $destinationDir")
      tarInputStream.close()
    }
  }
}

