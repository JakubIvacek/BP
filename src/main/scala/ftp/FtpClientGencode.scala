package ftp

import org.apache.commons.net.ftp.FTPClient

import java.io.IOException

object FtpClientGencode {
  /**
   * Find the latest Gencode release available on the FTP server.
   *
   * @return The name of the latest release (e.g., "release_40").
   */
  def findLatestVersionGencode(): String = {
    val ftpClient = new FTPClient()
    val server = "ftp.ebi.ac.uk"
    val directory = "/pub/databases/gencode/Gencode_human/"

    try {
      ftpClient.connect(server)
      ftpClient.login("anonymous", "")
      ftpClient.changeWorkingDirectory(directory)
      ftpClient.enterLocalPassiveMode()
      // List files/directories
      val files = ftpClient.listNames()
      if (files == null || files.isEmpty) {
        throw new IOException(s"No releases found in directory: $directory")
      }

      val releases = files.filter(file => file.startsWith("release_"))
      
      val releaseNumbers = releases.flatMap { release =>
        val numberPart = release.stripPrefix("release_").stripSuffix("/")
        if (numberPart.forall(_.isDigit)) Some(numberPart.toInt) else None
      }

      releaseNumbers.sorted.lastOption
        .map(num => s"release_$num")
        .getOrElse {
          println("No valid releases found")
          ""
        }
    } catch {
      case e: IOException =>
        println(s"Error: ${e.getMessage}")
        ""
    } finally {
      if (ftpClient.isConnected) {
        ftpClient.logout()
        ftpClient.disconnect()
      }
    }
  }
  
}
