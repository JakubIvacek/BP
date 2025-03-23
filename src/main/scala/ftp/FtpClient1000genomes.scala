package ftp

import org.apache.commons.net.ftp.FTPClient
import java.io.IOException

object FtpClient1000genomes {
  /**
   * Find the latest release of the 1000 Genomes project available on the FTP server.
   *
   * @return The name of the latest release (e.g., "20130502").
   */
  def findLatestVersion1000Genomes(): String = {
    val ftpClient = new FTPClient()
    val server = "ftp.1000genomes.ebi.ac.uk"
    val directory = "/vol1/ftp/release/"

    try {
      ftpClient.connect(server)
      ftpClient.login("anonymous", "")
      ftpClient.changeWorkingDirectory(directory)
      ftpClient.enterLocalPassiveMode()

      // List files/directories and print them for debugging
      val files = ftpClient.listNames()

      if (files == null || files.isEmpty) {
        throw new IOException(s"No releases found in directory: $directory")
      }

      // Filter directories that match the pattern (year_month or yyyyMMdd)
      val releases = files.filter(file => file.matches("\\d{4}_\\d{2}") || file.matches("\\d{8}"))

      if (releases.isEmpty) {
        println("No valid release directories found.")
        return ""
      }

      // Extract release names and sort them
      val releaseNumbers = releases.flatMap { release =>
        val numberPart = release.stripSuffix("/")
        if (numberPart.matches("\\d{8}")) Some(numberPart) else None
      }

      releaseNumbers.sorted.reverse.headOption.getOrElse {
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

  /**
   * Check if a specific version exists on the FTP server.
   *
   * @param version The version (release folder) to check for (e.g., "20130502").
   * @return True if the version exists, false otherwise.
   */
  def isVersionPresent(version: String): Boolean = {
    val ftpClient = new FTPClient()
    val server = "ftp.1000genomes.ebi.ac.uk"
    val directory = "/vol1/ftp/release/"

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

      // Check if the version exists in the list of directories
      files.contains(version)
    } catch {
      case e: IOException =>
        println(s"Error: ${e.getMessage}")
        false
    } finally {
      if (ftpClient.isConnected) {
        ftpClient.logout()
        ftpClient.disconnect()
      }
    }
  }

}
