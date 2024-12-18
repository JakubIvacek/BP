package ftp

import org.apache.commons.net.ftp.FTPClient
import java.io.{FileOutputStream, IOException}

object FtpClient {

  /**
   * Download a specific file from a ftp server.
   *
   * @param localPath The local directory where the file should be saved.
   * @param fileName  The specific file to download (e.g., "gencode.v15.annotation.gff3.gz").
   * @param serverName    The ftp server name (e.g., ftp.ebi.ac.uk)
   * @param directoryName The directory path on specific ftp server
   *
   */
  def downloadSpecificFile(localPath: String, fileName: String, serverName: String, directoryName: String): Unit = {
    val ftpClient = new FTPClient()
    val server = serverName
    val directory = directoryName

    try {
      // Connect to the FTP server
      ftpClient.connect(server)
      ftpClient.login("anonymous", "")

      // Change to the release directory
      ftpClient.changeWorkingDirectory(directory)

      // Check if the specific file exists
      val files = ftpClient.listNames()
      if (files == null || !files.contains(fileName)) {
        throw new IOException(s"File not found: $fileName in directory: $directory")
      }
      // Create the local directory if it doesn't exist
      val localDir = new java.io.File(localPath)
      if (!localDir.exists()) {
        localDir.mkdirs()
      }

      // Download the file
      val localFile = new java.io.File(localDir, fileName)
      val outputStream = new FileOutputStream(localFile)
      try {
        println(s"Downloading $fileName to ${localFile.getAbsolutePath}")
        if (!ftpClient.retrieveFile(fileName, outputStream)) {
          throw new IOException(s"Failed to download file: $fileName")
        }
      } finally {
        outputStream.close()
      }
    } catch {
      case e: IOException =>
        println(s"Error: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      // Close the FTP connection
      if (ftpClient.isConnected) {
        ftpClient.logout()
        ftpClient.disconnect()
      }
    }
  }

  /** GENCODE STUFF */

  /**
   * Download Gencode annotation file for the latest release.
   *
   * @param localPath The local directory where the file should be saved.
   */
  def downloadLatestGencodeAnnotation(localPath: String): Unit = {
    val latestRelease = findLatestVersionGencode()
    val directory = s"/pub/databases/gencode/Gencode_human/$latestRelease/"
    val server = "ftp.ebi.ac.uk"
    val fileName = s"gencode.v${latestRelease.stripPrefix("release_")}.annotation.gff3.gz"
    if (latestRelease.nonEmpty) {
      downloadSpecificFile(localPath, fileName, server, directory)
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }
  /**
   * Download Gencode annotation file for the specific release.
   *
   * @param localPath The local directory where the file should be saved.
   * @param releaseNumber The release number
   */
  def downloadSpecificGencodeAnnotation(localPath: String, releaseNumber: Int): Unit = {
    val release = s"release_$releaseNumber"
    val directory = s"/pub/databases/gencode/Gencode_human/$release/"
    val server = "ftp.ebi.ac.uk"
    val fileName = s"gencode.v${release.stripPrefix("release_")}.annotation.gff3.gz"
    if (release.nonEmpty) {
      downloadSpecificFile(localPath, fileName, server, directory)
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }
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
      // Connect to the FTP server
      ftpClient.connect(server)
      ftpClient.login("anonymous", "")

      // Change to the target directory
      ftpClient.changeWorkingDirectory(directory)

      // List files/directories
      val files = ftpClient.listNames()
      if (files == null || files.isEmpty) {
        throw new IOException(s"No releases found in directory: $directory")
      }

      val releases = files.filter(file => file.startsWith("release_"))

      // Extract numeric release numbers and find the newest
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
      // Close the connection
      if (ftpClient.isConnected) {
        ftpClient.logout()
        ftpClient.disconnect()
      }
    }
  }
}