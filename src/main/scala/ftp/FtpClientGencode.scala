package ftp

import org.apache.commons.net.ftp.FTPClient

import java.io.IOException
import database.ServiceModules

object FtpClientGencode {

  /**
   * Download Gencode annotation file hg38 for the latest release.
   *
   * @param localPath The local directory where the file should be saved.
   */
  def downloadLatestGencodeAnnotation(localPath: String): Unit = {
    val latestRelease = findLatestVersionGencode()
    val directory = s"/pub/databases/gencode/Gencode_human/$latestRelease/"
    val server = "ftp.ebi.ac.uk"
    val fileName = s"gencode.v${latestRelease.stripPrefix("release_")}.annotation.gff3.gz"
    val referenceFile = "GRCh38.primary_assembly.genome.fa.gz"
    val finalLocalPath = s"$localPath\\gencode\\${latestRelease.stripPrefix("release_")}\\hg38"
    if (latestRelease.nonEmpty) {
      FtpClient.downloadSpecificFile(finalLocalPath, fileName, server, directory)
      FtpClient.downloadSpecificFile(finalLocalPath, referenceFile, server, directory)
      //ServiceModules.addModuleToDatabase("gencode", latestRelease.stripPrefix("release_"), s"$localPath\\gencode\\hg38", directory, false)
      //ServiceModules.deleteModuleFromDatabase("gencode", latestRelease.stripPrefix("release_"))
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }

  /**
   * Download Gencode annotation file hg38 for the specific release.
   *
   * @param localPath     The local directory where the file should be saved.
   * @param releaseNumber The release number
   */
  def downloadSpecificGencodeAnnotation(localPath: String, releaseNumber: Int): Unit = {
    val release = s"release_$releaseNumber"
    val directory = s"/pub/databases/gencode/Gencode_human/$release/"
    val server = "ftp.ebi.ac.uk"
    val referenceFile = "GRCh38.primary_assembly.genome.fa.gz"
    val fileName = s"gencode.v${release.stripPrefix("release_")}.annotation.gff3.gz"
    if (release.nonEmpty) {
      FtpClient.downloadSpecificFile(localPath + "\\gencode\\hg38", fileName, server, directory)

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
