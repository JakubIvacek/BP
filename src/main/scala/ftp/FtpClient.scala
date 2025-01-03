package ftp

import org.apache.commons.net.ftp.{FTP, FTPClient}

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
      ftpClient.connect(server)
      ftpClient.login("anonymous", "")
      ftpClient.changeWorkingDirectory(directory)
      ftpClient.enterLocalPassiveMode()
      ftpClient.setFileType(FTP.BINARY_FILE_TYPE)
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
        println(s"Downloaded $fileName to ${localFile.getAbsolutePath}")
        outputStream.close()
      }
    } catch {
      case e: IOException =>
        println(s"Error: ${e.getMessage}")
        e.printStackTrace()
    } finally {
      if (ftpClient.isConnected) {
        ftpClient.logout()
        ftpClient.disconnect()
      }
    }
  }
}