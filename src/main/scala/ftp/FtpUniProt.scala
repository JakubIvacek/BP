package ftp

import database.modules.ServiceModules
import org.apache.commons.net.ftp.{FTP, FTPClient}

import scala.util.control.Breaks.*
import java.sql.Timestamp

object FtpUniProt {
  
  
  private val remoteFilePath: String = "/pub/databases/uniprot/knowledgebase/complete/"


  /**
   * Checks for Uniprot module on fpt server if there is newer version available
   * Compares timestamp from database and from file on server to determine if newer is there
   */
  def checkForNewerVersion( dbTimestamp: Timestamp): Boolean = {
    val ftpClient = new FTPClient()
    var result = false 

    breakable { 
      try {
        // Connect to the FTP server
        ftpClient.connect("ftp.uniprot.org")
        ftpClient.login("anonymous", "")
        ftpClient.enterLocalPassiveMode()
        ftpClient.setFileType(FTP.BINARY_FILE_TYPE)

        // Change directory to the path where the file is located
        ftpClient.changeWorkingDirectory(remoteFilePath)
        // List the files in the directory
        val files = ftpClient.listFiles()
        files.foreach { file =>
          // Get the file modification timestamp from the FTP server
          val remoteTimestamp = file.getTimestamp().getTime()
          if (remoteTimestamp.getTime > dbTimestamp.getTime) {
            result = true
            break
          }
        }
      } catch {
        case e: Exception =>
          e.printStackTrace()
          result = false 
      } finally {
        ftpClient.disconnect()
      }
    }

    result 
  }
}
