package module

import database.{DatabaseConnection, Module, QueryModules, TablesCreate}
import org.apache.commons.net.ftp.FTPClient
import ftp.{FtpClient, FtpClientGencode}

import java.sql.Timestamp
import java.time.Instant

object GencodeLatestRelease {
  def main(args: Array[String]): Unit = {
   FtpClientGencode.downloadLatestGencodeAnnotation("C:\\Users\\ivace")
    
  }
}

