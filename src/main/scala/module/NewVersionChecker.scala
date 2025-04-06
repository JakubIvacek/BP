package module
import database.modules.ServiceModules
import ftp.{FtpClient1000genomes, FtpClientGencode}
/**
 * The NewVersionChecker object is responsible for checking if there is new version
 * of modules available if so downloading them
 */
object NewVersionChecker {

  /**
   * Checks if there is new version available for each module if there is download it
   */
  def checkNewVersion(moduleName: String, getNewestVersion: String => String, findLatestVersionFtp: () => String, downloadModule: String => Unit): Unit = {
    val newest = getNewestVersion(moduleName) // retrieve newest from db
    val ftpNewest = findLatestVersionFtp().stripPrefix("release_") // retrieve newest from ftp

    if (ftpNewest.toInt > newest.toInt) {
      println(s"New version of $moduleName found downloading...")
      downloadModule("")
    } else {
      println(s"No new version of $moduleName.")
    }
  }

  def checkNewVersions(): Unit = {
    // Check GenCode module
    checkNewVersion("gencode", ServiceModules.getNewestModuleVersion, FtpClientGencode.findLatestVersionGencode, GenCodeModule.downloadModuleLatest)

    // Check 1000 Genomes module
    checkNewVersion("1000Genomes", ServiceModules.getNewestModuleVersion, FtpClient1000genomes.findLatestVersion1000Genomes, Genomes1000Module.downloadModuleLatest)

    // Check Cosmic module

  }
}
