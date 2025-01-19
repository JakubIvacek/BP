package module
import database.modules.ServiceModules
import ftp.FtpClientGencode
/**
 * The NewVersionChecker object is responsible for checking if there is new version
 * of modules available if so downloading them
 */
object NewVersionChecker {

  /**
   * Checks if there is new version available for each module if there is download it
   */
  def checkNewVersions(): Unit = {
    // check GenCode module
    val newest = ServiceModules.getNewestModuleVersion("gencode") // retrieve newest from db
    val ftpNewest = FtpClientGencode.findLatestVersionGencode().stripPrefix("release_") // retrieve newest from ftp
    if (ftpNewest.toInt > newest.toInt) {
      println("New version of GenCode found downloading...")
      GenCodeModule.downloadModuleLatest("")
    } else println("No new version of GenCode.")
    
    // next module ...
    
  }
}
