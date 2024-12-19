package module
import ftp.{FtpClientGencode, FtpClient}
import database.{ServiceModules, RepositoryModules}
import utils.RepositoryManager

object GenCodeModule extends ModuleManager {
  /**
   * Download Gencode annotation files hg38, specific release
   *
   * @param localPath The local directory where the files should be saved.ň
   * @param releaseNumber The release number
   */
  override def downloadModule(localPath: String, releaseNumber: String): Unit = {
    val release = s"release_$releaseNumber"
    val directory = s"/pub/databases/gencode/Gencode_human/$release/"
    val server = "ftp.ebi.ac.uk"
    val referenceFile = "GRCh38.primary_assembly.genome.fa.gz"
    val fileName = s"gencode.v${release.stripPrefix("release_")}.annotation.gff3.gz"
    val finalLocalPath = s"$localPath\\gencode\\$release\\hg38"
    if (release.nonEmpty) {
      FtpClient.downloadSpecificFile(localPath + s"\\gencode\\$releaseNumber\\hg38", fileName, server, directory)
      //FtpClient.downloadSpecificFile(finalLocalPath, referenceFile, server, directory)
      ServiceModules.addModuleToDatabase("gencode", releaseNumber, s"$localPath\\gencode\\$releaseNumber\\hg38",
        s"$server$directory", false, "hg38")
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }

  /**
   * Download Gencode annotation files hg38, latest release
   *
   * @param localPath The local directory where the files should be saved.
   */
  override def downloadModuleLatest(localPath: String): Unit = {
    val latestRelease = FtpClientGencode.findLatestVersionGencode()
    val directory = s"/pub/databases/gencode/Gencode_human/$latestRelease/"
    val server = "ftp.ebi.ac.uk"
    val fileName = s"gencode.v${latestRelease.stripPrefix("release_")}.annotation.gff3.gz"
    val referenceFile = "GRCh38.primary_assembly.genome.fa.gz"
    val finalLocalPath = s"$localPath\\gencode\\${latestRelease.stripPrefix("release_")}\\hg38"
    if (latestRelease.nonEmpty) {
      FtpClient.downloadSpecificFile(finalLocalPath, fileName, server, directory)
      FtpClient.downloadSpecificFile(finalLocalPath, referenceFile, server, directory)
      ServiceModules.addModuleToDatabase("gencode", latestRelease.stripPrefix("release_"),
        s"$localPath\\gencode\\${latestRelease.stripPrefix("release_")}\\hg38", s"$server$directory", false, "hg38")
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }

  /**
   * Remove Gencode module files by information
   *
   * @param name              Module name
   * @param release           Module release
   * @param versionReference  Module reference version
   */
  override def removeModule(name: String, release: String, versionReference: String): Unit = {
    val module = ServiceModules.getModuleFromDatabase(name, release, versionReference)
    module match {
      case Some(module) =>
        RepositoryManager.deleteRepository(module.locationPath.getOrElse("N/A")) //delete if location path present
      case None =>
        println("No module found with this information.")
    }
    ServiceModules.deleteModuleFromDatabase(name, release, versionReference) //delete from database
  }

  /**
   * Remove Gencode module files by id
   *
   * @param id             Module id
   */
  override def removeModuleById(id: Int): Unit = {
    val module = ServiceModules.getModuleFromDatabaseById(id)
    module match {
      case Some(module) =>
        RepositoryManager.deleteRepository(module.locationPath.getOrElse("N/A")) //delete if location path present
      case None =>
        println("No module found with this information.")
    }
    ServiceModules.deleteModuleFromDatabaseById(id) //delete from database
  }

  /**
   * Print all saved gencode modules information
   */
  override def printAllClassModules(): Unit = {
    val modules = ServiceModules.getModulesByName("gencode")
    if (modules.isEmpty){
      println("No gencode modules installed")
    }else{
      println("GENCODE MODULES -")
      modules.foreach(_.print())
    }
  }

  /**
   * Print all saved modules information
   */
  override def printAllModules(): Unit = {
    val modules = ServiceModules.getModules 
    if (modules.isEmpty) {
      println("No modules installed")
    } else {
      println("ALL MODULES -")
      modules.foreach(_.print())
    }
  }
}
