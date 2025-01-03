package module
import ftp.{FtpClient, FtpClientGencode}
import database.modules.{RepositoryModules, ServiceModules}
import utils.{Gunzip, LiftOverGFF, LiftOverVcf, RepositoryManager, FileStuff}
object GenCodeModule extends ModuleManager {

  private val referenceFile = "GRCh38.primary_assembly.genome.fa.gz"  // name of reference file on ftp server
  private val server = "ftp.ebi.ac.uk"                                // ftp server
  /**
   * Download Gencode annotation files hg38, specific release
   *
   * @param localPath The local directory where the files should be saved.ň
   * @param releaseNumber The release number
   */
  override def downloadModule(localPath: String, releaseNumber: String): Unit = {
    val latestRelease = FtpClientGencode.findLatestVersionGencode()
    var release = s"release_$releaseNumber"
    // check if not higher release version entered (if yes download latest)
    release = if releaseNumber > latestRelease.stripPrefix("release_") then latestRelease else release
    val newReleaseNumber = if releaseNumber > latestRelease.stripPrefix("release_") then latestRelease.stripPrefix("release_") else releaseNumber

    val directory = s"/pub/databases/gencode/Gencode_human/$release/"
    val fileName = s"gencode.v${release.stripPrefix("release_")}.annotation.gff3.gz"
    // LINUX PATH
    val finalLocalPath = if localPath == "" then s"gencode/$newReleaseNumber/hg38" else s"$localPath/gencode/$newReleaseNumber/hg38"
    // WINDOWS PATH
    //val finalLocalPath = if localPath == "" then s"gencode\\$newReleaseNumber\\hg38" else s"$localPath\\gencode\\$newReleaseNumber\\hg38"
    if (release.nonEmpty) {
      // Download module and save
      FtpClient.downloadSpecificFile(finalLocalPath, fileName, server, directory)
      FtpClient.downloadSpecificFile(finalLocalPath, referenceFile, server, directory)
      ServiceModules.addModuleToDatabase("gencode", newReleaseNumber, finalLocalPath,
        s"$server$directory", false, "hg38")
      // Overlift module to T2T and save
      val finalOverLiftPath = if localPath == "" then s"gencode/$newReleaseNumber/t2t" else s"$localPath/gencode/$newReleaseNumber/t2t"
      overLiftToT2T(finalOverLiftPath, newReleaseNumber, server + directory, finalLocalPath, fileName)
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
    if (latestRelease.nonEmpty) {
      downloadModule(localPath, latestRelease.stripPrefix("release_"))
    } else {
      println("Could not determine the latest Gencode release.")
    }
  }

  /**
   * overLiftToT2T to T2T reference version
   *
   * @param outputPath The local directory where the files should be saved.
   * @param releaseNumber The release number of module (e.g., 34)
   * @param downloadPath  The url path to ftp server
   * @param filePath The path to file to overlift
   * @param fileName The name of the file to overlift
   */
  override def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String,
                             fileName: String): Unit = {
    Gunzip.unzipFile(s"$filePath/$fileName")
    val outputFileName = fileName.stripSuffix(".gz")
    LiftOverGFF.liftOverGFF(s"$filePath/$outputFileName", outputPath, outputFileName)
    Gunzip.zipFile(s"$filePath/$outputFileName")
    Gunzip.zipFile(s"$outputPath/$outputFileName")
    FileStuff.copyFile("reference/t2t/chm13v2.0.fa", s"$outputPath/chm13v2.0.fa")
    ServiceModules.addModuleToDatabase("gencode", releaseNumber, outputPath, downloadPath, false, "t2t")
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
        ServiceModules.deleteModuleFromDatabaseById(module.id.getOrElse(-1)) //delete from database
      case None =>
        println("No module found with this information.")
    }
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
