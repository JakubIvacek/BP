package module

import database.modules.ServiceModules
import ftp.{FtpClient, FtpClient1000genomes}
import logfiles.RefChainDirManager
import utils.{FileStuff, Gunzip, LiftOverTool, RepositoryManager}

object Genomes1000Module extends ModuleManager {
  private val server = "ftp.1000genomes.ebi.ac.uk"
  private val directory = "vol1/ftp/release/20130502/"
  // LIST OF FILES TO BE DOWNLOADED FROM SERVER
  private val filesToDownload = List(
    "ALL.chr1.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr1.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr2.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr2.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr3.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr3.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr4.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr4.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr5.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr5.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr6.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr6.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr7.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr7.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr8.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr8.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr9.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr9.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr10.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr10.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr11.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr11.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr12.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr12.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr13.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr13.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr14.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr14.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr15.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr15.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr16.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr16.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr17.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr17.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr18.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr18.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr19.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr19.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr20.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr20.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr21.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr21.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chr22.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "ALL.chr22.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz.tbi",
    "ALL.chrMT.phase3_callmom-v0_4.20130502.genotypes.vcf.gz",
    "ALL.chrMT.phase3_callmom-v0_4.20130502.genotypes.vcf.gz.tbi",
    "ALL.chrX.phase3_shapeit2_mvncall_integrated_v1c.20130502.genotypes.vcf.gz",
    "ALL.chrX.phase3_shapeit2_mvncall_integrated_v1c.20130502.genotypes.vcf.gz.tbi",
    "ALL.chrY.phase3_integrated_v2b.20130502.genotypes.vcf.gz",
    "ALL.chrY.phase3_integrated_v2b.20130502.genotypes.vcf.gz.tbi"
  )

  /**
   * Downloads the latest release of 1000Genomes annotation files
   *
   * @param localPath The local directory where the files should be saved.
   */
  def downloadModuleLatest(localPath: String): Unit = {
    val latestVersion = FtpClient1000genomes.findLatestVersion1000Genomes()
    if (latestVersion.nonEmpty) {
      downloadModule(localPath, latestVersion)
    } else {
      println("Could not determine the latest 1000genomes release.")
    }
  }

  /**
   * Downloads a specific release of 1000Genomes annotation files for hg38.
   *
   * @param localPath     The local directory where the files should be saved.
   * @param version       The 1000genomes release number (e.g., "39").
   */
  def downloadModule(localPath: String, version: String): Unit = {

    val finalLocalPath = if localPath == "" then s"1000genomes/$version/hg38" else s"$localPath/1000genomes/$version/hg38"
    val versionInstalledCheck = ServiceModules.getModuleFromDatabase("1000genomes", version, "hg38")
    if (versionInstalledCheck.isEmpty && FtpClient1000genomes.isVersionPresent(version)) {
      // DOWNLOAD
      filesToDownload.foreach { file =>
        //println(s"Downloading $file...")
        FtpClient.downloadSpecificFile(finalLocalPath, file, server, directory)
      }
      ServiceModules.addModuleToDatabase("1000genomes", version, finalLocalPath, s"$server$directory", false, "hg38")
      
      // OVERLIFT TO T2T
      val finalOverliftPath = if localPath == "" then s"1000genomes/$version/t2t" else s"$localPath/1000genomes/$version/t2t"
      overLiftToT2T(finalOverliftPath, version, server + directory, finalLocalPath, filesToDownload)
    }
  }

  /**
   * Overlifts 1000Genomes annotation files from hg38 to the T2T reference version.
   *
   * @param outputPath    The local directory where the files should be saved.
   * @param releaseNumber The Gencode release number.
   * @param downloadPath  The URL path to the FTP server.
   * @param filePath      The path to the input file to be overlifted.
   * @param fileNames     The List of the files to be overlifted.
   */
  def overLiftToT2T(outputPath: String, releaseNumber: String, downloadPath: String, filePath: String,
                    fileNames: List[String]): Unit = {
    fileNames.foreach { file =>
      if (!file.endsWith(".tbi")) {
        LiftOverTool.liftOverVcf(s"$filePath/$file", outputPath, file)
      }
    }
    val refPath = RefChainDirManager.getReferenceFileDir.getOrElse("")
    FileStuff.copyFile(s"$refPath/chm13v2.0.fa", s"$outputPath/chm13v2.0.fa")
    ServiceModules.addModuleToDatabase("1000genomes", releaseNumber, outputPath, s"$server$directory", true, "t2t")
  }

  /**
   * Removes a 1000GENOMES module by its unique ID.
   *
   * @param id The module ID.
   */
  def removeModuleById(id: Int): Unit = {
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
   * Prints information about all installed 1000genomes modules.
   */
  def printAllClassModules(): Unit = {
    val modules = ServiceModules.getModulesByName("1000genomes")
    if (modules.isEmpty) {
      println("No 1000genomes modules installed")
    } else {
      println("1000genomes MODULES -")
      modules.foreach(_.print())
    }
  }

  /**
   * Prints information about all modules.
   */
  def printAllModules(): Unit = {
    val modules = ServiceModules.getModules
    if (modules.isEmpty) {
      println("No modules installed")
    } else {
      println("ALL MODULES -")
      modules.foreach(_.print())
    }
  }
}
