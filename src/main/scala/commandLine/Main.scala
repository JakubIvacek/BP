package commandLine

import anotation.Annotation
import database.annotationruns.ServiceAnnotationRuns
import logfiles.{ConfigCredentials, PathSaver, RefChainDirManager}
import module.{CosmicModule, GenCodeModule, Genomes1000Module, UniprotModule}
import org.rogach.scallop.*
import utils.ModuleNewerVersionChecker

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {

  banner(
    """|Usage: sbt run [options]
       |
       |  -h / --help
       |      displays help
       |
       |  -n / --newer
       |      checks for newer versions of scientific databases and downloads them if needed
       |
       |  -c <folder> / --chain <folder>  -a <folder> / --reference <folder>
       |      sets and saves paths to:
       |        • chain files (for “overlift” coordinate mapping between genome builds)
       |        • reference files (DNA sequences for the chosen build, e.g. hg38, chm13)
       |      if not set, prints guidance on required directory structure
       |
       |  -d <module> / --download <module>  -v <version> / --version <version>?
       |      downloads a specified scientific module; version is optional (defaults to latest)
       |
       |  -r <id> / --remove <id>
       |      removes a scientific database by its ID
       |
       |  -z / --annotations
       |      lists all annotation runs stored in the SQLite database
       |
       |  -e <email> / --email <email>  -w <password> / --password <password>
       |      saves COSMIC login credentials into cred.config
       |
       |  -i [<module>] / --info [<module>]
       |      prints info about downloaded modules; if none specified, shows all
       |
       |  -f <input.vcf> / --filename <input.vcf>  -a <genome> / --reference <genome>  -o <output.maf> / --out_path <output.maf>
       |      annotates the input VCF with the chosen reference genome and produces a MAF output
       |""".stripMargin
  )

  val remove = opt[Int](short = 'r', name = "remove", required = false, descr = "Remove module by ID (1,2)")
  val download = opt[String](short = 'd', name = "download", required = false, descr = "Download module name (gencode, gnomad ...)")
  val version = opt[String](short = 'v', name="version", required = false, descr = "Module version (13,20)")
  val info = opt[String](short = 'i', name = "info", required = false, descr = "Print info module name (gencode, gnomad ...)")
  val help = opt[Boolean](short = 'h', name = "help", descr = "Show help message")
  val filename = opt[String](short = 'f', name = "filename", required = false, descr = "Filename for annotation (Lynch.vcf)")
  val referenceVersion = opt[String](short = 'a', name = "reference", required = false, descr = "Reference version for annotation (hg38, t2t) or path to reference dir (/path/)")
  val chainFiles = opt[String](short = 'c', name= "chain", required = false, descr = "Chain files directory path (/path/dir)")
  val path = opt[String](short = 'p', name = "d_path", required = false, descr = "File path for download (/path/dir) (optional path saves to .log file)")
  val outPath = opt[String](short = 'o', name = "out_path", required = false, descr = "File path for output (/path/dir)")
  val checkNew = opt[Boolean](short = 'n', name = "newer", required = false, descr = "Download latest version modules if not on device. Usage to check new versions or download all.")
  val email =  opt[String](short = 'e', name = "email", required = false, descr = "Credentials email set up to Cosmic")
  val password =  opt[String](short = 'w', name = "password", required = false, descr = "Credentials password set up to Cosmic")
  val annotationRuns = opt[Boolean](short = 'z', name = "annotations", required = false, descr = "Print from database all annotation runs")
  // Call verify after defining all options
  verify()
}

object Main {

  def main(args: Array[String]): Unit = {
    // Ensure args are passed from the command line
    if (args == null || args.isEmpty) {
      println("No arguments provided! Please provide some arguments.")
      sys.exit(1) // Exit if no args are provided
    }

    val conf = new Conf(args) // Pass args to Conf
    conf match {
      case c if c.help() =>
        sys.exit(0)

      case c if c.checkNew() =>
        println("Checking if new version available for modules")
        ModuleNewerVersionChecker.checkNewVersions()
      case c if c.annotationRuns() =>
        println("Printing all annotation runs")
        val annotationRuns = ServiceAnnotationRuns.getAllAnnotationRuns
        if annotationRuns.nonEmpty then annotationRuns.foreach(_.print()) else println("No annotation runs found.")
      case c if c.chainFiles.isDefined && c.referenceVersion.isDefined =>
        println(s"Saving dir paths chain - ${c.chainFiles()} ref - ${c.referenceVersion()}")
        RefChainDirManager.savePathsToLogFile(c.referenceVersion(), c.chainFiles())

      case c if RefChainDirManager.getPaths.isEmpty =>
        println(
          """|Before using COMMANDS set up CHAIN , REFERENCE files directory path
             |By using command - sbt run -c CHAIN/DIR -a REFERENCE/DIR
             |reference dir should contain : chm13.fa , hg38.fa
             |chain dir should contain : chm13-hg38.over.chain , hg38-chm13.over.chain
           """.stripMargin)
        sys.exit(1)
      case c if c.email.isDefined && c.password.isDefined =>
        println(s"Saving credentials to cred.config : ${c.email()}  ${c.password()}")
        ConfigCredentials.saveConfig(c.email(), c.password())
      case c if c.download.isDefined && c.path.isEmpty && c.version.isEmpty =>
        PathSaver.getPath match {
          case Some(path) => downloadModuleLatest(c.download(), path)
          case None => println("No path found; use -p to specify save directory")
        }

      case c if c.download.isDefined && c.version.isDefined =>
        PathSaver.getPath match {
          case Some(path) => downloadModule(c.download(), path, c.version())
          case None => println("No path found; use -p to specify save directory")
        }

      case c if c.download.isDefined && c.path.isDefined && c.version.isEmpty =>
        PathSaver.savePathToLogFile(c.path())
        println(s"Download latest module ${c.download()} to ${c.path()}")
        downloadModuleLatest(c.download(), c.path())

      case c if c.download.isDefined && c.path.isDefined && c.version.isDefined =>
        PathSaver.savePathToLogFile(c.path())
        println(s"Download module ${c.download()} version ${c.version()} to ${c.path()}")
        downloadModule(c.download(), c.path(), c.version())

      case c if c.remove.isDefined =>
        println(s"Remove module with ID: ${c.remove()}")
        GenCodeModule.removeModuleById(c.remove())

      case c if c.info.isDefined =>
        println(s"Print: ${c.info()}")
        printInformation(c.info())

      case c if c.filename.isDefined && c.referenceVersion.isDefined && c.outPath.isDefined =>
        val file = c.filename()
        val version = c.referenceVersion()
        val outPath = c.outPath()
        version match {
          case v@("hg38" | "t2t") =>
            println(s"Annotate file : $file reference : $v → output dir : $outPath")
            Annotation.annotateInBatches(file, outPath, v)
          case _ =>
            println("Enter referenceVersion: hg38 or t2t")
        }

      case _ =>
        println("Unknown command or missing arguments; try `-h` for help.")
    }
  }

  def printInformation(name: String): Unit = {
    if (name == "gencode"){
      GenCodeModule.printAllClassModules()
    }
    else if (name == "1000genomes"){
      Genomes1000Module.printAllClassModules()
    }
    else if (name == "uniprot"){
      UniprotModule.printAllClassModules()
    }
    else if (name == "cosmic"){
      CosmicModule.printAllClassModules()
    }
    else{
      println("Wrong module Printing all. (Gencode, ...)")
      GenCodeModule.printAllModules()
    }
  }

  def downloadModuleLatest(name: String, path: String): Unit = {
    if (name == "gencode") {
      GenCodeModule.downloadModuleLatest(path)
    }
    else if (name == "1000genomes"){
      Genomes1000Module.downloadModuleLatest(path)
    }
    else if (name == "uniprot"){
      UniprotModule.downloadModuleLatest(path)
    }else if (name == "cosmic"){
      CosmicModule.downloadModuleLatest(path)
    }
    else{
      println("Wrong module name Try again (Gencode, ...)")
    }
  }

  def downloadModule(name: String, path: String, version: String): Unit = {
    if (name == "gencode") {
      GenCodeModule.downloadModule(path, version)
    }
    else if (name == "1000genomes"){
      Genomes1000Module.downloadModule(path, version)
    }
    else if (name == "uniprot"){
      UniprotModule.downloadModule(path, version)
    }
    else if (name == "cosmic"){
      CosmicModule.downloadModule(path, version)
    }
    else {
      println("Wrong module name Try again (Gencode, ...)")
    }
  }
}



