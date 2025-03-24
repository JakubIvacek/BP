package commandLine

import anotation.Annotation

import java.io.PrintWriter
import module.{GenCodeModule, Genomes1000Module}
import org.rogach.scallop.*

// sbt "run -d gencode -p /path/save (optional path if saves to .log file)"        DOWNLOAD LATEST
// sbt "run -d gencode -p /path/save" -v 43  (optional path if saves to .log file) DOWNLOAD VERSION
// sbt "run -r 1"                                   REMOVE ID
// sbt "run -i gencode"                             PRINT INFO
// sbt "run -f filename -rv version -op /output/path"   ANNOTATION not connceted yet
// sbt "run -h"                                     HELP

class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
  val remove = opt[Int]("r", required = false, descr = "Remove module by ID (1,2)")
  val download = opt[String]("d", required = false, descr = "Download module name (gencode, gnomad ...)")
  val version = opt[String]("v", required = false, descr = "Module version (13,20)")
  val info = opt[String]("i", required = false, descr = "Print info module name (genocde, gnomad ...)")
  val help = opt[Boolean]("h", noshort = true, descr = "Show help message")
  val filename = opt[String]("f", required = false, descr = "Filename for annotation (Lynch.vcf)")
  val referenceVersion = opt[String]("a", required = false, descr = "Reference version for annotation (hg38, t2t)")
  val path = opt[String]("p", required = false, descr = "File path for download (/path/dir) (optional path saves to .log file)")
  val outPath = opt[String]("o", required = false, descr = "File path for output (/path/file_name.maf)")
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

    if (conf.help()) {
      conf.printHelp()
    } else {
      // sbt run -d name  no -p tries to retrieve from .log file
      if (conf.download.isDefined) {
        val path = PathSaver.getPath
        path match{
          case Some(path) => downloadModuleLatest(conf.download(), path)
          case None =>  println("No path found enter command with -p dir/save")
        }
      }
      // sbt run -d name -v version no -p tries to retrieve from .log file
      if (conf.download.isDefined && conf.version.isDefined) {
        val path = PathSaver.getPath
        path match {
          case Some(path) => downloadModule(conf.download(), path, conf.version())
          case None => println("No path found enter command with -p dir/save")
        }
      }
      // sbt run -d name -p path
      if (conf.download.isDefined && conf.path.isDefined) {
        println(s"Download latest module: ${conf.download()} with path: ${conf.path()}")
        downloadModuleLatest(conf.download(), conf.path())
      }
      // sbt run -d name -v version -p path
      if (conf.download.isDefined && conf.path.isDefined && conf.version.isDefined) {
        //add path to log file if not added
        println(s"Download latest module: ${conf.download()} with path: ${conf.path()} version : ${conf.version()}")
        downloadModule(conf.download(), conf.path(), conf.version())
      }
      // sbt run -r id
      if (conf.remove.isDefined) {
        println(s"Remove module with ID: ${conf.remove()}")
        GenCodeModule.removeModuleById(conf.remove())
      }
      //sbt run -i name
      if (conf.info.isDefined) {
        println(s"Print: ${conf.info()}")
        printInformation(conf.info())
      }
      // sbt run -f filename -a referenceVersion -o path
      if (conf.filename.isDefined && conf.referenceVersion.isDefined && conf.outPath.isDefined) {
        val file = conf.filename()
        val version = conf.referenceVersion()
        val outPath = conf.outPath()
        if (version != "hg38" && version != "t2t") println("Enter referenceVersion : hg38 or t2t")
        else{
          println(s"Annotate with file: $file and reference version: $version out path: ${outPath}")
          Annotation.annotateInBatches(file,  outPath, version)
        }
      }
    }
  }

  def printInformation(name: String): Unit = {
    if (name == "gencode"){
      GenCodeModule.printAllClassModules()
    }
    else if (name == "1000genomes"){
      Genomes1000Module.printAllClassModules()
    }
    // GNOMAD ... ADD DALSIE
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
    else {
      println("Wrong module name Try again (Gencode, ...)")
    }
  }
}



