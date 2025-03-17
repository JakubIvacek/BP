package pdb

import scala.sys.process._
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

import java.nio.file.{Files, Paths}
import scala.sys.process._
import java.nio.charset.StandardCharsets

object ExtractPDB {

  /**
   * Extracts PDB mappings **along with** the corresponding UniProt ID
   * @param inputFilePath Path to the UniProt .dat.gz file
   * @param outputFilePath Output file where extracted mappings will be stored
   */
  def extractPdbMappings(inputFilePath: String, outputFilePath: String): Unit = {
    val command = Seq("zgrep", "-E", "^(ID|DR   PDB;)", inputFilePath) // Get UniProt ID and PDB lines
    val outputFile = Paths.get(outputFilePath)

    try {
      val builder = new StringBuilder
      var currentUniprotId: Option[String] = None // Store the last seen UniProt ID

      // Capture output using ProcessLogger and process manually
      val processStatus = command.!(ProcessLogger { line =>
        if (line.startsWith("ID   ")) {
          // Extract UniProt ID
          currentUniprotId = Some(line.split("\\s+")(1).split("_")(0)) // Extract ID like "Q6GZX4"
        } else if (line.startsWith("DR   PDB;") && currentUniprotId.nonEmpty) {
          // Extract PDB mapping and associate with UniProt ID
          builder.append(s"${currentUniprotId.get}\t$line\n")
        }
      })

      if (processStatus == 0) {
        Files.write(outputFile, builder.toString.getBytes(StandardCharsets.UTF_8))
        println(s" PDB mappings with UniProt IDs successfully written to $outputFilePath")
      } else {
        println("Command execution failed.")
      }
    } catch {
      case e: Exception =>
        println(s" Error extracting PDB mappings: ${e.getMessage}")
    }
  }
}





