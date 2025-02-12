package utils

import java.nio.file.{Files, Paths}
import scala.sys.process._
import java.nio.charset.StandardCharsets

object ExtractPDB {
  def extractPdbMappings(inputFilePath: String, outputFilePath: String): Unit = {
    val command = Seq("zgrep", "DR   PDB", inputFilePath)
    val outputFile = Paths.get(outputFilePath)

    try {
      val builder = new StringBuilder

      // Capture output using ProcessLogger and write it manually
      val processStatus = command.!(ProcessLogger(line => builder.append(line).append("\n")))

      if (processStatus == 0) {
        Files.write(outputFile, builder.toString.getBytes(StandardCharsets.UTF_8))
        println(s"PDB mappings successfully written to $outputFilePath")
      } else {
        println("Command execution failed.")
      }
    } catch {
      case e: Exception =>
        println(s"Error extracting PDB mappings: ${e.getMessage}")
    }
  }
}
