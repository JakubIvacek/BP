package pdb

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.io.Source

object ExtractIsoforms {

  case class Isoform(id: String, sequence: String)
  case class UniProtProtein(id: String, canonicalSequence: String, isoforms: List[Isoform])

  def extractSequences(inputFilePath: String, outputFilePath: String): Unit = {
    val source = Source.fromFile(inputFilePath)
    val entries = source.mkString.split("\n//\n") // UniProt entries are separated by //
    val outputBuilder = new StringBuilder

    entries.foreach { entry =>
      val lines = entry.split("\n").map(_.trim)

      val idOpt = lines.find(_.startsWith("ID")).map(_.split("\\s+")(1))
      val accessionOpt = lines.find(_.startsWith("AC")).map(_.split(";")(0).stripPrefix("AC").trim)

      val canonicalSeq = new StringBuilder
      var inSeq = false
      for (line <- lines) {
        if (line.startsWith("SQ")) inSeq = true
        else if (inSeq && !line.startsWith("//")) canonicalSeq.append(line.replaceAll("\\s", ""))
      }

      val canonical = canonicalSeq.toString()
      if (accessionOpt.isEmpty || canonical.isEmpty) return

      val acc = accessionOpt.get
      val isoforms = parseIsoforms(lines.toList, canonical)

      // Output canonical + isoforms
      outputBuilder.append(s">Canonical|$acc-1\n$canonical\n")
      isoforms.foreach { iso =>
        outputBuilder.append(s">${iso.id}\n${iso.sequence}\n")
      }
    }

    Files.write(Paths.get(outputFilePath), outputBuilder.toString().getBytes(StandardCharsets.UTF_8))
    println(s"Isoform sequences written to: $outputFilePath")
    source.close()
  }

  def parseIsoforms(lines: List[String], canonical: String): List[Isoform] = {
    val isoformMap = mutable.Map[String, mutable.ListBuffer[(Int, Int, String)]]()
    var currentIsoIds = List.empty[String]

    lines.foreach { line =>
      if (line.startsWith("CC   -!- ALTERNATIVE PRODUCTS:")) {
        // Skip header
      } else if (line.startsWith("CC       Name=")) {
        currentIsoIds = extractIsoformIds(line)
      } else if (line.startsWith("CC         ")) {
        val varseq = parseVarSeqLine(line)
        if (varseq.nonEmpty) {
          currentIsoIds.foreach { isoId =>
            val buffer = isoformMap.getOrElseUpdate(isoId, mutable.ListBuffer())
            buffer += varseq.get
          }
        }
      }
    }

    isoformMap.map { case (isoId, edits) =>
      val editedSeq = applyVarSeqs(canonical, edits.toList)
      Isoform(isoId, editedSeq)
    }.toList
  }

  def extractIsoformIds(line: String): List[String] = {
    // Parses: CC       Name=Isoform 2; IsoId=P12345-2; Sequence=Displayed.
    val isoIdPattern = """IsoId=([\w\-]+)""".r
    isoIdPattern.findAllIn(line).matchData.map(_.group(1)).toList
  }

  def parseVarSeqLine(line: String): Option[(Int, Int, String)] = {
    // Parses: CC         VAR_SEQ  124..167; Missing (in isoform 2).
    val pattern = """VAR_SEQ\s+(\d+)\.\.(\d+); (.+?) \(""".r
    line match {
      case pattern(start, end, replacement) =>
        val replaced = replacement match {
          case "Missing" => ""
          case aaSeq => aaSeq.replaceAll(" ", "")
        }
        Some((start.toInt, end.toInt, replaced))
      case _ => None
    }
  }

  def applyVarSeqs(seq: String, edits: List[(Int, Int, String)]): String = {
    var newSeq = seq
    var offset = 0

    edits.sortBy(_._1).foreach { case (start, end, repl) =>
      val sIdx = start - 1 + offset
      val eIdx = end + offset
      newSeq = newSeq.substring(0, sIdx) + repl + newSeq.substring(eIdx)
      offset += repl.length - (end - start + 1)
    }
    newSeq
  }
}
