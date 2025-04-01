package anotation

import data.VariantType.Other
import data.{DnaVariant, GffEntry, VariantType}
import files.{FileReaderVcf, WriteToMaf, GFFReaderSW, FastaReaderSW}
import hgvs.HGVS
import utils.Gunzip

object AnnotationGencode {
  private var faPathSaved: Option[String] = None

  def annotateGencodeSetup(referenceGenome: String): Option[String] = {
    // SET UP GENCODE
    val modulePaths = database.modules.ServiceModules.getNewestModulePathGenCode(referenceGenome)
    val (pathGencode, faPathGencode) = modulePaths match {
      case Some((p, f)) => (p, f)
      case None =>
        println("Gencode module not found. Please download Gencode first.")
        return None 
    }
    GFFReaderSW.loadGffFile(pathGencode)
    Some(faPathGencode)
  }

  /**
   * Annotate a single DNA variant using the Gencode annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantGencode(variant: DnaVariant, referenceGenome: String): Unit = {
    val faPath = faPathSaved.getOrElse {
      faPathSaved = annotateGencodeSetup(referenceGenome)
      faPathSaved.getOrElse("") // Return empty string if faPathSaved is still None
    }
    if faPath.isEmpty then return // means gencode is not installed so cant annotate
    
    variant.positionEnd = VariantTypeAnnotation.calculateEndPosition(variant)
    GFFReaderSW.ensureVariantInWindow(variant.positionEnd.toInt, variant.contig) //load more if needed

    var overlappingEntries = {
      val overlaps = GFFReaderSW.loadedEntries.filter(gene =>
        gene.contig == variant.contig &&
          gene.start < variant.position &&
          gene.end > variant.position &&
          gene.start < variant.positionEnd &&
          gene.end > variant.positionEnd
      )

      if (overlaps.nonEmpty) {
        overlaps.toSeq // Convert to immutable Seq
      } else {
        Seq.empty // Immutable empty sequence
      }
    }
    val matchingEntries = overlappingEntries.filter { entry =>
      val refSequence = FastaReaderSW.getSequence(faPath, entry.contig, entry.start, entry.end, entry.strandPlus)

      // Calculate offset of variant position within the entry
      val offset = (variant.position - entry.start).toInt
      val refAlleleLength = variant.refAllele.length

      // Ensure offset and full reference allele range are within bounds
      if (offset >= 0 && (offset + refAlleleLength) <= refSequence.length) {
        val refBaseAtVariant = refSequence.substring(offset, offset + refAlleleLength) // Extract matching-length reference bases
        refBaseAtVariant == variant.refAllele
      } else {
        false // Skip entries where the variant range is out of bounds
      }
    }

    // assignAttributes
    variant.geneID = getAttribute(matchingEntries, "gene_id")
    variant.geneName = prioritizeAttribute(matchingEntries, "gene_name")
    variant.geneType = getAttribute(matchingEntries, "gene_type")
    variant.transID = prioritizeAttribute(matchingEntries, "transcript_id")
    variant.transName = prioritizeAttribute(matchingEntries, "transcript_name")
    variant.transType = prioritizeAttribute(matchingEntries, "transcript_type")
    variant.exonID = prioritizeAttribute(matchingEntries, "exon_id")
    variant.exonNum = getAttribute(matchingEntries, "exon_number")
    variant.level = getAttribute(matchingEntries, "level")
    variant.NCBIBuild = referenceGenome
    //set var type
    variant.varType = VariantTypeAnnotation.returnVariantTypeDnaRna(variant.refAllele, variant.altAllele)

    // Check if the variant is mapped to a CDS region
    val cdsEntryOpt = matchingEntries.find(entry =>
      entry.attributes.contains("protein_id") && entry.attributes.get("gene_type").contains("protein_coding")
    )
    if (cdsEntryOpt.isDefined) {
      //If the variant is within a CDS, perform protein-level annotation
      val cdsEntry = cdsEntryOpt.get
      variant.proteinVarType = VariantTypeAnnotation.returnVariantTypeProtein(variant, variant.refAllele, variant.altAllele, cdsEntry, faPath)
    }
    HGVS.variantAddHGVS(variant, matchingEntries)

  }

  /**
   * Retrieve a specific attribute value from the list of overlapping GFF entries.
   *
   * @param entries The list of overlapping GFF entries.
   * @param key     The key (attribute name) to retrieve.
   * @return The value of the attribute, or a placeholder if not found.
   */
  def getAttribute(entries: Seq[GffEntry], key: String): String =
    entries.flatMap(_.attributes.get(key)).distinct.mkString(",") match {
      case "" => "."
      case result => result
    }

  /**
   * Prioritize the name in the list of overlapping GFF entries.
   * If there are multiple gene names, prioritize those not starting with "ENSG".
   *
   * @param entries The list of overlapping GFF entries.
   * @return The prioritized gene name or "." if none is found.
   */
  def prioritizeAttribute(entries: Seq[GffEntry], attributeName: String): String = {
    val geneNames = entries.flatMap(_.attributes.get(attributeName)).distinct
    geneNames.find(!_.startsWith("ENSG"))
      .orElse(geneNames.find(_.startsWith("ENSG")))
      .getOrElse(".")
  }
}
