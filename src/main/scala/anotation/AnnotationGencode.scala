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
          gene.start < variant.position && gene.end > variant.position &&
          gene.start < variant.positionEnd && gene.end > variant.positionEnd
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
    
    // ASSIGN ATTRIBUTES
    val selectedExon = matchingEntries.find(entry => entry.attributes.contains("exon_id"))
    if (selectedExon.isDefined) {
      // Use exon data if available
      assignAttributes(selectedExon.get, variant)
    } else {
      // If no exon, check for a transcript
      val selectedTranscript = matchingEntries.find(entry => entry.attributes.contains("transcript_id"))
      if (selectedTranscript.isDefined) {
        // Use transcript data if available
        assignAttributes(selectedTranscript.get, variant)
      } else {
        // If no transcript, check for gene
        val selectedGene = matchingEntries.find(entry => entry.attributes.contains("gene_id"))
        if (selectedGene.isDefined) {
          // Use gene data if available
          assignAttributes(selectedGene.get, variant)
        }
      }
    }
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

    HGVS.variantAddHGVS(variant, matchingEntries, faPath)

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
   * If there are multiple gene names for example, prioritize those not starting with "ENSG".
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

  /**
   * Helper function to extract attributes and assign them to the variant
   */
  def assignAttributes(entry: GffEntry, variant: DnaVariant): Unit = {
    variant.exonID_Gencode = entry.attributes.getOrElse("exon_id", ".")
    variant.exonNum_Gencode = entry.attributes.getOrElse("exon_number", ".")
    variant.geneID_Gencode = entry.attributes.getOrElse("gene_id", ".")
    variant.geneName_Gencode = entry.attributes.getOrElse("gene_name", ".")
    variant.geneType_Gencode = entry.attributes.getOrElse("gene_type", ".")
    variant.transID_Gencode = entry.attributes.getOrElse("transcript_id", ".")
    variant.transName_Gencode = entry.attributes.getOrElse("transcript_name", ".")
    variant.transType_Gencode = entry.attributes.getOrElse("transcript_type", ".")
    variant.level_Gencode = entry.attributes.getOrElse("level", ".")
    variant.strandPlus_Gencode = if entry.strandPlus then "+" else "-"
  }
}
