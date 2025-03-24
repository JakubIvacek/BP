package anotation

import data.VariantType.Other
import data.{DnaVariant, GffEntry, VariantType}
import files.{FileReaderVcf, WriteToMaf, GFFReaderSW, FastaReaderSW}
import hgvs.HGVS
import utils.Gunzip
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * The `Annotation` object is responsible for annotating DNA variants with genomic information from VCF files
 * data sources and outputting the results in the MAF format.
 */
object Annotation {


  /**
   * Main method to process the input VCF file, annotate the variants, and write the result to an output MAF file.
   *
   * @param inputFile       The path to the input VCF file containing DNA variants.
   * @param outputFile      The path to the output MAF file where the annotated variants will be written.
   * @param referenceGenome The reference genome used for annotation (e.g., "hg38").
   * @param batchSize       Size of vcf batch to annotate at once                       
   */
  def annotateInBatches(inputFile: String, outputFile: String, referenceGenome: String, batchSize: Int = 1000): Unit = {
    FileReaderVcf.open(inputFile) // Open the VCF reader once
    // SET UP GENCODE
    val modulePaths = database.modules.ServiceModules.getNewestModulePathGenCode("hg38")
    val (pathGencode, faPathGencode) = modulePaths match {
      case Some((p, f)) => (p, f)
      case None =>
        println("Gencode module not found. Please download Gencode first.")
        return // Stops execution here
    }
    GFFReaderSW.loadGffFile(pathGencode)
    //GFFReader2.loadGffFile("gencode.v47.annotation.gff3") // Load GFF annotations once
    
    
    var batchCount = 1
    var hasMoreVariants = true

    while (hasMoreVariants) {

      val dnaVariants = FileReaderVcf.readBatch(batchSize)
      if (dnaVariants.isEmpty) {
        hasMoreVariants = false
      } else {
        println(s"Processing batch $batchCount... ${dnaVariants.toList.head.contig}")
        annotateVariants(dnaVariants.toList, referenceGenome, faPathGencode)

        WriteToMaf.writeMafFile(dnaVariants, outputFile, append = batchCount > 1)
        batchCount += 1
      }
    }

    // Cleanup
    FileReaderVcf.close() // Close the VCF reader when done
    GFFReaderSW.close() // Close GFFReader once after processing all batches
  }
  /**
   * Annotate a list of DNA variants
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   * @param faPathGencode   Path to .fa file from gencode                       
   */
  def annotateVariants(dnaVariants: List[DnaVariant], referenceGenome: String, faPathGencode: String): Unit = {
    dnaVariants.foreach(variant =>
      annotateVariantGencode(variant, referenceGenome, faPathGencode) //GENCODE
      Annotation1000Genomes.annotateVariant1000Genomes(variant, referenceGenome)   //1000GENOMES
      //...
    )
  }
  


  /**
   * Annotate a single DNA variant using the Gencode annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantGencode(variant: DnaVariant, referenceGenome: String, faPath: String): Unit = {
    
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
    variant.geneName = prioritizeGeneName(matchingEntries)
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
   * Prioritize the gene name in the list of overlapping GFF entries.
   * If there are multiple gene names, prioritize those not starting with "ENSG".
   *
   * @param entries The list of overlapping GFF entries.
   * @return The prioritized gene name or "." if none is found.
   */
  def prioritizeGeneName(entries: Seq[GffEntry]): String = {
    val geneNames = entries.flatMap(_.attributes.get("gene_name")).distinct
    geneNames.find(!_.startsWith("ENSG"))
      .orElse(geneNames.find(_.startsWith("ENSG")))
      .getOrElse(".")
  }

  def prioritizeAttribute(entries: Seq[GffEntry], attributeName: String): String = {
    val geneNames = entries.flatMap(_.attributes.get(attributeName)).distinct
    geneNames.find(!_.startsWith("ENSG"))
      .orElse(geneNames.find(_.startsWith("ENSG")))
      .getOrElse(".")
  }
}
