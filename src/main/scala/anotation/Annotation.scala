package anotation

import data.VariantType.Other
import data.{DnaVariant, GffEntry, VariantType}
import files.{FastaReader, FileReaderVcf, GFFReader, WriteToMaf}
import hgvs.{HGVSCoding, Utils, CodonAmino}

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
   * @param inputFile The path to the input VCF file containing DNA variants.
   * @param outputFile The path to the output MAF file where the annotated variants will be written.
   * @param referenceGenome The reference genome used for annotation (e.g., "hg38").
   */
  def annotate(inputFile: String, outputFile: String, referenceGenome: String): Unit = {
    // Read the VCF file and extract DNA variants
    val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(inputFile)

    //val path = database.modules.ServiceModules.getNewestModulePathGenCode("hg38")
    // Load the GFF3 file containing Gencode annotations if not already loaded.
    if (!GFFReader.isLoaded) GFFReader.preloadGff3File("gencode.v47.annotation.gff3")

    // Annotate the variants
    annotateVariants(dnaVariants.toList, referenceGenome)

    // Write the annotated variants to MAF file.
    WriteToMaf.writeMafFile(dnaVariants, outputFile)
  }

  /**
   * Annotate a list of DNA variants
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   */
  def annotateVariants(dnaVariants: List[DnaVariant], referenceGenome: String): Unit = {
    dnaVariants.foreach(variant =>
      annotateVariantGencode(variant, referenceGenome)
      //here can be added annotateVariantGnomAD ...
    )
  }

  /**
   * Annotate a single DNA variant using the Gencode annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantGencode(variant: DnaVariant, referenceGenome: String): Unit = {

    val intervalTree = GFFReader.getIntervalTree(variant.contig)

    // Search for overlapping entries
    val overlappingEntries: Seq[GffEntry] = {
      val entries = intervalTree.search(variant.contig, variant.position.toInt)
      if (entries.nonEmpty) entries
      else {
        (intervalTree.findClosestUpstream(variant.contig, variant.position.toInt).toSeq ++
          intervalTree.findClosestDownstream(variant.contig, variant.position.toInt).toSeq)
      }
    }
    // assignAttributes
    variant.geneID = getAttribute(overlappingEntries, "gene_id")
    variant.geneName = prioritizeGeneName(overlappingEntries)
    variant.geneType = getAttribute(overlappingEntries, "gene_type")
    variant.transID = getAttribute(overlappingEntries, "transcript_id")
    variant.transName = getAttribute(overlappingEntries, "transcript_name")
    variant.transType = getAttribute(overlappingEntries, "transcript_type")
    variant.exonID = getAttribute(overlappingEntries, "exon_id")
    variant.exonNum = getAttribute(overlappingEntries, "exon_number")
    variant.level = getAttribute(overlappingEntries, "level")
    variant.NCBIBuild = referenceGenome
    //set var type
    variant.varType = VariantTypeAnnotation.returnVariantTypeDnaRna(variant.refAllele, variant.altAllele)
    // check if maped to coding region so protein level annotation
    val proteinEntryOpt = overlappingEntries.find(entry => entry.attributes.contains("protein_id"))
    if (proteinEntryOpt.isDefined) {
      val proteinEntry = proteinEntryOpt.get
      variant.proteinVarType = VariantTypeAnnotation.returnVariantTypeProtein(variant, variant.refAllele, variant.altAllele, proteinEntry)
    }
    HGVSCoding.variantAddHGVS(variant, overlappingEntries)
    
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
}
