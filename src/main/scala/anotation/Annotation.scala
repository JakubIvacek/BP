package anotation

import data.VariantType.Other
import data.{DnaVariant, GffEntry, VariantType}
import files.{FastaReader, FileReaderVcf, GFFReader, WriteToMaf, GFFReader2}
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
   * @param inputFile The path to the input VCF file containing DNA variants.
   * @param outputFile The path to the output MAF file where the annotated variants will be written.
   * @param referenceGenome The reference genome used for annotation (e.g., "hg38").
   */
  def annotate(inputFile: String, outputFile: String, referenceGenome: String): Unit = {
    // Read the VCF file and extract DNA variants
    val dnaVariants: ListBuffer[DnaVariant] = FileReaderVcf.read(inputFile)
    
    // Load the GFF3 file containing Gencode annotations if not already loaded.
    //val path = database.modules.ServiceModules.getNewestModulePathGenCode("hg38")
    //val finalPath = path.getOrElse("")
    //Gunzip.unzipFile(finalPath)
    //val unzipedFile = finalPath.stripSuffix(".gz")
    //if (!GFFReader.isLoaded) GFFReader.preloadGff3File(unzipedFile)
    //if (!GFFReader.isLoaded) GFFReader.preloadGff3File("gencode.v47.annotation.gff3")
    //val faPath = database.modules.ServiceModules.getReferenceFilePathGenCode(referenceGenome).getOrElse("")
    //Gunzip.unzipFile(faPath)
    //val faUnzipped = faPath.stripSuffix(".gz")
    // Annotate the variants
    val faUnzipped = "reference/hg38/GRCh38.primary_assembly.genome.fa"
    GFFReader2.loadGffFile("gencode.v47.annotation.gff3")
    annotateVariants(dnaVariants.toList, referenceGenome, faUnzipped)

    // Write the annotated variants to MAF file.
    WriteToMaf.writeMafFile(dnaVariants, outputFile)
    GFFReader2.close()
    //Gunzip.zipFile(unzipedFile)
    //Gunzip.zipFile(faUnzipped)
  }

  /**
   * Annotate a list of DNA variants
   *
   * @param dnaVariants     A list of DNA variants to annotate.
   * @param referenceGenome The reference genome to use for annotation (e.g., "hg38").
   */
  def annotateVariants(dnaVariants: List[DnaVariant], referenceGenome: String, faPath: String): Unit = {
    dnaVariants.foreach(variant =>
      annotateVariantGencode(variant, referenceGenome, faPath)
      //here can be added annotateVariantGnomAD ...
    )
  }

  /**
   * Annotate a single DNA variant using the Gencode annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantGencode(variant: DnaVariant, referenceGenome: String, faPath: String): Unit = {

    //val intervalTree = GFFReader.getIntervalTree(variant.contig)

    // Search for overlapping entries
    //val overlappingEntries: Seq[GffEntry] = {
    //  val entries = intervalTree.search(variant.contig, variant.position.toInt, variant.positionEnd.toInt)
    //  if (entries.nonEmpty) entries
    //  else {
    //    (intervalTree.findClosestUpstream(variant.contig, variant.position.toInt).toSeq ++
    //      intervalTree.findClosestDownstream(variant.contig, variant.position.toInt).toSeq)
    //  }
    //}
    variant.positionEnd = VariantTypeAnnotation.calculateEndPosition(variant)
    GFFReader2.ensureVariantInWindow(variant.positionEnd.toInt, variant.contig) //load more if needed

    val overlappingEntries = {
      val overlaps = GFFReader2.loadedEntries.filter(gene =>
        gene.contig == variant.contig &&
          gene.start < variant.position &&
          gene.end > variant.position &&
          gene.start < variant.positionEnd &&
          gene.end > variant.positionEnd
      )

      if (overlaps.nonEmpty) {
        overlaps.toSeq // Convert to immutable Seq
      } else {
        //println("closest")
        // If no overlap found, find the closest upstream or downstream gene
        //val sameContigLoaded = GFFReader2.loadedEntries.filter(_.contig == variant.contig)
        //if (sameContigLoaded.nonEmpty) {
        //  val closestGene = sameContigLoaded
        //    .filter(entry => !entry.attributes.contains("protein_id") && !entry.attributes.get("gene_type").contains("protein_coding"))
        //    .minByOption(entry => Math.abs(entry.start - variant.position.toInt))
        // closestGene match {
        //   case Some(gene) => Seq(gene)
        //   case None => Seq.empty // Return an empty sequence if no closest gene is found
        // }
        //} else {
        Seq.empty // Immutable empty sequence
      }
    }

    // assignAttributes
    variant.geneID = getAttribute(overlappingEntries, "gene_id")
    variant.geneName = prioritizeGeneName(overlappingEntries)
    variant.geneType = getAttribute(overlappingEntries, "gene_type")
    variant.transID = prioritizeAttribute(overlappingEntries, "transcript_id")
    variant.transName = prioritizeAttribute(overlappingEntries, "transcript_name")
    variant.transType = prioritizeAttribute(overlappingEntries, "transcript_type")
    variant.exonID = prioritizeAttribute(overlappingEntries, "exon_id")
    variant.exonNum = getAttribute(overlappingEntries, "exon_number")
    variant.level = getAttribute(overlappingEntries, "level")
    variant.NCBIBuild = referenceGenome
    //set var type
    variant.varType = VariantTypeAnnotation.returnVariantTypeDnaRna(variant.refAllele, variant.altAllele)

    // Check if the variant is mapped to a CDS region
    val cdsEntryOpt = overlappingEntries.find(entry =>
      entry.attributes.contains("protein_id") && entry.attributes.get("gene_type").contains("protein_coding")
    )
    if (cdsEntryOpt.isDefined) {
       //If the variant is within a CDS, perform protein-level annotation
      val cdsEntry = cdsEntryOpt.get
      variant.proteinVarType = VariantTypeAnnotation.returnVariantTypeProtein(variant, variant.refAllele, variant.altAllele, cdsEntry, faPath)
    }
    HGVS.variantAddHGVS(variant, overlappingEntries)
    
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
