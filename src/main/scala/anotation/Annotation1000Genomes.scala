package anotation

import data.DnaVariant
import files.VcfReaderSW

import scala.util.Try

object Annotation1000Genomes {
  private var directory: Option[String] = None
  private var activeConting: String = ""
  /**
   * Annotate a single DNA variant using the 1000Genomes annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariant1000Genomes(variant: DnaVariant, referenceGenome: String): Unit = {
    // if not installed return
    directory = directory.orElse(database.modules.ServiceModules.get1000GenomesPath)
    if directory.isEmpty then return // means 1000genomes is not installed so cant annotate

    if activeConting == "" || activeConting != variant.contig then {
      activeConting = variant.contig
      VcfReaderSW.loadVcfFile(s"${directory.getOrElse("")}/${getVcfFile(activeConting)}")

      println("Loaded file " + s"${directory.getOrElse("")}/${getVcfFile(activeConting)}" )
    }
    // SLIDE WINDOW VCF
    VcfReaderSW.ensureVariantInWindow(variant.position.toInt)
    // FIND MATCHING ENTRIES
    var matchingEntries = {
      val overlaps = VcfReaderSW.loadedEntries.filter(entry =>
        entry.chrom == variant.contig && entry.pos == variant.position
          && entry.alt == variant.altAllele && entry.ref == variant.refAllele

      )

      if (overlaps.nonEmpty) {
        overlaps.toSeq // Convert to immutable Seq
      } else {
        Seq.empty // Immutable empty sequence
      }
    }
    // ANNOTATE WITH MATCHING ENTRY
    if (matchingEntries.nonEmpty) {
      val entry = matchingEntries.head
      //println("Matching entries - " + matchingEntries.length)
      // Separate info field into pairs
      val infoField = entry.info.split(";").map { keyValue =>
        val parts = keyValue.split("=")
        parts.head -> parts.lift(1).getOrElse("")
      }.toMap
      // Extract the relevant population frequencies and add to variant
      variant.AMR_AF_1000G = infoField.get("AMR_AF").flatMap(value => Try(value.toDouble).toOption).map(_.toString).getOrElse(".")
      variant.AFR_AF_1000G = infoField.get("AFR_AF").flatMap(value => Try(value.toDouble).toOption).map(_.toString).getOrElse(".")
      variant.EUR_AF_1000G = infoField.get("EUR_AF").flatMap(value => Try(value.toDouble).toOption).map(_.toString).getOrElse(".")
      variant.SAS_AF_1000G = infoField.get("SAS_AF").flatMap(value => Try(value.toDouble).toOption).map(_.toString).getOrElse(".")
      variant.EAS_AF_1000G = infoField.get("EAS_AF").flatMap(value => Try(value.toDouble).toOption).map(_.toString).getOrElse(".")
      variant.AF_1000G = infoField.get("AF").flatMap(value => Try(value.toDouble).toOption).map(_.toString).getOrElse(".")
      
    }
  }




  // Function to get the VCF file for a given chromosome.
  def getVcfFile(chromosome: String): String = {
    Genomes1000Files.getOrElse(chromosome, "")
  }

  private val Genomes1000Files: Map[String, String] = Map(
    "chr1" -> "ALL.chr1.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr2" -> "ALL.chr2.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr3" -> "ALL.chr3.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr4" -> "ALL.chr4.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr5" -> "ALL.chr5.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr6" -> "ALL.chr6.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr7" -> "ALL.chr7.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr8" -> "ALL.chr8.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr9" -> "ALL.chr9.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr10" -> "ALL.chr10.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr11" -> "ALL.chr11.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr12" -> "ALL.chr12.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr13" -> "ALL.chr13.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr14" -> "ALL.chr14.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr15" -> "ALL.chr15.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr16" -> "ALL.chr16.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr17" -> "ALL.chr17.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr18" -> "ALL.chr18.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr19" -> "ALL.chr19.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr20" -> "ALL.chr20.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr21" -> "ALL.chr21.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chr22" -> "ALL.chr22.phase3_shapeit2_mvncall_integrated_v5b.20130502.genotypes.vcf.gz",
    "chrMT" -> "ALL.chrMT.phase3_callmom-v0_4.20130502.genotypes.vcf.gz",
    "chrX" -> "ALL.chrX.phase3_shapeit2_mvncall_integrated_v1c.20130502.genotypes.vcf.gz",
    "chrY" -> "ALL.chrY.phase3_integrated_v2b.20130502.genotypes.vcf.gz"
  )

}
