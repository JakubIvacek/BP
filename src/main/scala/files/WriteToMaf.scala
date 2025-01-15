package files

import data.{DnaVariant, VariantType}

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

/**
 * The `WriteToMaf` object is responsible for writing DNA variant data to a Mutation Annotation Format (MAF) file.
 *
 */
object WriteToMaf {

  /**
   * Write a list of DNA variants to a MAF file.
   *
   * @param variants   A list buffer containing DNA variants to be written to the MAF file.
   * @param outputPath The file path where the MAF file will be saved.
   */
  def writeMafFile(variants: ListBuffer[DnaVariant], outputPath: String): Unit = {
    //create write header
    val mafHeaders = "Hugo_Symbol\tChrom\tStart_Pos\tEnd_Pos\tNCBI_Build\tRef_Allele\tTumor_Seq_Allele2" +
        "\tVSQR_Score\tVariant_Classification\tVariant_Type\tAllele_Freq\tGene_Type__Gencode\tEntrez_Gene_Id__Gencode\tTranscript_id__Gencode" +
        "\tTranscript_name__Gencode\tExon_id__Gencode\tExon_number__Gencode\tTranscript_type__Gencode\tLevel__Gencode"
    val writer = new PrintWriter(new File(outputPath))
    writer.println(mafHeaders)

    //write entries
    variants.foreach { variant =>
      val mafEntry = createMafEntry(variant) // Convert DnaVariant to MAF entry format
      writer.println(mafEntry)
    }

    writer.close()
  }

  /**
   * Convert a `DnaVariant` object into a single MAF file entry.
   *
   * @param dnaVariant The DNA variant to convert.
   * @return A string representing the DNA variant in MAF format.
   */
  def createMafEntry(dnaVariant: DnaVariant): String = {
    val fields = Seq(
      dnaVariant.geneName,
      dnaVariant.contig,
      dnaVariant.position,
      dnaVariant.position,
      dnaVariant.NCBIBuild,
      dnaVariant.refAllele,
      dnaVariant.altAllele,
      dnaVariant.VQSR_score,
      classifyVariant(dnaVariant),
      dnaVariant.varType,
      dnaVariant.alleleFreq,
      dnaVariant.geneType,
      dnaVariant.geneID,
      dnaVariant.transID,
      dnaVariant.transName,
      dnaVariant.exonID,
      dnaVariant.exonNum,
      dnaVariant.transType,
      dnaVariant.level
    )

    fields.mkString("\t")
  }

  /**
   * Determine the classification of a DNA variant based on its type.
   *
   * @param dnaVariant The DNA variant to classify.
   * @return A string representing the classification
   */
  def classifyVariant(dnaVariant: DnaVariant): String = {
    dnaVariant.varType match {
      case VariantType.SNP => "Missense_Mutation"
      case VariantType.Indel => "Frameshift_Indel"
      case VariantType.Other => "Unknown"
    }
  }
}
