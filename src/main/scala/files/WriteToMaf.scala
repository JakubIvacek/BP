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
    val mafHeaders = "Hugo_Symbol\tChrom\tStart_Pos\tEnd_Pos\tNCBI_Build\tRef_Allele\tAlt_Allele" +
        "\tVSQR_Score\tVariant_Classification\tVariant_Type\tVariant_Type_Protein\tAllele_Freq\tGene_Type__Gencode\tHGVS_DNA\tHGVS_RNA\tHGVS_Protein\tPDB_ID__Uniprot" +
      "\tEntrez_Gene_Id__Gencode\tTranscript_id__Gencode" +
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
      dnaVariant.positionEnd,
      dnaVariant.NCBIBuild,
      dnaVariant.refAllele,
      dnaVariant.altAllele,
      dnaVariant.VQSR_score,
      classifyVariant(dnaVariant),
      dnaVariant.varType,
      dnaVariant.proteinVarType,
      dnaVariant.alleleFreq,
      dnaVariant.geneType,
      dnaVariant.HGVSDNA,
      dnaVariant.HGVSRNA,
      dnaVariant.HGVSProtein,
      dnaVariant.pdbID,
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
      case VariantType.SNP =>
        "Missense_Mutation"
      case VariantType.INS =>
        if ((dnaVariant.altAllele.length - dnaVariant.refAllele.length) % 3 == 0)
          "Inframe_Insertion"
        else
          "Frameshift_Insertion"
      case VariantType.DEL =>
        if ((dnaVariant.refAllele.length - dnaVariant.altAllele.length) % 3 == 0)
          "Inframe_Deletion"
        else
          "Frameshift_Deletion"
      case VariantType.INDEL =>
        if ((dnaVariant.altAllele.length - dnaVariant.refAllele.length) % 3 == 0)
          "Inframe_Indel"
        else
          "Frameshift_Indel"
      case VariantType.RPT =>
        "Repeat_Variant"
      case VariantType.INV =>
        "Inversion_Variant"
      case VariantType.ALLELES =>
        "Allelic_Variant"
      case VariantType.EXT =>
        "External_Variant"
      case VariantType.FS =>
        "Frameshift_Variant"
      case VariantType.DUP =>
        "Duplication_Variant"
      case VariantType.Other =>
        "Unknown"
    }
  }
}
