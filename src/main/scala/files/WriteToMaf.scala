package files

import scala.collection.mutable.ListBuffer
import java.io.{File, PrintWriter}
import data.DnaVariant
import data.VariantType

object WriteToMaf{
  /**
   * Write a list of DNA variants to a MAF file.
   *
   * @param variants   List of DNA variants to write
   * @param outputPath Path to MAF file
   * @param append     Whether to append to an existing file or create a new one
   */
  def writeMafFile(variants: ListBuffer[DnaVariant], outputPath: String, append: Boolean): Unit = {
    val fileExists = new File(outputPath).exists()
    val writer = new PrintWriter(new java.io.FileWriter(outputPath, append))

    if (!fileExists || !append) {
      // Write header only if file is new
      val mafHeaders = "Hugo_Symbol\tChrom\tStart_Pos\tEnd_Pos\tNCBI_Build\tRef_Allele\tAlt_Allele" +
        "\tVSQR_Score\tVariant_Classification\tVariant_Type\tVariant_Type_Protein\tAllele_Freq\tGene_Type_Gencode\tHGVS_DNA\tHGVS_RNA\tHGVS_Protein\tPDB_ID_Uniprot" +
         "\tAF_1000G\tAMR_AF_1000G\tAFR_AF_1000G\tEUR_AF_1000G\tSAS_AF_1000G\tEAS_AF_1000G" + "\tEntrez_Gene_Id_Gencode\tStrand_Plus_Gencode\tTranscript_id_Gencode" +
        "\tTranscript_name_Gencode\tExon_id_Gencode\tExon_number_Gencode\tTranscript_type_Gencode\tLevel_Gencode"
      writer.println(mafHeaders)
    }

    variants.foreach { variant =>
      val mafEntry = createMafEntry(variant)
      writer.println(mafEntry)
    }

    writer.close()
  }

  def createMafEntry(dnaVariant: DnaVariant): String = {
    val fields = Seq(
      dnaVariant.geneName_Gencode,
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
      dnaVariant.geneType_Gencode,
      dnaVariant.HGVSDNA,
      dnaVariant.HGVSRNA,
      dnaVariant.HGVSProtein,
      dnaVariant.pdbID,
      dnaVariant.AF_1000G,
      dnaVariant.AMR_AF_1000G,
      dnaVariant.AFR_AF_1000G,
      dnaVariant.EUR_AF_1000G,
      dnaVariant.SAS_AF_1000G,
      dnaVariant.EAS_AF_1000G,
      dnaVariant.geneID_Gencode,
      dnaVariant.strandPlus_Gencode,
      dnaVariant.transID_Gencode,
      dnaVariant.transName_Gencode,
      dnaVariant.exonID_Gencode,
      dnaVariant.exonNum_Gencode,
      dnaVariant.transType_Gencode,
      dnaVariant.level_Gencode
    )

    fields.mkString("\t")
  }

  def classifyVariant(dnaVariant: DnaVariant): String = {
    dnaVariant.varType match {
      case VariantType.SNP => "Missense_Mutation"
      case VariantType.INS => if ((dnaVariant.altAllele.length - dnaVariant.refAllele.length) % 3 == 0) "Inframe_Insertion" else "Frameshift_Insertion"
      case VariantType.DEL => if ((dnaVariant.refAllele.length - dnaVariant.altAllele.length) % 3 == 0) "Inframe_Deletion" else "Frameshift_Deletion"
      case VariantType.INDEL => if ((dnaVariant.altAllele.length - dnaVariant.refAllele.length) % 3 == 0) "Inframe_Indel" else "Frameshift_Indel"
      case VariantType.RPT => "Repeat_Variant"
      case VariantType.INV => "Inversion_Variant"
      case VariantType.ALLELES => "Allelic_Variant"
      case VariantType.EXT => "External_Variant"
      case VariantType.FS => "Frameshift_Variant"
      case VariantType.DUP => "Duplication_Variant"
      case VariantType.Other => "Unknown"
    }
  }
}
