import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer


object WriteToMaf {

  def writeMafFile(variants: ListBuffer[DnaVariant], outputPath: String): Unit = {
    val mafHeaders = "Hugo_Symbol\tChrom\tStart_Pos\tEnd_Pos\tNCBI_Build\tRef_Allele\tTumor_Seq_Allele2" +
        "\tVSQR_Score\tVariant_Classification\tVariant_Type\tAllele_Freq\tCopy_Num\tGene_Type\tEntrez_Gene_Id\tTranscript_id" +
        "\tTranscript_name\tExon_id\tExon_number\tTranscript_type\tLevel"

    val writer = new PrintWriter(new File(outputPath))
    writer.println(mafHeaders)

    variants.foreach { variant =>
      val mafEntry = createMafEntry(variant) // Convert DnaVariant to MAF entry format
      writer.println(mafEntry)
    }

    writer.close()
  }
  def createMafEntry(dnaVariant: DnaVariant): String = {
    val hugoSymbol = dnaVariant.geneName
    val entrezGeneId = dnaVariant.geneID
    val chromosome = dnaVariant.contig
    val startPos = dnaVariant.position
    val endPos = dnaVariant.position
    val refAllele = dnaVariant.refAllele
    val tumorSeqAllele2 = dnaVariant.altAllele
    val varClassification = classifyVariant(dnaVariant)
    val varType = dnaVariant.varType
    val alleleFreq = dnaVariant.alleleFreq
    val copyNum = dnaVariant.copyNum
    val vqsrScore = dnaVariant.VQSR_score
    val geneType = dnaVariant.geneType
    val NCBI = dnaVariant.NCBIBuild
    val transID = dnaVariant.transID
    val transName = dnaVariant.transName
    val transType = dnaVariant.transType
    val exonNum = dnaVariant.exonNum
    val exonID = dnaVariant.exonID
    val level = dnaVariant.level

    s"$hugoSymbol\t$chromosome\t$startPos\t$endPos\t$NCBI\t$refAllele\t$tumorSeqAllele2\t$vqsrScore\t$varClassification" +
    s"\t$varType\t$alleleFreq\t$copyNum\t$geneType\t$entrezGeneId\t$transID\t$transName\t$exonID\t$exonNum\t$transType\t$level"
  }
  //Get variant classification from type
  def classifyVariant(dnaVariant: DnaVariant): String = {
    dnaVariant.varType match {
      case VariantType.SNP => "Missense_Mutation"
      case VariantType.Indel => "Frameshift_Indel"
      case VariantType.Other => "Unknown"
    }
  }
}
