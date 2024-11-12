import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer


object WriteToMaf {
  
  def writeMafFile(variants: ListBuffer[DnaVariant], outputPath: String): Unit = {
    val mafHeaders = "Hugo_Symbol\tChromosome\tStart_Position\tEnd_Position\tReference_Allele\tTumor_Seq_Allele2\tVariant_Classification\tVariant_Type\tAllele_Frequency"

    val writer = new PrintWriter(new File(outputPath))
    writer.println(mafHeaders)

    variants.foreach { variant =>
      val mafEntry = createMafEntry(variant) // Convert DnaVariant to MAF entry format
      writer.println(mafEntry)
    }

    writer.close()
  }
  def createMafEntry(dnaVariant: DnaVariant): String = {
    val hugoSymbol = "unknown"
    val chromosome = dnaVariant.contig
    val startPos = dnaVariant.position
    val endPos = dnaVariant.position
    val refAllele = dnaVariant.refAllele
    val tumorSeqAllele2 = dnaVariant.altAllele
    val varClassification = classifyVariant(dnaVariant)
    val varType = dnaVariant.varType
    val alleleFreq = dnaVariant.alleleFreq

    s"$hugoSymbol\t$chromosome\t$startPos\t$endPos\t$refAllele\t$tumorSeqAllele2\t$varClassification\t$varType\t$alleleFreq"
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
