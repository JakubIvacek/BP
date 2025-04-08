package anotation

import cosmic.FAtoGFFaLoadCOSMIC
import data.DnaVariant

object AnnotationCosmic {
  /**
   * Annotate a single DNA variant using the COSMIC annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantCosmic(variant: DnaVariant, referenceGenome: String): Unit = {
    
    if FAtoGFFaLoadCOSMIC.loadedList.isEmpty then FAtoGFFaLoadCOSMIC.loadFastaFromGzip("data/cosmic/v101/hg38/Cosmic_Genes_v101_GRCh38.fasta.gz")
    
  }
}
