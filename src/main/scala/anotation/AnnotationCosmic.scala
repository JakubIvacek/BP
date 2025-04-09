package anotation

import cosmic.FALoadCOSMIC
import data.DnaVariant

object AnnotationCosmic {
  /**
   * Annotate a single DNA variant using the COSMIC annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantCosmic(variant: DnaVariant, referenceGenome: String): Unit = {
    
    if FALoadCOSMIC.loadedList.isEmpty then FALoadCOSMIC.loadFastaFromGzip("data/cosmic/v101/hg38/Cosmic_Genes_v101_GRCh38.fasta.gz")
    
  }
}
