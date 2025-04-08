package anotation

import cosmic.FaLoadCOSMIC
import data.DnaVariant

object AnnotationCosmic {
  /**
   * Annotate a single DNA variant using the COSMIC annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantCosmic(variant: DnaVariant, referenceGenome: String): Unit = {
    
    if FaLoadCOSMIC.loadedList.isEmpty then FaLoadCOSMIC.loadFastaFromGzip("data/cosmic/v101/hg38/Cosmic_Genes_v101_GRCh38.fasta.gz")
    
  }
}
