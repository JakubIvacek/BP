package anotation

import data.DnaVariant
import database.modules.ServiceModules
import files.FastaLoadCOSMIC

object AnnotationCosmic {
  /**
   * Annotate a single DNA variant using the COSMIC annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantCosmic(variant: DnaVariant, referenceGenome: String): Unit = {
    if FastaLoadCOSMIC.loadedList.isEmpty || FastaLoadCOSMIC.loadedGenome != referenceGenome then {
      val path = ServiceModules.getNewestModulePath("cosmic", referenceGenome)
      val genome = if referenceGenome == "hg38" then "GRCh38" else "Chm13"
      val version = ServiceModules.getNewestModuleVersion("cosmic")
      FastaLoadCOSMIC.loadFastaFromGzip(s"$path/Cosmic_Genes_v${version}_$genome.fasta.gz", referenceGenome)
    }
    //teraz asi matchnem rovno s genmi vsetkymi v tom FA file
    //potom len nacitam prejdem tie 3 mensie subory a ked najdem tie matchnute tam pridam info 
    
    //potom ked tak este ten dlhy nejak sliding budem musiet urobit pre to
  }
}
