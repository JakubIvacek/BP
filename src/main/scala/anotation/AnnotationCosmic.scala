package anotation

import data.{DnaVariant, FaEntryCosmic, GeneCensusCosmic, GffEntry, ResMutationCosmic}
import database.modules.ServiceModules
import files.{FastaLoadCOSMIC, GFFReader}

object AnnotationCosmic {
  var geneCensusEntries: Option[List[GffEntry]] = None
  var resistanceMutationsEntries: Option[List[GffEntry]] = None
  var path: Option[String] = None
  var reference: Option[String] = None
  var version: Option[String] = None
  /**
   * Annotate a single DNA variant using the COSMIC annotation information
   *
   * @param variant         The DNA variant to annotate.
   * @param referenceGenome The reference genome to use for annotation.
   */
  def annotateVariantCosmic(variant: DnaVariant, referenceGenome: String): Unit = {
    // Reload annotation data if none loaded or new reference Genome
    if FastaLoadCOSMIC.getLoadedList.isEmpty || FastaLoadCOSMIC.loadedGenome != referenceGenome then {

      path = ServiceModules.getNewestModulePath("cosmic", referenceGenome)
      reference = if referenceGenome == "hg38" then Some("GRCh38") else Some("Chm13")
      version = Some(ServiceModules.getNewestModuleVersion("cosmic"))
      val fPath = path.getOrElse("")
      val fReference = reference.getOrElse("")
      val fVersion = version.getOrElse("")
      FastaLoadCOSMIC.loadFastaFromGzip(s"$fPath/Cosmic_Genes_v${fVersion}_$fReference.fasta.gz", referenceGenome)
      geneCensusEntries = Some(GFFReader.loadGffFileReturnList(s"$fPath/Cosmic_CancerGeneCensus_v${fVersion}_$fReference.gff"))
      resistanceMutationsEntries = Some(GFFReader.loadGffFileReturnList(s"$fPath/Cosmic_ResistanceMutations_v${fVersion}_$fReference.gff"))
    }
    //GET MATCHING COSMIC FA ENTRIES
    val matchingEntriesFa = FastaLoadCOSMIC.getLoadedList.filter(entry =>
      entry.chromosome == variant.contig.stripPrefix("chr") &&
        entry.genomeStart < variant.position.toInt &&
        entry.genomeStop > variant.positionEnd.toInt &&
        entry.getSequencePart(variant.position.toInt, variant.positionEnd.toInt) == variant.refAllele

    )
    val matchedGenes: Set[String] = matchingEntriesFa.map(_.geneSymbol).toSet
    val matchedTranscripts: Set[String] = matchingEntriesFa.map(_.transcriptAccession).toSet

    //CHECK IF GeneCensus matching with FA entries
    val geneCensusMatching: List[GffEntry] = geneCensusEntries.getOrElse(Nil).filter { entry =>
      entry.attributes.get("ID").exists(matchedGenes.contains) // Boolean
    }

    //CHECK IF ResistanceMutation matching with FA entries
    val resMutationsMatching: List[GffEntry] = resistanceMutationsEntries.getOrElse(Nil).filter { entry =>
      entry.attributes.get("TRANSCRIPT_ACCESSION").exists(matchedTranscripts.contains) // Boolean
    }

    // annotate with the first gene‐census hit
    geneCensusMatching.headOption.foreach { hit =>
      val attrs = hit.attributes
      variant.geneID_COSMIC = attrs.getOrElse("ID", ".")
      variant.cancerSyndrome_COSMIC = attrs.getOrElse("CANCER_SYNDROME", ".")
      variant.tissueType_COSMIC = attrs.getOrElse("TISSUE_TYPE", ".")
      variant.molecularGenetics_COSMIC = attrs.getOrElse("MOLECULAR_GENETICS", ".")
      variant.roleInCancer_COSMIC = attrs.getOrElse("ROLE_IN_CANCER", ".")
      variant.mutationTypes_COSMIC = attrs.getOrElse("MUTATION_TYPES", ".")
    }

    // annotate with the first resistance-mutation hit
    resMutationsMatching.headOption.foreach { hit =>
      val attrs = hit.attributes
      variant.transcriptID_COSMIC = attrs.getOrElse("TRANSCRIPT_ACCESSION", ".")
      variant.censusGene_COSMIC = attrs.getOrElse("CENSUS_GENE", ".")
      variant.drugName_COSMIC = attrs.getOrElse("DRUG_NAME", ".")
      variant.mutZygosity_COSMIC = attrs.getOrElse("MUTATION_ZYGOSITY", ".")
    }
  }
}
