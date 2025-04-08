package cosmic.dataCosmic

case class NonCodingVariant(
                             cosmicGeneId: String,
                             transcriptAccession: String,
                             cosmicPhenotypeId: String,
                             genomicMutationId: String,
                             legacyMutationId: String,
                             zygosity: String,
                             chromosome: String,
                             genomeStart: Long,
                             genomeStop: Long,
                             genomicWtAllele: String,
                             genomicMutAllele: String,
                             mutationSomaticStatus: String
                           )
