package cosmic.dataCosmic

case class ResistanceMutation(
                               cosmicGeneId: String,
                               transcriptAccession: String,
                               censusGene: String,
                               drugName: String,
                               drugResponse: String,
                               chromosome: String,
                               genomeStart: Long,
                               genomeStop: Long,
                               strand: String,
                               mutationZygosity: String
                             )

