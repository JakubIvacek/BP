package cosmic.dataCosmic

case class FaEntryCosmic(
                          geneSymbol: String,
                          transcriptAccession: String,
                          chromosome: String,
                          genomeStart: Int,
                          genomeStop: Int,
                          strand: String,
                          sequence: String)
