package cosmic.data

case class GeneCensusVariant(
                              cosmicGeneId: String,
                              chromosome: String,
                              genomeStart: Long,
                              genomeStop: Long,
                              tumourTypesSomatic: String,
                              tumourTypesGermline: String,
                              cancerSyndrome: String,
                              tissueType: String,
                              molecularGenetics: String,
                              roleInCancer: String,
                              mutationTypes: String,
                            )
