#' Telecharge le genome
#'
#' @param maxOrganism un entier du nombre d'organisme max
#' @param accession.list la liste des différentes especes
#' @param repertoire l'emplacement pour telecharger les fichiers
#'
#' @return il ecrit dans le repertoire
#' @export

downloadgenome <- function(maxOrganism,accession.list,repertoire)
{
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)
  species <- gsub(basename(accession.list),pattern = '.csv',replacement = '')
  n.species <- length(species)
  for(i in 1:n.species)
  {
    dir.create(paste0(repertoire,species[i]))
    accession <- read.csv(accession.list[i],stringsAsFactors = F)
    accession  <- accession$accession
    N.accession <- length(accession)
    print(N.accession)
    for(j in 1:min(c(maxOrganism,N.accession)))
    {
      current.accession <- accession[j]
      current.accession <- strsplit(current.accession,split=':')[[1]]
      current.accession <- gsub(current.accession,pattern = ' ',replacement = '')
      N.chromosomes <- length(current.accession)
      dir.create(paste0(repertoire,gsub(species[i],pattern = ' ',replacement = '.'),'/',current.accession[1]))
      for(k in 1:min(5,N.chromosomes))
      {
        seq <- read.GenBank(current.accession[k])
        write.FASTA(seq,paste0(repertoire,gsub(species[i],pattern = ' ',replacement = '.'),'/',current.accession[1],'/',current.accession[k],'.fasta'))
      }
      seq <- readDNAStringSet(list.files(paste0(repertoire,species[i],'/',current.accession[1]),full.names = T))
      writeXStringSet(seq,paste0(repertoire,gsub(species[i],pattern = ' ',replacement = '.'),'/',paste(current.accession[1:min(5,N.chromosomes)],collapse = ':'),'.fasta'))
    }
  }
}
