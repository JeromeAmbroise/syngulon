#' Title
#'
#' @param proks.selected
#' @param species
#' @param repertoire
#'
#' @return
#' @export
#'
#' @examples
accession-list <- function(proks.selected,species,repertoire)
{
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)
  proks.selected$Replicons <- unlist(lapply(strsplit(proks.selected$Replicons,split='plasmid'),function(x) x[1]))
  n.species <- length(species)

  for(i in 1:n.species)
  {
    data1species <- proks.selected[proks.selected$Organism==species[i],]
    data1species <- data1species[is.na(data1species$Replicons)==F,]
    Nstrains <- dim(data1species)[1]
    Replicon.vec <- NULL

    for(j in 1:Nstrains)
    {
      Replicon <- data1species$Replicons[j]
      Replicon <- strsplit(Replicon,split=';')[[1]]
      Replicon <- Replicon[grep(':',Replicon)]
      Replicon <- unlist(lapply(strsplit(Replicon,split=':'), function(x) x[[2]]))
      Replicon <- unlist(lapply(strsplit(Replicon,split='/'), function(x) x[[1]]))
      Replicon <- gsub(Replicon,pattern = ' ',replacement = '')
      Replicon <- paste(Replicon,collapse = " : ")
      Replicon.vec <- c(Replicon.vec,Replicon)
    }
    accession <- data.frame(accession=Replicon.vec)
    write.csv(accession,paste0(repertoire,species[i],'.csv'),row.names = F)
  }
}
