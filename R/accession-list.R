#' Title
#'
#' @param bacteria.table
#' @param species
#' @param repertoire
#'
#' @return
#' @export
#'
#' @examples
accessionlist <- function(bacteria.table,species,repertoire)
{
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)
  bacteria.table$Replicons <- unlist(lapply(strsplit(bacteria.table$Replicons,split='plasmid'),function(x) x[1]))
  n.species <- length(species)
  for(i in 1:n.species)
  {
    data1species <- bacteria.table[bacteria.table$Organism==species[i],]
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
