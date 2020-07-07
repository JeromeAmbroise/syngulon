#' extract a table based on the genome package
#'
#' to compute later
#'
#' @param phylum a data.frame produced by the function extract.phylum
#' @return a table with the one line for each genome available
#' @export


#' Title
#'
#' @param phylum
#'
#' @return
#' @export
#'
#' @examples
extract.bacteria.table <- function(phylum)
{
  library(genomes)
  library(dplyr)
  proks <- reports("prokaryotes.txt")
  proks.selected <- proks%>%filter(Status=='Complete Genome'|Status=='Chromosome')
  proks.selected <- proks.selected[grep(paste(phylum$taxonname,collapse='|'),proks.selected$SubGroup,ignore.case = T),]
  proks.selected$Organism <- unlist(lapply(strsplit(proks.selected$Organism,split=' '),function(x) paste(x[1],x[2],sep=' ')))
  proks.selected$Organism <- gsub(proks.selected$Organism,pattern='\\[',replacement = '')
  proks.selected$Organism <- gsub(proks.selected$Organism,pattern='\\]',replacement = '.')
  proks.selected$Organism <- gsub(proks.selected$Organism,pattern='\'',replacement = '')
  proks.selected$Organism <- proks.selected$Organism %>% gsub(' ','_',.)
  return(proks.selected)
}


