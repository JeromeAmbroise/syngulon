#' extract a table based on the genome package
#'
#' to compute later
#'
#' @param phylum a data.frame produced by the function extract.phylum
#' @return a table with the one line for each genome available
#' @export
extract.bacteria.table <- function(phylum)
{
  library(genomes)
  library(dplyr)
  proks <- reports("prokaryotes.txt")
  bacteria.table <- proks%>%filter(Status=='Complete Genome'|Status=='Chromosome')
  bacteria.table <- bacteria.table[grep(paste(phylum$taxonname,collapse='|'),bacteria.table$SubGroup,ignore.case = T),]
  bacteria.table$Organism <- unlist(lapply(strsplit(bacteria.table$Organism,split=' '),function(x) paste(x[1],x[2],sep=' ')))
  bacteria.table$Organism <- gsub(bacteria.table$Organism,pattern='\\[',replacement = '')
  bacteria.table$Organism <- gsub(bacteria.table$Organism,pattern='\\]',replacement = '.')
  bacteria.table$Organism <- gsub(bacteria.table$Organism,pattern='\'',replacement = '')
  bacteria.table$Organism <- bacteria.table$Organism %>% gsub(' ','_',.)
  return(bacteria.table)
}


