
compute.n.bacteria <- function()
{
  library(taxize)
  library(genomes)
  library(reutils)
  library(ape)
  library(seqinr)
  library(Biostrings)
  library(dplyr)
  library(WriteXLS)

  phylum <- itis_downstream(id = 50, downto="phylum")
  phylum <- tibble(phylum)
  phylum <- phylum %>% select(tsn,taxonname)
  proks <- reports("prokaryotes.txt")
  proks.selected <- proks%>%filter(Status=='Complete Genome'|Status=='Chromosome')
  proks.selected <- proks.selected[grep(paste(phylum$taxonname,collapse='|'),proks.selected$SubGroup,ignore.case = T),]
  table(proks.selected$SubGroup)
  proks.selected$Organism <- unlist(lapply(strsplit(proks.selected$Organism,split=' '),function(x) paste(x[1],x[2],sep=' ')))
  proks.selected$Organism <- gsub(proks.selected$Organism,pattern='\\[',replacement = '')
  proks.selected$Organism <- gsub(proks.selected$Organism,pattern='\\]',replacement = '.')
  proks.selected$Organism <- gsub(proks.selected$Organism,pattern='\'',replacement = '')
  proks.selected$Organism <- proks.selected$Organism %>% gsub(' ','_',.)
  bacteria.n <- data.frame(proks.selected%>%group_by(Organism)%>%summarise(n=n()))
  return(bacteria.n)
}



extract.phylum <- function()
{
  library(taxize)
  library(dplyr)
  phylum <- itis_downstream(id = 50, downto="phylum")
  phylum <- tibble(phylum)
  phylum <- phylum %>% select(tsn,taxonname)
  return(phylum)
}


extract.bacteria.table <- function()
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
