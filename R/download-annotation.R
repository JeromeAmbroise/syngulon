#' Title
#'
#' @param maxOrganism
#' @param accession.list
#' @param repertoire
#'
#' @return
#' @export
#'
#' @examples
download-annotation <- function(maxOrganism=20,accession.list,repertoire )
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
    accession <- accession[substr(accession,1,2)!='LR']
    N.accession <- length(accession)
    for(j in 1:min(c(maxOrganism,N.accession)))
    {
      current.accession <- accession[j]
      current.accession <- strsplit(current.accession,split=':')[[1]]
      current.accession <- gsub(current.accession,pattern = ' ',replacement = '')
      N.chromosomes <- length(current.accession)
      full.annotation <- NULL
      for(k in 1:N.chromosomes)
      {
        current.annotation <- try(getAnnotationsGenBank(access.nb=current.accession[k], quiet = TRUE))
        if(class(current.annotation)=='data.frame')
        {
          if(all(is.element(c('start','end','type','gene','product'),colnames(current.annotation))))
          {
            current.annotation <- tibble(current.annotation)
            current.annotation <- current.annotation %>% select(start,end,type,gene,product)
            current.annotation <- data.frame(genome=current.accession[k],current.annotation)
            full.annotation <- rbind(full.annotation,current.annotation)
          }
        }
      }
      if(class(full.annotation)=='data.frame')
      {
        write.csv(full.annotation,paste0(repertoire,species[i],'/',paste(current.accession[1:min(5,N.chromosomes)],collapse = ':'),'.csv'),row.names = F)
      }
    }
  }
}
