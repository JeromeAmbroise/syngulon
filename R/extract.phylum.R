




extract.phylum <- function()
{
  library(taxize)
  library(dplyr)
  phylum <- itis_downstream(id = 50, downto="phylum")
  phylum <- tibble(phylum)
  phylum <- phylum %>% select(tsn,taxonname)
  return(phylum)
}


