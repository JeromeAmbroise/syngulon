
compute.n.bacteria <- function(phylum,bacteria.table)
{
  library(dplyr)
  bacteria.n <- data.frame(bacteria.table%>%group_by(Organism)%>%summarise(n=n()))
  return(bacteria.n)
}
