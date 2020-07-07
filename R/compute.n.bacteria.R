
#' Renvoie une data frame avec le nombre de même organism
#'
#' @param phylum data frame
#' @param bacteria.table Table de bactérie
#'
#' @return Renvoie une data frame avec le nombre de même organism
#' @export
compute.n.bacteria <- function(phylum,bacteria.table)
{
  library(dplyr)
  bacteria.n <- data.frame(bacteria.table%>%group_by(Organism)%>%summarise(n=n()))
  return(bacteria.n)
}
