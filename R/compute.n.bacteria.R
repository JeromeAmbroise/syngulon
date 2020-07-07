#' Compute the number of genome available for each bacteria
#'
#' to compute later
#'
#' @param bacteria.table a data.frame produce by the function extract.bacteria.table
#' @return a table with the number of available genomes for each bacteria
#' @export

compute.n.bacteria <- function(bacteria.table)
{
  library(dplyr)
  bacteria.n <- data.frame(bacteria.table%>%group_by(Organism)%>%summarise(n=n()))
  return(bacteria.n)
}
