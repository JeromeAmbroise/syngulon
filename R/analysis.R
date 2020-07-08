library(syngulon)
library(dplyr)

phylum <- extract.phylum()
write.csv(phylum,'01-selected.species/phylum.table.csv')

bacteria.table <- extract.bacteria.table(phylum = phylum)
write.csv(bacteria.table,'01-selected.species/bacteria.table.csv',row.names = F)


########## toutes les bactéries

bacteria.n <- compute.n.bacteria(bacteria.table= bacteria.table)
bacteria.n <- bacteria.n%>%arrange(desc(n))
write.csv(bacteria.n,'01-selected.species/bacteria.n.csv',row.names = F)

########## uniquement les bactéries avec n>10


bacteria.n <- read.csv('01-selected.species/bacteria.n.csv')
bacteria.n.selected <- bacteria.n%>%filter(n>=5)
bacteria.n.selected <- bacteria.n.selected%>%arrange(desc(n))
write.csv(bacteria.n.selected,'01-selected.species/bacteria.n.selected.csv',row.names = F)



##########

phylum <- read.csv('01-selected.species/phylum.table.csv')
bacteria.table <- read.csv('01-selected.species/bacteria.table.csv')
bacteria.n.selected <- read.csv('01-selected.species/bacteria.n.selected.csv')

species <- bacteria.n.selected$Organism

accessionlist(proks.selected = bacteria.table,species =species,repertoire = '02-accession-list/'  )
accession.list <- list.files('02-accession-list/',full.names = T)

downloadannotation(maxOrganism = 20,accession.list = accession.list,repertoire = '03-annotation/' )
downloadgenome(maxOrganism = 20,accession.list = accession.list,repertoire = '04-genomes/')
