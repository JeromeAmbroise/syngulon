writteextract1gene <- function(accession.list,collicin,repertoire__gene_annotation)
{
  species <- gsub(basename(accession.list),pattern = '.csv',replacement = '')
  nspecies <- length(species)
  collicin <- collicin$genename
  ngenes <- length(collicin)


  for(i in 1:nspecies)
  {
    dir.create(paste0(repertoire__gene_annotation,species[i]))
    for(j in 1:ngenes)
    {
      extract1gene(selectedspecies=species[i],selectedgene=collicin[j])
    }
  }

  fasta.list.ecoli <- list.files(paste0(repertoire__gene_annotation,species),full.names = T)
  fasta.list.ecoli <- fasta.list.ecoli[grep('fasta',fasta.list.ecoli)]
  fasta.list.cholerae <- list.files('05-genes-annotation-based/Vibrio_cholerae/',full.names = T)
  fasta.list.cholerae <- fasta.list.cholerae[grep('fasta',fasta.list.cholerae)]
  fasta.list <- c(fasta.list.ecoli,fasta.list.cholerae)

  genename <- basename(fasta.list.ecoli)
  for(i in 1:length(genename))
  {
    current.sequence <- readDNAStringSet(fasta.list[grep(genename[i],fasta.list)])
    writeXStringSet(current.sequence,paste0('05-genes-annotation-based/',genename[i]))
  }
}
