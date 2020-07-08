exctratblast <- function()
{

  genometoscreen <- c(list.files('04-genomes/Escherichia_coli/',recursive = F,full.names = T),list.files('04-genomes/Vibrio_cholerae/',recursive = F,full.names = T))
  genometoscreen <- genometoscreen[grep('fasta',genometoscreen)]

  genetosreen <- list.files('05-genes-annotation-based/',full.names = T)
  genetosreen <- genetosreen[grep('fasta',genetosreen)]

  ngenes <- length(genetosreen)
  ngenomes <- length(genometoscreen)
  for(i in 1:ngenes)
  {
    currentgene <- genetosreen[i]
    all.sequences <- DNAStringSet()
    for(j in 1:ngenomes)
    {
      sequence <- extract.closest(genomePath = genometoscreen[j],genepath =genetosreen[i],lengthconf = 90,identconf=80,offset = 0  )
      names(sequence) <- paste(strsplit(genometoscreen[j],split='/')[[1]][2],names(sequence),sep='_')
      all.sequences <- DNAStringSet(c(all.sequences,sequence))
      print(j)
    }
    writeXStringSet(all.sequences,paste0('06-genes-blast-based/',basename(currentgene)))
  }
}
