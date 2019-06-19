load("/Lacie_RRW10023/DATASETS/STUDY/GEO/GSE40576/AsthmaInnerCity.Rdata")

library(minfi)
library(methassoc)

numsamples <- ncol(esetM)

  phenoNames <- grep("characteristics", varLabels( esetM ) )
    phenoData(esetM) <- phenoData(esetM)[, phenoNames]
  p <- sapply(colnames(pData(esetM)), function(x) as.character(pData(esetM)[,x]))
phenoList <- strsplit(p, ": ")
  phenoVars <- unlist(lapply(phenoList, function(x) x[2]))
p <- data.frame(matrix( phenoVars, nrow = numsamples))
  rownames(p) <- sampleNames(esetM )
  colnames(p) <- c(phenoList[[1]][1], phenoList[[(1+numsamples)]][1], 
phenoList[[(1 + 2*numsamples)]][1])

p[,1] <- as.logical(p[,1])
p[,1] <- ifelse(p[,1], "asthma", "control")
p[,1] <- as.factor(p[,1])
p[,2] <- as.factor(p[,2])
p[,3] <- as.factor(p[,3])
pData(esetM) <- p

ms <-  RatioSet(M = exprs(esetM),annotation=c(array= "IlluminaHumanMethylation450k",  annotation = "ilmn12.hg19"))
ms <- dropMethylationLoci(ms)
ms <- mapToGenome(ms)
ms <- dropLociWithSnps(ms, snps=c("Probe", "CpG", "SBE"))

methylation_betas_from_project_X_20190423 =getBeta(ms)
pheno= pData(esetM)
annot= fData(esetM)
annot$ CHR= paste0("chr", annot$CHR)
annot$MAPINFO =as.numeric( annot$MAPINFO)
fData(esetM)= annot

save(methylation_betas_from_project_X_20190423, pheno, annot, file = "methRaw.RData")
