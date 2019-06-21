#'################################################
#'##### PROJECT X DATA PREPARATION          ######
#' Prepare data for analysis                ######
#'################################################

#'#####################################
#'### Author: X 			   ###########
#'### Starting date: 24th May 2019 ####
#'### RStudio version: 1.0.143 ########
#'### Script version: 6.0 (clean) #####
#'#####################################

## Load libraries ####
library(minfi)
library(methassoc)

## Load data ####
load("./data/AsthmaInnerCity.Rdata")

numsamples <- ncol(esetM)

# Prepare phenotypes ####
phenoNames <- grep("characteristics", varLabels(esetM))
phenoData(esetM) <- phenoData(esetM)[, phenoNames]
phenoFrame <- sapply(colnames(pData(esetM)), 
                     function(x) as.character(pData(esetM)[, x]))
phenoList <- strsplit(phenoFrame, ": ")
phenoVars <- unlist(lapply(phenoList, function(x) x[2]))
phenoData <- data.frame(matrix(phenoVars, nrow = numsamples))
rownames(phenoFrame) <- sampleNames(esetM)
colnames(phenoFrame) <- c(phenoList[[1]][1], phenoList[[(1 + numsamples)]][1], 
                          phenoList[[(1 + 2*numsamples)]][1])

phenoFrame[, 1] <- as.logical(phenoFrame[, 1])
phenoFrame[, 1] <- ifelse(phenoFrame[, 1], "asthma", "control")
phenoFrame[, 1] <- as.factor(phenoFrame[, 1])
phenoFrame[, 2] <- as.factor(phenoFrame[, 2])
phenoFrame[, 3] <- as.factor(phenoFrame[, 3])
pData(esetM) <- p

# Prepare betas ####
minfiset <-  RatioSet(M = exprs(esetM), 
                      annotation = c(array = "IlluminaHumanMethylation450k",
                                     annotation = "ilmn12.hg19"))
minfiset <- dropMethylationLoci(ms)
minfiset <- mapToGenome(minfiset)
minfiset <- dropLociWithSnps(minfiset, snps = c("Probe", "CpG", "SBE"))

# Prepare final objects ####
betas <- getBeta(minfiset)
pheno <- pData(esetM)
annot <- fData(esetM)
annot$CHR <- paste0("chr", annot$CHR)
annot$MAPINFO <- as.numeric(annot$MAPINFO)
fData(esetM) <- annot

save(betas, pheno, annot, file = "results/preproc/methRaw.RData")
