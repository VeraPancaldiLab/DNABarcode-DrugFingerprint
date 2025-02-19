### IMPORTANT for 281022 the last cols of the files were removed _75 and P...
## so the file include is exp281022_proc. the original file is in excluded

# Read barcode files ----
fil <- list.files("./", pattern = ".csv")
fil <- setdiff(fil, "exp281022_time course.csv")

el <- list()
for (f in fil) {
  el[[f]] <- read.table(f, header = T, row.names = 1, sep = ";")
}

for (e in el) {
  message(dim(e))
}

# Core analysis and normalisation ----

## Normalisation and DE per replicate with 'create_fil' ----

# elsel=el[-c(2,5,9,13, 14, 15)]
# elsel=el[-c(14)]
elsel <- el

# barcodesel and efc, full matrix
# barcodescombl and comb, mean expression per sample/drug, including selection of drugs
proclist <- barcodesl <- barcodescombl <- list()
for (e in 1:length(elsel)) {
  message(names(elsel)[e])
  proclist[[e]] <- create_fil(elsel[[e]], 3)
  message(dim(proclist[[e]]$efc))
  barcodesl[[e]] <- rownames(proclist[[e]]$efc)
  barcodescombl[[e]] <- rownames(proclist[[e]]$comb)
}

## Merge all barcode counts into a single global matrix, TODO: in one line ----

proclistcomb <- lapply(proclist, function(x) {
  return(x$comb)
})

int <- Reduce(intersect, barcodescombl) # avoid int, reminds integer
# int=Reduce(intersect, barcodesl[which(sapply(barcodesl, length)>0)])

newma <- proclistcomb[[1]][int, ]
for (e in 1:length(elsel)) {
  message(dim(newma))
  newma <- merge(newma, proclistcomb[[e]][int, ], by.x = 0, by.y = 0, all.x = T, all.y = T)
  rownames(newma) <- newma[, 1]
  newma <- newma[, -1]
}
# make a full merge to subsequently remove rows with missing values, really???? :-)
newmacomp <- newma[complete.cases(newma), ]

## Make a graph of correlations ----

library(igraph)
corcond <- cor(newma) # correlation matrix -> prefer partial correlation if relevant.
Gcond <- graph_from_adjacency_matrix(corcond, weighted = T)
write.table(cbind(get_edgelist(Gcond), E(Gcond)$weight), "Gcond.txt", quote = F, sep = "\t")

# names(barcodesl)=names(elsel)
# int=Reduce(intersect, barcodesl[which(sapply(barcodesl, length)>0)])
#  length(int)
# names(barcodesl)=names(el)
# barcodescomm=Reduce(intersect, barcodesl)


# Core function create_fil ----

## Arguments: raw barcode counts and threshold
create_fil <- function(data, thresh) {
  ## Remove lowly expressed barcodes ----
  barsel <- which(rowSums(data[, grep("ct|Cont|ctl", colnames(data))]) > thresh)
  ## Normalisation ----
  for (c in 1:ncol(data)) {
    data[barsel, c] <- data[barsel, c] / sum(data[barsel, c]) * 100000
  }

  ## Average control values ----
  cmean <- rowMeans(data[barsel, c(1:4)])

  ## Average treatment effect, filter on barcodes with expression > 3 ----
  if (length(barsel) > 0) {
    message(paste0("length barcode sel ", length(barsel)))
    efc <- data[barsel, -c(1:4)]
    efcfil <- apply(efc, 2, function(x) {
      res <- as.numeric(x / cmean > 3)
      return(res)
    })

    rownames(efcfil) <- rownames(efc)

    if (length(grep("_", colnames(data))) > 0) { ### if info on conc is given
      message("info on conc given")
      datacond <- data[, grep("_", colnames(data))]


      conds <- colnames(datacond)
      condtable <- sapply(conds, function(x) {
        return(unlist(strsplit(x, split = "_")))
      })
      # message(colnames(datacond))
      rownames(condtable) <- c("drug", "conc", "exp", "run", "num")
      condtabledf <- data.frame(condtable)


      cond <- unique(paste(condtable["drug", ], condtable["conc", ], sep = "_"))
      cond <- cond[grep("Contro|ct|ctl", cond, invert = T)]
      conc <- sapply(cond, function(x) {
        return(unlist(strsplit(x, split = "_"))[2])
      })
      drugs <- as.vector(sapply(cond, function(x) {
        x <- unlist(strsplit(x, split = "_"))[1]
        return(substring(x, 0, nchar(x) - 1))
      }))
      message(nrow(efcfil))
      message("before newcond")
      newcond <- unique(paste(drugs, conc, sep = "_"))
      # Comb will contain a new matrix with only 1 or 0s
      comb <- matrix(0, ncol = length(newcond), nrow = nrow(efc))
      colnames(comb) <- newcond
      rownames(comb) <- rownames(efc)

      # make new conditions to average replicates
      for (nc in newcond) {
        message(nc)

        d <- as.vector(sapply(nc, function(x) {
          return(unlist(strsplit(x, split = "_"))[1])
        }))
        c <- as.vector(sapply(nc, function(x) {
          return(unlist(strsplit(x, split = "_"))[2])
        }))

        newsel <- intersect(grep(d, colnames(efc)), grep(c, colnames(efc)))
        comb[, nc] <- rowSums(efc[, newsel])
      } # end of nc loop

      # message(colnames(comb))
    } ######### end if  conc given


    if (length(grep("_", colnames(data))) == 0) { ## if there is no conc info

      drugs <- as.vector(sapply(colnames(data), function(x) {
        return(substring(x, 0, nchar(x) - 1))
      }))
      drugs <- setdiff(unique(drugs), c("Contro", "ct", "ctl"))

      comb <- matrix(0, ncol = length(drugs), nrow = nrow(efc))
      colnames(comb) <- drugs
      rownames(comb) <- rownames(efc)

      for (d in drugs) {
        comb[, d] <- rowSums(efc[, grep(d, colnames(efc))])
      }

      # message(colnames(comb))
    } ## end of if there is no conc info

    message("complete cases of comb")
    message(length(complete.cases(comb)))

    ## Keep only barcodes that have at least 1 cond with non zero value ----
    comb <- comb[which(rowSums(comb) > 0), ]
    retlist <- list(efc, comb)
    names(retlist) <- c("efc", "comb")

    return(retlist)
  } # end if there are some barcodes

  if (length(barcodesl) == 0) {
    return("NA")
    message("there are no barcodes")
  }
} # end of function


# Spurious or old functions ----

# barfcfil<-function(x){
# efc=x
#
#
# efcfil=apply(efc, 2, function(x){
# res=as.numeric(x/cmean[barcodesel]>3)
# return(res)
# })
#
# rownames(efcfil)=rownames(efc)
#
# drugs=(condtable['drug',])
# drugs=as.vector(sapply(drugs,function(x){
# return(substring(x,0,nchar(x)-1))
# }))
# drugs=setdiff(unique(drugs), c('Contro', 'ct'))
#
#
# comb=matrix(0,ncol=length(drugs), nrow=nrow(efcfil))
# colnames(comb)=drugs
# rownames(comb)=rownames(efcfil)
#
# for (d in drugs){
# comb[,d]=rowSums(efcfil[,grep(d, colnames(efcfil))])
# }
#
# comb=comb[which(rowSums(comb)>0),]
# return(comb)
# }
# if (length(barcodesl)==0){
# return('NA')
# }
# message('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
# }# end of function
