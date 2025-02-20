### IMPORTANT for 281022 the last cols of the files were removed _75 and P...
## so the file include is exp281022_proc. the original file is in excluded

source("R/process_barcode.R")

# Read barcode files ----
fil <- setdiff(list.files("./data/data-raw/barcode-counts", pattern = ".csv"),
               "exp281022_time course.csv")[1:4]

elsel <- list()
for (f in fil[1:2]) {
  elsel[[f]] <- read.table(file.path("data", "data-raw", "barcode-counts", f),
                           header = T, row.names = 1, sep = ";")
}

# Core analysis and normalisation ----

## Normalisation and DE per replicate with 'create_fil' ----

# barcodesel and efc, full matrix
# barcodescombl and comb, averaged expression per sample/drug
proclist <- barcodescombl <- list()
for (e in 1:length(elsel)) {
  message(names(elsel)[e])
  proclist[[e]] <- create_fil(elsel[[e]], 3)
  message(dim(proclist[[e]]$efc))
  barcodescombl[[e]] <- rownames(proclist[[e]]$comb)
}

## Merge all barcode counts into a single global matrix ----

proclistcomb <- lapply(proclist, function(x) {
  return(x$comb)
})

int <- Reduce(intersect, barcodescombl) # avoid int, reminds integer

newma <- proclistcomb[[1]][int, ]
for (e in 1:length(elsel)) {
  message(dim(newma))
  newma <- merge(newma, proclistcomb[[e]][int, ], by.x = 0, by.y = 0, all.x = T, all.y = T)
  rownames(newma) <- newma[, 1]
  newma <- newma[, -1]
}
# make a full merge to subsequently remove rows with missing values, really???? :-)
# newmacomp <- newma[complete.cases(newma), ]




## Make a graph of correlations ----

library(igraph)
corcond <- cor(newma) # correlation matrix
Gcond <- graph_from_adjacency_matrix(corcond, weighted = T)
write.table(cbind(get_edgelist(Gcond), E(Gcond)$weight), "Gcond.txt", quote = F, sep = "\t")



# names(barcodesl)=names(elsel)
# int=Reduce(intersect, barcodesl[which(sapply(barcodesl, length)>0)])
#  length(int)
# names(barcodesl)=names(el)
# barcodescomm=Reduce(intersect, barcodesl)



