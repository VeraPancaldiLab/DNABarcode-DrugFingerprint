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

  ## Average treatment effect, filter on barcodes with expression > 3 but not used!!
  if (length(barsel) > 0) {
    message(paste0("length barcode sel ", length(barsel)))
    efc <- data[barsel, -c(1:4)]


    # efcfil <- apply(efc, 2, function(x) {
    #   res <- as.numeric(x / cmean > 3)
    #   return(res)
    # })
    # rownames(efcfil) <- rownames(efc)

    if (length(grep("_", colnames(data))) > 0) { ### if info on conc is given -> does not work
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

      newcond <- unique(paste(drugs, conc, sep = "_"))
      comb <- matrix(0, ncol = length(newcond), nrow = nrow(efc))
      colnames(comb) <- newcond
      rownames(comb) <- rownames(efc)

      # make new conditions to average replicates
      for (nc in newcond) {

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
    } ## end of if there is no conc info


    ## Keep only barcodes that have at least 1 cond with non zero value ----
    comb <- comb[which(rowSums(comb) > 0), ]
    retlist <- list(efc, comb)
    names(retlist) <- c("efc", "comb")

    return(retlist)
  } # end if there are some barcodes
} # end of function
