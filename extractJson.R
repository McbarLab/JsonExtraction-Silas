# This analysis is coded by Di "Silas" Kuang
# Email: dkuang5@wisc.edu
# RStudio version: 2023.03.0 Build 386
# R version: 4.3.0
# All rights reserved unless the author is dead ORZ

if (!require("tidyverse", quietly = TRUE))
  install.packages("tidyverse")
if (!require("rjson", quietly = TRUE))
  install.packages("rjson")

library(rjson)
library(data.table)
library(gdata)
library(tidyverse)

# Parse the JSON string into a list
json_data <-
  rjson::fromJSON(file = "20230518_nuc_pre_ct1_Cloud.json")

# Number of groups in the dataset
json_length <- length(json_data)
trait_list <- c("RadiusMajor", "RadiusMinor", "Size", "Circularity")

# cellState == 1, live; cellState == 2, dead
traitExtract <- function(cellState) {
  if (cellState != 1 & cellState != 2) {
    stop("Cell state should be 1 or 2")
  }
  else{
    cellCount <- length(json_data[[cellState]]$Cells)
    # Create empty table for data storage
    table <- as.data.frame(matrix(nrow = length(json_data[[cellState]]$Cells), ncol = 4))
    colnames(table) <- trait_list
    for (i in 1:cellCount) {
      for (j in 1:length(trait_list)) {
        table[i, j] <- json_data[[cellState]]$Cells[[i]][trait_list[j]]
      }
    }
    if (cellState == 1) {
      live_cells <- assign("live_cells", table, envir = parent.frame())
      return(live_cells)
    } else{
      dead_cells <- assign("dead_cells", table, envir = parent.frame())
      return(dead_cells)
    }
  }
}

for(k in 1:2){
  traitExtract(k)
}