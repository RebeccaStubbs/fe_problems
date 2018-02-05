
rm(list=ls()) # Clear working environment
setwd("/Users/stubbsrw/Documents/fe_problems") # Set working directory

library("MapSuite") #Self-written library, has many common libs as dependencies

# Duwamish Waterway Data (Dataset B)

# Load in data, which is a spatialpointsdataframe
  chem_sp<-readRDS("/Users/stubbsrw/Documents/fe_problems/data/Dataset_B.RDS")

# Creating an index-- since the data is only linked to the spatial information via order, we need to make sure it's preserved.
  chem_data<-data.table(chem_sp@data)
  chem_data[,sp_index:=seq(1:nrow(chem_data))]

