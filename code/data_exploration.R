
rm(list=ls()) # Clear working environment
setwd("/Users/stubbsrw/Documents/fe_problems") # Set working directory

library("MapSuite") #Self-written library, has many common libs as dependencies

# Duwamish Waterway Data (Dataset B)

# Load in data, which is a spatialpointsdataframe
  chem_sp<-readRDS("/Users/stubbsrw/Documents/fe_problems/data/Dataset_B.RDS")

# Creating an index-- since the data is only linked to the spatial information via order, we need to make sure it's preserved.
  chem_data<-data.table(chem_sp@data)
  chem_data[,sp_index:=seq(1:nrow(chem_data))]

# What kind of samples are there in this data set?
  unique(chem_data$ChemicalGroup)
#OK, so, for now, let's restrict this to only the chemicals that we are intersted in--
# Arsenic, PAHs, and PCBs
  chem_data<-chem_data[(Chemical %in% c("Total PCBs","Arsenic"))|(ChemicalGroup=="PAHs")]
  
# Some basic cleanup: I only want to know about samples that have a valid entry for "Detected".
  chem_data<-chem_data[!is.na(Detected)]
  
# What year was each sample taken?
  chem_data[,Year:=as.numeric(substr(as.character(chem_data$SampleDate),1,4))]
# If it was dredged, how many years ago was it dredged?
  chem_data[DredgeYear=="2003/2004",DredgeYear:="2004"] #Assign ambiguous year ("2003/2004") later year, since we can't assume more time ahs passed than actually has between dredging and sampling.
  chem_data[,DredgeYear:=as.numeric(DredgeYear)]
  chem_data[,dredge_gap:=Year-DredgeYear]
  
# Let's try a log-transform for the value variables 
    chem_data[,log_value:=log(ValueOrHalfQL)]
  
#What do these points look like?
  MapSuite::PointMap(coords=chem_data,id="sp_index",xcol="X",ycol="Y",variable="Year",map_title="Samples by Year")

# Let's see if we can find the dredged navigation channel/explore the dredging history
  MapSuite::PointMap(coords=chem_data[!is.na(DredgeYear)],id="sp_index",xcol="X",ycol="Y",variable="DredgeYear",
                     map_title="Sample Location Dredging History")
  
# What about different chemical levels-- where were each of the chemicals detected, and not?
  
  ## Arsenic
    MapSuite::PointMap(coords=chem_data[Chemical=="Arsenic",],id="sp_index",xcol="X",ycol="Y",
                       variable="Detected",map_title="Arsenic Detection")
    MapSuite::PointMap(coords=chem_data[Chemical=="Arsenic",],id="sp_index",xcol="X",ycol="Y",
                       variable="ValueOrHalfQL",map_title="Arsenic Levels")
    MapSuite::PointMap(coords=chem_data[ChemicalGroup=="VOCs",],id="sp_index",xcol="X",ycol="Y",
                       variable="log_value",map_title="Log-transformed VOC Levels") 
  
  ## PCBs
  MapSuite::PointMap(coords=chem_data[ChemicalGroup=="PCBs",],id="sp_index",xcol="X",ycol="Y",
                     variable="log_value",map_title="Log-transformed PCB Levels",series_dimension = "Year")
  
  ## Arsenic
  MapSuite::PointMap(coords=chem_data[ChemicalGroup=="Arsenic",],id="sp_index",xcol="X",ycol="Y",
                     variable="log_value",map_title="Log-transformed Arsenic Levels")
  
  
# When were these samples taken?

#We are interested in PCBS, Arsenic, and PAH--so, let's plot those:
  
