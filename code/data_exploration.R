
rm(list=ls()) # Clear working environment
library("MapSuite") #Self-written library, has many common libs as dependencies

# Duwamish Waterway Data (Dataset B)
# Worth noting here that the Duwamish effectively runs south, to north: Upstream is south, towards the harbor is north.

# Load in data, which is a spatialpointsdataframe
  chem_sp<-readRDS("/Users/stubbsrw/Documents/git_code/stubbs_repo/fe_problems/data/Dataset_B.RDS")
  
# Creating an index-- since the data is only linked to the spatial information via order, we need to make sure it's preserved.
  chem_data<-data.table(chem_sp@data)
  chem_data[,sp_index:=seq(1:nrow(chem_data))]
  
# OK, there are way too many unhelpful columns in this dataset; let's parse it down
  chem_data<-chem_data[,list(sp_index,X,Y,RM,
                             UpperDepth_cm,LowerDepth_cm, Unit,
                             Chemical,ChemicalGroup, SampleDate, 
                             Detected, observed_value=ValueOrHalfQL,dredged_flag=FE_NAVCHAN_F)]

# Some basic cleanup: I only want to know about samples that have a valid entry for "Detected".
  chem_data<-chem_data[!is.na(Detected)]
  
# What year was each sample taken?
  chem_data[,Year:=as.numeric(substr(as.character(chem_data$SampleDate),1,4))]

# # If it was dredged, how many years ago was it dredged?
#   chem_data[DredgeYear=="2003/2004",DredgeYear:="2004"] #Assign ambiguous year ("2003/2004") later year, since we can't assume more time ahs passed than actually has between dredging and sampling.
#   chem_data[,DredgeYear:=as.numeric(DredgeYear)]
#   chem_data[,dredge_gap:=Year-DredgeYear]

    chem_data[,log_value:=log(observed_value)] # Let's try a log-transform for the value variables 
    
    # Check out sample mid-points, categorize into mid-point groups for core depth
    chem_data[,sample_midpoint:=(LowerDepth_cm-UpperDepth_cm)/2] # Generate mid-point of samples
    chem_data[,sample_midpoint_group:=findInterval(sample_midpoint, seq(0,275,25))*25] # Categorizing into max depths
    chem_data[,noise:=rnorm(nrow(chem_data),mean=0,sd=.2)] # Adding a column of "noise" to be able to jitter our plots
    
# Making a structured list for each of the chemicals with data sets, units, and 
    chemlist<-list()
    chemlist[["PAHs"]]<-list(dataset=chem_data[ChemicalGroup=="PAHs",],colors="thanksgiving",name="PAHs",unit="ug/kg dw")
    chemlist[["Arsenic"]]<-list(dataset=chem_data[Chemical=="Arsenic",],colors="seaside",name="Arsenic",unit="ug/kg dw")
    chemlist[["PCBs"]]<-list(dataset=chem_data[Chemical=="Total PCBs"],colors="skyforest",name="PCBs",unit="ug/kg dw")
    
#What do these points look like?
  MapSuite::PointMap(coords=chem_data,id="sp_index",xcol="X",ycol="Y",variable="Year",map_title="Samples by Year")

# Let's see if we can find the dredged navigation channel/explore the dredging history
  MapSuite::PointMap(coords=chem_data[Dredged=="Yes"],id="sp_index",xcol="X",ycol="Y",variable="DredgeYear",
                     map_title="Sample Location Dredging History",map_colors = wpal("betafish"))

  
# Let's see if we can find the dredged navigation channel/explore the dredging history
  MapSuite::PointMap(coords=chem_data,id="sp_index",xcol="X",ycol="Y",variable="RM",
                     map_title="River Miles",map_colors = wpal("betafish"))
    
# What about different chemical levels-- some plots 
  
  ## Arsenic 
    MapSuite::PointMap(coords=chem_data[Chemical=="Arsenic",],id="sp_index",xcol="X",ycol="Y",
                       variable="Detected",map_title="Arsenic Detection")
    MapSuite::PointMap(coords=chem_data[Chemical=="Arsenic",],id="sp_index",xcol="X",ycol="Y",
                       variable="observed_value",map_title="Arsenic Levels")
    MapSuite::PointMap(coords=chem_data[Chemical=="Arsenic",],id="sp_index",xcol="X",ycol="Y",
                       variable="log_value",map_title="Log-Transformed Arsenic Levels",
                       map_colors=wpal("bright_fire"))
  
  ## PCBs (polychlorinated biphenyls)
    MapSuite::PointMap(coords=chem_data[Chemical=="Total PCBs",],id="sp_index",xcol="X",ycol="Y",
                       variable="log_value",map_title="Log-transformed Total PCBs Levels",
                       map_colors=wpal("cool_blue_jeans"))
    
  ## PAHs (polycyclic aromatic hydrocarbons)
    MapSuite::PointMap(coords=chem_data[ChemicalGroup=="PAHs",],id="sp_index",xcol="X",ycol="Y",
                       variable="log_value",map_title="Log-transformed PAH Levels",
                       map_colors=wpal("ocean"))
  
  # By River Depth
    
    for (chemical in names(chemlist)){
      d<-chemlist[[chemical]]
      
      p1<-ggplot() +
      #geom_point(data=d[["dataset"]][Detected=="No",], aes(x=RM, y=LowerDepth_cm)) + # Undetected
      geom_point(data=d[["dataset"]][Detected=="Yes",], aes(x=RM, y=LowerDepth_cm, color=log_value)) + 
        xlab("River Miles") + 
        ylab("Core Depth (lower boundary, in cm)") + 
        ggtitle(d[["name"]], subtitle="Detected Values visualized by Core Depth and River Miles") + theme_bw() + 
        scale_color_gradientn(colors=wpal(d[["colors"]])) +
        guides(fill=guide_colourbar(title=paste0(d[["unit"]]), title.position="top", barheight=10, barwidth=1,
                                    label=TRUE, ticks=FALSE, direction="vertical"))+ylim(0,600)
      print(p1)
      
      
      # Core-style rectangles
      p1<-ggplot() +
        #geom_point(data=d[["dataset"]][Detected=="No",], aes(x=RM, y=LowerDepth_cm)) + # Undetected
        geom_rect(data=d[["dataset"]][Detected=="Yes",], aes(xmin=RM+noise, xmax=RM+noise+.02, ymin=-1*UpperDepth_cm, ymax=-1*LowerDepth_cm,fill=log_value)) + 
        xlab("River Miles") + 
        ylab("Core Depth (lower boundary, in cm)") + 
        ggtitle(d[["name"]], subtitle="Detected Values visualized by Core Depth and River Miles") + theme_bw() + 
        scale_fill_gradientn(colors=wpal(d[["colors"]])) +
        guides(fill=guide_colourbar(title=paste0(d[["unit"]]), title.position="top", barheight=10, barwidth=1,
                                    label=TRUE, ticks=FALSE, direction="vertical"))+ylim(-600,0)
      print(p1)
      
      # OK, so the cores really close to the surface have so many, it's difficult to tell what's happening.
      # Let's plot those separately:
      p1<-ggplot() +
        geom_point(data=d[["dataset"]][Detected=="Yes" & UpperDepth_cm<10,], aes(x=RM+noise, y=SampleDate, color=log_value)) + 
        xlab("River Miles") + 
        ylab("Year of Observation") + 
        ggtitle(d[["name"]], subtitle="Detected Values visualized by Core Depth and River Miles") + theme_bw() + 
        scale_color_gradientn(colors=wpal(d[["colors"]])) +
        guides(color=guide_colourbar(title=paste0(d[["unit"]]), title.position="top", barheight=10, barwidth=1,
                                    label=TRUE, ticks=FALSE, direction="vertical"))
      print(p1)
      
      ## OK, now let's try 
      
       t<-1990
       while(t<=5){
        
        p_time<-ggplot() + geom_point(data=d[["dataset"]][Detected=="Yes" & (Year>=t) & (Year<(t+5)),], aes(x=RM, y=LowerDepth_cm, color=log_value)) + 
          xlab("River Miles") + 
          ylab("Core Depth (lower boundary, in cm)") + 
          ggtitle(d[["name"]], subtitle=paste0("Years ",t," to ",t+5)) + theme_bw() + 
          scale_color_gradientn(colors=wpal(d[["colors"]])) +
          guides(fill=guide_colourbar(title=paste0(d[["unit"]]), title.position="top", barheight=10, barwidth=1,
                                      label=TRUE, ticks=FALSE, direction="vertical"))+ylim(0,600)
        print(p_time)
       }
      
      # p3<-ggplot(d[["dataset"]][Detected=="Yes" & Year>=t&RM<=(t+5),], aes(x=Year, y=sample_midpoint, color=log_value)) + geom_point() + 
      #   xlab("Time in Years") + 
      #   ylab("Sediment Core Depth, Midpoint in cm") + 
      #   ggtitle(paste0(d[["name"]],": Core Depth and Values vs Time"), subtitle=paste0("Mile ",t," to ",t+1)) + theme_bw() + 
      #   scale_color_gradientn(colors=wpal(d[["colors"]])) +
      #   guides(fill=guide_colourbar(title="N Stations", title.position="top", barheight=10, barwidth=1,
      #                               label=TRUE, ticks=FALSE, direction="vertical"))
      # print(p3)
      # t<-t+1
      # }
      
      # # Where was data with high values taken?
      # p2<-ggplot(d[["dataset"]][Detected=="Yes",], aes(x=RM, y=log_value)) + geom_hex() + 
      #   xlab("River Miles") + 
      #   ylab(d[["dataset"]][Detected=="Yes",]$Unit[1]) + 
      #   ggtitle(d[["name"]], subtitle="Frequency of Detected Values Visualized by River Miles") + theme_bw() + 
      #   scale_fill_gradientn(colors=wpal(d[["colors"]])) +
      #   guides(fill=guide_colourbar(title="N Stations", title.position="top", barheight=10, barwidth=1,
      #                               label=TRUE, ticks=FALSE, direction="vertical"))
      # print(p2)

      # Where was data with high values taken?

      # t<-0      
      # while(t<=5){
      # p3<-ggplot(d[["dataset"]][Detected=="Yes" & RM>=t&RM<=(t+1),], aes(x=Year, y=sample_midpoint, color=log_value)) + geom_point() + 
      #   xlab("Time in Years") + 
      #   ylab("Sediment Core Depth, Midpoint in cm") + 
      #   ggtitle(paste0(d[["name"]],": Core Depth and Values vs Time"), subtitle=paste0("Mile ",t," to ",t+1)) + theme_bw() + 
      #   scale_color_gradientn(colors=wpal(d[["colors"]])) +
      #   guides(fill=guide_colourbar(title="N Stations", title.position="top", barheight=10, barwidth=1,
      #                               label=TRUE, ticks=FALSE, direction="vertical"))
      # print(p3)
      # t<-t+1
      # }
    }
      
       chemical<-"Arsenic"
       d<-chemlist[[chemical]]
       
       # Let's map the river, based on mileage increments
       t<-0
       while(t<=5){
       MapSuite::PointMap(coords=d[["dataset"]][Detected=="Yes" & RM>=t&RM<=(t+1),],id="sp_index",xcol="X",ycol="Y",variable="DredgeYear",
                          map_title="Sample Location Dredging History",map_colors = wpal("betafish"))
       t<-t+1
       }
       
       
 