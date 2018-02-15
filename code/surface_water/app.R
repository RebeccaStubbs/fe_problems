## Shiny Web App for Surface water data exploration 
## R. Stubbs, Feb 2018

# Surface Water Data Viz and Seasonality; RStubbs 02/2018
# Generates plots and calculates sesaonally-adjusted data
# to compare to raw data to determine magnitude of seasonality
# Input: Surface water observations by station-analyte

rm(list=ls()) # Clear working environment

library(shiny) #Load shiny (an R package for interactive visuaizations)library:
library("MapSuite") #Self-written library, has many common libs as dependencies
library("hexbin") # For the hex-bin plots
library(lme4) # Mixed-effects modeling package
library(htmlTable)

# Read in surface water observations as data.table sw
setwd("/Users/stubbsrw/Documents/git_code/stubbs_repo/fe_problems/code/")
sw<-fread("Dataset_C.csv")

## Dataset Prep
    
    # Parse out date information from character date column
    sw[,index:=seq(1:nrow(sw))]
    sw[,Month:=as.numeric(strsplit(SampleDate,"/")[[1]][1]),by=index] 
    sw[,Day:=as.numeric(strsplit(SampleDate,"/")[[1]][2]),by=index]
    sw[,Year:=as.numeric(strsplit(SampleDate,"/")[[1]][3]),by=index]
    # Create formal column of integer-date
    sw[,Date:=as.IDate(paste0((2000+Year),"-",Month,"-",Day))] 
    # For each analyte, discover the minimum date; generate a yr/month index from that date:
    sw[,year_index:=Year-min(Year,na.rm=T),by=StandardAnalyte]
    # Number of months from the start of the samples
    sw[,month_index:=12*(Year-min(Year,na.rm=T)) + Month] 
    
    sw[Month %in% c(12,1,2), Season:='Winter']
    sw[Month %in% c(3,4,5), Season:='Spring']
    sw[Month %in% c(6,7,8), Season:='Summer']
    sw[Month %in% c(9,10,11), Season:='Fall']
    # Defining Season as a factor variable
    sw[,Season:=factor(Season, levels = c("Winter","Spring","Summer","Fall"))] 
    
    # Add variables on mean concentration for each 
    #analyte by site and globally/for all samples
    sw[,mean_conc:=mean(StandardResult,na.rm=T),
       by=list(StationName,StandardAnalyte)]
    
    # Just in case a log-transform would be more informative, 
    # although NaNs will exist where the observation is negative
    sw[,log_obs:=log(StandardResult)] 

## Functions for Graphing
    # Define function for p using the HexBin Frequency graphics, where the number of 
    #stations with an observation in that category is essentially heat-mapped
    plot_full_ts<-function(a){
      p<-ggplot(sw[StandardAnalyte==a], aes(x=Date, y=StandardResult)) + 
        geom_hex() + # honeycomb-plot geometry
        scale_x_date(labels = function(x) format(x, "%b-%y")) + xlab("Time") +
        ylab(sw[StandardAnalyte==a]$StandardUnit[1]) + 
        ggtitle(paste0(a), 
                (subtitle="Observations Over Full Time Series, All Stations")) + 
        theme_bw() + scale_fill_gradientn(colors=wpal("berries")) +
        guides(fill=guide_colourbar(title="N Stations", 
                                    title.position="top", barheight=10, barwidth=1,
                                    label=TRUE, ticks=FALSE, direction="vertical")) 
      return(p)
    }
    
    plot_by_month<-function(a){
      p<-ggplot(sw[StandardAnalyte==a], aes(x= Month, y=log_obs)) + geom_hex() + 
        ggtitle(paste0(a), 
                subtitle="Observations in Each Month, All Years, All Stations") + 
        scale_x_continuous(limits=c(1,12),breaks=seq(1,12), 
                           labels=c("Jan","Feb","Mar","Apr",
                                    "May","Jun","Jul","Aug",
                                    "Sep","Oct","Nov","Dec")) +
        ylab(sw[StandardAnalyte==a]$StandardUnit[1]) + theme_bw() + 
        scale_fill_gradientn(colors=wpal("berries")) +
        guides(fill=guide_colourbar(title="N Stations", title.position="top", 
                                    barheight=10, barwidth=1, direction="vertical",
                                    label=TRUE, ticks=FALSE))
      return(p)
    }
    
    # Define a color pallette for the factor variable, Season
    SeasonColors<-wpal("foliage",noblack=T,n=4) # grab colors from MapSuite's pallettes
    names(SeasonColors) <- c("Winter","Spring","Summer","Fall")
    
    # Define function to generate plot
    MakeAnalyteTSPlot<-function(a,s){
      
      p<-ggplot(sw[StandardAnalyte==a & StationName==s], 
                aes(x= Date, y=StandardResult, color=Season)) + geom_point(size=4) + 
        xlab("Date of Sample") + 
        scale_x_date(labels = function(x) format(x, "%b-%y")) + 
        ylab(sw[StandardAnalyte==a & StationName==s]$StandardUnit[1]) + 
        ggtitle(paste0(a), subtitle=paste0("Station ",s)) + theme_bw()  + 
        scale_colour_manual(name = "Seasons",values = SeasonColors, drop=F) +
        geom_hline(yintercept = sw[StandardAnalyte==a &
                                     StationName==s]$mean_conc[1]) + 
        annotate("text", min(sw[StandardAnalyte==a & StationName==s]$Date), 
                 sw[StandardAnalyte==a & StationName==s]$mean_conc[1], 
                 vjust = -1, label = "Mean")
      
      return(p)  
    }

# Initiate a shiny application
shiny::shinyApp(

  # Define the User Interface
    ui = pageWithSidebar(
      # Application title
        headerPanel("Surface Water Data Exploration"),
    # Sidebar with a selectable input for analytes and stations
      sidebarPanel(
         selectInput("analyte", "Analyte:",
                    choices = unique(sw$StandardAnalyte)),
          selectInput("station", "Station:",
                    choices = unique(sw$StationName))
      ),
    # Show the time series GGplot in the main panel
      mainPanel(
          tabsetPanel(
            tabPanel("Station-Specific", plotOutput("Analyte_Station_TimeSeries")), 
            tabPanel("Time Series", plotOutput("analyte_ts")), 
            tabPanel("Month by Month", plotOutput("analyte_month"))
          )
      
      )
    ),

  # Define the server/output
  server = function(input, output) {
    
    # Define the Analyte_Station_TimeSeries output  as the TS plot in fn above
      output$Analyte_Station_TimeSeries <- renderPlot({
        ts<-MakeAnalyteTSPlot(a=input$analyte,s=input$station) # Get inputs from UI, use function
        print(ts)
      })
      
      output$analyte_ts <- renderPlot({
        analyte_ts<-plot_full_ts(input$analyte) # Get inputs from UI, use function
        print(analyte_ts)
      })
      
      output$analyte_month <- renderPlot({
        analyte_month<-plot_by_month(input$analyte) # Get inputs from UI, use function
        print(analyte_month)
      })
      

  },
  options = list(height = 500) # Define how big this visualization GUI is
)




