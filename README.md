# fe_problems
Work in progress for FE analytics exercises

 

Here are a set of three data analysis exercises.  These exercises are intentionally open-ended and were developed to help us understand how you think about a problem as opposed to solving for the “correct” answer.

 

Describe how you would find and track the extent of a waterbody within a 1 year period.  Explain the data, method, and platform you would use.
 

Background: The attached rds file (Dataset_B) contains chemistry data for sediment cores from an urban waterway (downloaded from here and slightly modified to achieve the attached format).  This urban waterway has a complex history of discharges from storm water, sanitary sewer, and industrial sources as well as operations that took place up to 50 years prior to when samples were collected.  Over time, substantial anthropogenic modifications (e.g. dredging and disturbance) have occurred.  The key variables of interest within this dataset include:
Coordinates: X, Y
Chemical names: Chemical
Sample date: SampleDate
Analytical chemical results: ValueOrHalfQL
Result units: Unit
Detection status (was the result above or below the analytical method detection limit): Detected
Flag indicating if the sample is in the dredged navigation channel: FE_NAVCHAN_F
Core segment upper/lower depth: UpperDepth_ft , LowerDepth_ft, UpperDepth_cm, LowerDepth_cm
Elevation of sediment at coordinate where sample was collected: FE_Z2003Bathy_MLLW
 

Other variables may be of interest/may be used with grouping efforts, but are much less of a focus. 

 

       Given the attached dataset:

1.       Divide the site into spatial and depth “zones” (groups, regions, strata) based on chemistry of Total PCBs, Arsenic, and PAHs.

2.       Are there upstream to downstream patterns?  If so, what are the differences between chemicals?

3.       Describe the trends you found and the tools/approaches that you used.  Provide PDF maps, tables, and statistics depicting the spatial zones, strata, and groupings.

4.       Are there any additional useful interpretations that you can identify?

Bonus:  Can you infer anything about the modification history of the site from the chemistry data?  If so, what did you find? What tools, approach, algorithms, and statistics did you use?

 

Background: The attached Excel file (Dataset_C) contains analytical chemistry data from a variety of surface water stations. 
 

Given the attached dataset:

Develop a tool for plotting time series by station and analyte.
Symbolize data by season.
Add a horizontal line for the mean concentration to each plot.
Which analytes have the most seasonal variability?  What tools/approach did you use?
Produce a pdf of your plots.
 