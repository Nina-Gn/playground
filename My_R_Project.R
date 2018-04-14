# necessary libraries
library(raster)
library(sp)
library(RStoolbox)
library(rgdal)
library(shapefiles)
library(foreign)
library(rgeos)
library(hydromad)
library(reshape2) # for melt
library(plotly)
library(ggplot2)
library(rasterVis)
library(rgl)

# Main AIM: Drainage Network Delineation of WÃ¼rzburg
# Data source: USGS Earth Explorer (SRTM, Landsat 8), QGIS - QuickOSM
# directory:
setwd("C:\\Users\\nina-\\Documents\\playground\\project_1")

### Task 1: Topographic Analysis and visualisation (in combination with LULC classification)
# Data input + reprojection:
sr_utm32u <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
srtm_wueL <- raster("C:\\Users\\nina-\\Documents\\playground\\project_1\\SRTM_InputData\\n49_e009_1arc_v3.tif") # Source: USGS Earth Explorer
srtm_wueR <- raster("C:\\Users\\nina-\\Documents\\playground\\project_1\\SRTM_InputData\\n49_e010_1arc_v3.tif")
  # srtm_wueL and srtm_wueR --> requirement: UTM zone 32U, because of metric units
srtm_wueL_utm <- projectRaster(srtm_wueL, crs = sr_utm32u, res=30) # by adding res = 30 -> square pixels, required in case you want to use RSAGA
srtm_wueR_utm <- projectRaster(srtm_wueR, crs = sr_utm32u, res=30)
  # merge to get full coverage
srtm_wue_merge <- merge(srtm_wueL_utm,srtm_wueR_utm, tolerance=0.1)

wue_districts <- rgdal::readOGR("C:\\Users\\nina-\\Documents\\playground\\project_1\\Shapes_InputData\\wue_city_districts.shp") # extent of study area
wue_river <- rgdal::readOGR("C:\\Users\\nina-\\Documents\\playground\\project_1\\Shapes_InputData\\main_river_asShape.shp")
wue_districts_utm <- spTransform(wue_districts,sr_utm32u) # polygons
wue_river_utm <- spTransform(wue_river,sr_utm32u) # line

srtm_wue_utm_small<- crop(srtm_wue_merge,wue_districts_utm)
writeRaster(srtm_wue_utm_small, filename="srtm_wue_utm_small.tif", datatype='FLT4S',format="GTiff", overwrite=TRUE) # SAVE
plot(srtm_wue_utm_small)
plot(wue_districts_utm, add=TRUE)
plot(wue_river_utm, col="dodgerblue4", add=TRUE)

# if plot() does not work due to error: cannot coerce type 'S4' to vector of type 'double'
devtools::install_github("rmcelreath/rethinking", force = TRUE)
library(rethinking)

## Some statistics
# query - calculate Area of districts
sum(rgeos::gArea(wue_districts_utm,byid=TRUE)) # 101403798 m2: 101,403798 km2
# height info query
srtm_wue_utm_small_matrix <- as.matrix(srtm_wue_utm_small)
hist(srtm_wue_utm_small_matrix, main="Height histogram of Würzburg", xlab="occuring heights in m",ylab="frequency of pixels") # Which heights exist in the area?
min(srtm_wue_utm_small_matrix) # 160m
max(srtm_wue_utm_small_matrix) # 384m

## terrain() function
ext <- extent(srtm_wue_utm_small) # define extent
slope=terrain(srtm_wue_utm_small,opt='slope') # you can adapt the unit: degree or radiant desired? 
aspect=terrain(srtm_wue_utm_small,opt='aspect')
hill=hillShade(slope,aspect,0,0)
plot(hill,col=grey(0:100/100),legend=FALSE) # desired grey level [0(black):1(white)], specified opacity
plot(srtm_wue_utm_small,col=rainbow(25,alpha=0.35),add=TRUE) # add heights to hillshade, increases detail of information
plot(aspect,col=rainbow(20,alpha=1)) # alpha = transparency, the higher the rainbow color number, the more detailed the result

## Combination with LULC - put categorial raster over DEM
# Landsat image to classify
lsat_wue <- stack(list.files("C:\\Users\\nina-\\Documents\\playground\\project_1\\LC08_L1TP_194025_20171015_20171024_01_T1.tar\\lsat_bands_wue\\",pattern="TIF",full.names=TRUE)) # load raster data in one go
lsat_wue_crop <- crop(lsat_wue,ext) # crop to right extent
# Source: http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/static/talk1.pdf
# LULC classification
TrainingData_wue <- rgdal::readOGR("C:\\Users\\nina-\\Documents\\playground\\project_1\\TrainingData_wue.shp")
TrainingData_wue_utm <- spTransform(TrainingData_wue,sr_utm32u) # Levels: agriculture forest settlement water
superClass(lsat_wue_crop,trainData = TrainingData_wue_utm, responseCol="class") # default's at random forest
plot(SC1$map)
# change color setting
arg <- list(at=seq(1,4,1),labels=c("agriculture","forest","settlement","water"))
color <- c("green","darkgreen","yellow","blue")
SC_colour <- plot(SC1$map,col=color,acis.arg=arg)
# validation of classification
sc_val <- superClass(SC1$map,trainData=TrainingData_wue_utm,responseCol="class",trainPartition=0.7)
sc_val$validation$performance
# SAVE
writeRaster(SC1$map,filename="SC_wue",format="GTiff",overwrite=TRUE)

# 3D Visualization (rasterVis)
library(rasterVis)
plot3D(srtm_wue_utm_small, drape=SC1$map, col=c("lawngreen","darkgreen","darkgoldenrod","blue") , at=4) # at to define breakpoints
rgl.snapshot("C:\\Users\\nina-\\Documents\\playground\\project_1\\SRTMcategorialRaster.png",fmt="png", top=FALSE)

### another interactive 3D Surface Plot of Würzburg
# 1st option
wue.plotly1 <- plot_ly(z=~srtm_wue_utm_small_matrix) %>% add_surface()
wue.plotly1
# 2nd option
wue.plotly2 <- plot_ly(z=~srtm_wue_utm_small_matrix, name="isolines") %>% add_contour()
wue.plotly2

### Task 2 - Hydrologic analysis (TauDEM)
# List of packages for Hydrologists: http://abouthydrology.blogspot.de/2012/08/r-resources-for-hydrologists.html?m=1

# Required beforehand: install everything listed here - http://hydrology.usu.edu/taudem/taudem5/downloads2.html
# Understand mpiexec programming: http://docs.par-tec.com/html/psmpi-userguide/rn01re01.html
# Manual for commands: http://hydrology.usu.edu/taudem/taudem5/TauDEM51CommandLineGuide.pdf

z=srtm_wue_utm_small # simplyfication
# 1 Pitremove
system("mpiexec -n 8 pitremove -z srtm_wue_utm_small.tif -fel srtm_wue_utm_smallfel.tif") # equal to fill sinks (necessary to avoid confusions of flow accumulation (if pits are not removed, there would be more sinks, where the water could flow into))
# mpiexec is a message passing interface standard. The basic grammar of the mpiexec command is: mpiexec -np num prog [args] 
# -n OR -np num = to specify the number of processes to start (n and np are synonyms)
# prog = program/ job to carry out = pitremove
# [args] = optional arguments which will be passed to each task
# zfile: srtm input
# felfile: pitremoved output data 
fel=raster("srtm_wue_utm_smallfel.tif")
plot(fel) # faster than the fill sinks command of the R package topmodel

# D8 flow directions:
# "manual": http://hydrology.usu.edu/taudem/taudem5/help53/D8FlowDirections.html
# Creates 2 grids
# - The first contains the flow direction from each grid cell to one of its adjacent or diagonal neighbors, calculated using the direction of steepest descent. 
# - The second contains the slope, as evaluated in the direction of steepest descent, and is reported as drop/distance, i.e. tan of the angle.
system("mpiexec -n 8 D8Flowdir -p srtm_wue_utm_smallp.tif -sd8 srtm_wue_utm_smallsd8.tif -fel srtm_wue_utm_smallfel.tif",show.output.on.console=F,invisible=F)
# D8 = deterministic 8 Flowdir command
# you could use the DinfFlowdir as well (might better suit your needs?) -> see one step later
# felfile: Pit filled elevation input data
# pfile: D8 flow directions output
# sd8file: D8 slopes output
p=raster("srtm_wue_utm_smallp.tif") # = Flow Direction Raster, in comparison to the terrain(opt='flowdir') command, similarly quick and easy
plot(p)
sd8=raster("srtm_wue_utm_smallsd8.tif")
plot(sd8)

# DInf flow directions:
system("mpiexec -n 8 DinfFlowdir -ang srtm_wue_utm_smallang.tif -slp srtm_wue_utm_smallslp.tif -fel srtm_wue_utm_smallfel.tif",show.output.on.console=F,invisible=F)
ang=raster("srtm_wue_utm_smallang.tif")
plot(ang)
slp=raster("srtm_wue_utm_smallslp.tif")
plot(slp)

# Contributing area (= Accumulation Area)
# command produces the output file srtm_wue_utm_smallad8.tif, input is the FlowDirectionRaster
system("mpiexec -n 8 AreaD8 -p srtm_wue_utm_smallp.tif -ad8 srtm_wue_utm_smallad8.tif")
# optional: system("mpiexec -n 8 AreaDinf -ang loganang.tif -sca logansca.tif") # same as above, but with D-Infinity contributing area
ad8=raster("srtm_wue_utm_smallad8.tif")
plot(log(ad8))
# Indexing shapefile to get smaller extent
xy <- crop(log(ad8),wue_districts_utm[5,]) # row 5 is Sanderau (extent, e.g. get coordinates of your house to have a georeference)  
plot(xy)
# zoom(log(ad8))

# Dinf contributing area
system("mpiexec -n 8 AreaDinf -ang srtm_wue_utm_smallang.tif -sca srtm_wue_utm_smallsca.tif")
sca=raster("srtm_wue_utm_smallsca.tif")
plot(log(sca))
# zoom(log(sca))

# Grid Network 
# produces the output files srtm_wue_utm_smallplen.tif, srtm_wue_utm_smalltlen.tif and srtm_wue_utm_smallgord.tif containing respectively 
# (1) plen: the longest flow path along D8 flow directions to each grid cell (), 
# (2) tlen: the  total length of all flow paths that end at each grid cell, and 
# (3) gord: the grid network order. This is obtained by applying the Strahler stream ordering system to the network defined starting at each grid cell
system("mpiexec -n 8 Gridnet -p srtm_wue_utm_smallp.tif -gord srtm_wue_utm_smallgord.tif -plen srtm_wue_utm_smallplen.tif -tlen srtm_wue_utm_smalltlen.tif")
gord=raster("srtm_wue_utm_smallgord.tif")
plen=raster("srtm_wue_utm_smallplen.tif")
tlen=raster("srtm_wue_utm_smalltlen.tif")
plot(gord) # main stream = green(er)
# addition:
plot(wue_river_utm,add=TRUE) # you can check by adding the river
plot(plen)
plot(tlen)

# Threshold
# Input D8 Contributing area: ...ad8 = stream raster
# There is an option to input a threshold, which if specified considering only grid cells with proportion more than the specified threshold contributing to a grid cell as being upslope of a grid cell for the calculation of distances to the ridge
system("mpiexec -n 8 Threshold -ssa srtm_wue_utm_smallad8.tif -src srtm_wue_utm_smallsrc.tif -thresh 500")
src=raster("srtm_wue_utm_smallsrc.tif") # the higher the threshold, the viewer streams visible
plot(src)

# Stream Reach and Watershed
system("mpiexec -n 8 Streamnet -fel srtm_wue_utm_smallfel.tif -p srtm_wue_utm_smallp.tif -ad8 srtm_wue_utm_smallad8.tif -src srtm_wue_utm_smallsrc.tif -o outlet.shp -ord srtm_wue_utm_smallord.tif -tree srtm_wue_utm_smalltree.txt -coord srtm_wue_utm_smallcoord.txt -net srtm_wue_utm_smallnet.shp -w srtm_wue_utm_smallw.tif")
# from all results above, most interesting: Watersheds
plot(raster("srtm_wue_utm_smallw.tif"))
plot(raster("srtm_wue_utm_smallsrc.tif"),alpha=0.5,add=TRUE) # watershed combined with streams -> alpha = transparency

# Peuker Douglas stream definition
system("mpiexec -n 8 PeukerDouglas -fel srtm_wue_utm_smallfel.tif -ss srtm_wue_utm_smallss.tif")
ss=raster("srtm_wue_utm_smallss.tif")
plot(ss)

# Wetness Index
system("mpiexec -n 8 SlopeAreaRatio -slp srtm_wue_utm_smallslp.tif -sca srtm_wue_utm_smallsca.tif -sar srtm_wue_utm_smallsar.tif", show.output.on.console=F, invisible=F)
sar=raster("srtm_wue_utm_smallsar.tif")
wi=sar
wi[,]=-log(sar[,])
plot(wi)

### Task 3 - Statistical part: Adding precipitation and data
# P data --> https://www.nid.bayern.de/niederschlag/unterer_main/wuerzburg-wst-10655/tabelle
# Q data --> https://www.hnd.bayern.de/pegel/unterer_main/wuerzburg-24042000
PQ_data <- read.table("C:\\Users\\nina-\\Documents\\playground\\project_1\\P_Q_data_wue\\Prec_and_Drainage_Wue.csv",header=TRUE,sep=",")
# aggregate the Precipitation data by DATE
precip.wue_daily_2018 <-aggregate(PQ_data$P_mm_daily,   # data to aggregate
                                by=list(PQ_data$date),  # variable to aggregate by
                                FUN=sum,   # take the sum (total) of the precip
                                na.rm=TRUE)  # if there are NA values ignore them

# rename column names
names(precip.wue_daily_2018)[names(precip.wue_daily_2018)=="Group.1"] <- "DATE"
names(precip.wue_daily_2018)[names(precip.wue_daily_2018)=="x"] <- "PRECIPITATION"
#
mean(precip.wue_daily_2018$PRECIPITATION)

# plot precipitation of Würzburg 2017-2018
# 1 daily sum of three month in 2017 - 2018
precPlot_daily_2018 <- ggplot(data=precip.wue_daily_2018,  # the data frame
                         aes(DATE, PRECIPITATION)) +   # the variables of interest
  geom_bar(stat="identity") +   # create a bar graph
  xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  ggtitle("Daily Precipitation - Würzburg Station\n October 2017 - January 2018") + # add a title
  stat_summary(aes(y = mean(PRECIPITATION),group=1), fun.y=mean, colour="green", geom="line",group=1) # add mean of precipitation

precPlot_daily_2018 + theme(axis.text.x=element_text(angle=70, vjust=1, hjust=1)) # angle must be > 60Â° otherwise dates can not be read

# optional: calculate standard deviation of rainfall data using: apply(PQ_data[,x:x], 1, fun=sd) # calc sd of P by row(daywise) --> requires more data (e.g. include data of last 30 years)

### Analysis of precipitation and Discharge 
# Source: https://plot.ly/ggplot2/time-series/
em <- melt(PQ_data, id = "date")
qplot(date, value, data = em, geom = "line", group = variable, main="Precipitation & Drainage - Dependency?", xlab="date",ylab="values") +
  facet_grid(variable ~ ., scale = "free_y") +
  theme_get() + theme(axis.text.x=element_text(angle=70, vjust=1, hjust=1))
ggplotly()

### Task 4 Modelling
# 1 Installation: http://hydromad.catchment.org/#installation
#install.packages(c("zoo", "latticeExtra", "polynom", "car", "Hmisc","reshape"))
#install.packages("hydromad", repos="http://hydromad.catchment.org")
#install.packages("DEoptim")
#install.packages("dream", repos="http://hydromad.catchment.org")
library(hydromad)
# ts plot
rain_AND_streamflow_zoo <- zooreg(PQ_data, start = 1, end = 81, frequency = 1)
ts.plot(rain_AND_streamflow_zoo,gpars= list(col=rainbow(10)))
?hydromad
# Hydromad Input
rain_AND_streamflow_zoo_ts <- as.ts(rain_AND_streamflow_zoo) # as.ts to coerce an object to a time series, is.ts to test whether an object is a time series
is.ts(rain_AND_streamflow_zoo_ts)
# to be continued...