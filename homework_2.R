install.packages("maps")
install.packages("maptools")
install.packages("rgeos") # ...
library(sp)
library(maps)
library(maptools)
library(rgeos)
library(raster)

# R uses class/method mechanisms
##### R as calculator #####
pi*10^2 # pi is a built-in constant  #OR
"*"(pi,"^"(10,2))
pi*(1:10)^2  
x <- pi*10^2
x
print(x)
print(x,digits=12)
class(x) # method used to handle the class
typeof(x) # storeage mode of object
# generic functions: print / plot/ summary

# data set cars
?cars
class(cars)
typeof(cars)
names(cars)
summary(cars)
data.frame(cars) # view the data as a rectangle of rows of observation and columns of values of variables of interest
# attribute values can be integer and floating point numeric types, logical, character and derived classes.
str(cars)
row.names(cars) # displays row names
class(dist~speed) # (response variable ~ determining variable)
lm(dist ~ speed, data = cars)
cars$qspeed <- cut(cars$speed, breaks = quantile(cars$speed),include.lowest = TRUE)
is.factor(cars$qspeed)
plot(dist ~ speed, data = cars) #numerical scatterplot
plot(dist ~ qspeed, data = cars) #factor: boxplots
lm(dist ~ qspeed, data = cars)

##### Spatial objects #####
getClass("Spatial") # returns classes + subclasses 
getClass("CRS") # "+proj=longlat" --> longlat: eastings always go before northings in sp classes

m <- matrix(c(0,0,1,1),ncol=2,dimnames=list(NULL,c("min","max")))
m
crs <- CRS(projargs=as.character(NA)) #In R, the notation used to describe the CRS is proj4string
crs
S <- Spatial(bbox=m,proj4string=crs) # a bounding box matrix
S
bb <- matrix(c(350,85,370,95),ncol=2,dimnames=list(NULL,c("min","max")))
Spatial(bb,proj4string = CRS("+proj=longlat")) ## ??? it works? p.30

##### SpatialPoints ##### (first subclass of Spatial)
# about: methods of spatial objects, extending spatial objects

CRAN_df <- read.table("http://www.asdar-book.org/datasets/CRAN051001a.txt", header = TRUE)
CRAN_df
CRAN_mat <- cbind(CRAN_df$long,CRAN_df$lat)
CRAN_mat
row.names(CRAN_mat) <- 1:nrow(CRAN_mat)
str(CRAN_mat)

getClass("SpatialPoints")
llCRS <- CRS("+proj=longlat +ellps=WGS84")
CRAN_sp <- SpatialPoints(CRAN_mat,proj4string = llCRS)
summary(CRAN_sp) # result: Is projected: here FALSE, because the string "longlat" is included in the projection description)

## METHODS (= to access the values of the slots of Spatial objects)
# bbox method returns the bounding box of the object; used to: preparing plotting methods AND handling data objects
# The first row reports the west–east range and the second the south–north direction.

bbox(CRAN_sp)
proj4string(CRAN_sp) # displays CRS
proj4string(CRAN_sp) <- CRS(as.character(NA)) # change the assigned CRS
proj4string(CRAN_sp)

brazil <- which(CRAN_df$loc == "Brazil") # extract coordinates of brazil
brazil
coordinates(CRAN_sp)[brazil, ] # displays coordinates of data set in (), indexing rows = brazil
summary(CRAN_sp[brazil, ])

south_of_equator <- which(coordinates(CRAN_sp)[, 2] < 0)
south_of_equator # you can check: data.frame(CRAN_sp)
summary(CRAN_sp[-south_of_equator, ])

##### Data Frames for Spatial Point Data
str(row.names(CRAN_df)) # Aim now: associate the correct rows of our data frame object with ‘their’ point coordinates
# If the matrix of point coordinates has row names and the match.ID argument is set to its default value of TRUE, then the matrix row names are checked against the row names of the data frame.
CRAN_spdf1 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df, proj4string = llCRS, match.ID = TRUE)
data.frame(CRAN_spdf1) # look at what has been created
CRAN_spdf1[4, ]
str(CRAN_spdf1$loc) # OR:
str(CRAN_spdf1[["loc"]])

s <- sample(nrow(CRAN_df))
CRAN_spdf2 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df[s,], proj4string = llCRS, match.ID = TRUE) ## What does indexing [s] mean?
all.equal(CRAN_spdf2, CRAN_spdf1)
CRAN_spdf2[4, ]

CRAN_df1 <- CRAN_df
row.names(CRAN_df1) <- sample(c(outer(letters, letters,paste, sep = "")), nrow(CRAN_df1))
CRAN_spdf3 <- SpatialPointsDataFrame(CRAN_mat, CRAN_df1,proj4string = llCRS, match.ID = TRUE) # Error, because of: non-matching ID values

########## p.36 ## understand different data set

CRAN_spdf4 <- SpatialPointsDataFrame(CRAN_sp, CRAN_df)
all.equal(CRAN_spdf4, CRAN_spdf2)
CRAN_df0 <- CRAN_df
coordinates(CRAN_df0) <- CRAN_mat
proj4string(CRAN_df0) <- llCRS
all.equal(CRAN_df0, CRAN_spdf2)
str(CRAN_df0, max.level = 2)

CRAN_df1 <- CRAN_df
names(CRAN_df1)
coordinates(CRAN_df1) <- c("long", "lat")
proj4string(CRAN_df1) <- llCRS
str(CRAN_df1, max.level = 2)

#### p.37

turtle_df <- read.csv("C:\\Users\\nina-\\Documents\\playground\\seamap105_mod.csv", sep=",",header=TRUE)
print(turtle_df)
summary(turtle_df)
timestamp <- as.POSIXlt(strptime(as.character(turtle_df$obs_date),"%m/%d/%Y %H:%M:%S"), "GMT")
?as.POSIXct()
?strptime()
turtle_df1 <- data.frame(turtle_df, timestamp = timestamp)
turtle_df1$lon <- ifelse(turtle_df1$lon < 0, turtle_df1$lon + 360, turtle_df1$lon)
turtle_sp <- turtle_df1[order(turtle_df1$timestamp),]
coordinates(turtle_sp) <- c("lon", "lat")
proj4string(turtle_sp) <- CRS("+proj=longlat +ellps=WGS84")


###### Spatial Lines
#### p.38
getClass("Line")
getClass("Lines")
getClass("SpatialLines")

## p.39
japan <- map("world", "japan", plot = FALSE)
p4s <- CRS("+proj=longlat +ellps=WGS84")
SLjapan <- map2SpatialLines(japan, proj4string = p4s)
str(SLjapan, max.level = 2)

## p.40
Lines_len <- sapply(slot(SLjapan, "lines"), function(x) length(slot(x,"Lines")))
table(Lines_len)

volcano_sl <- ContourLines2SLDF(contourLines(volcano))
t(slot(volcano_sl, "data"))

##p.41
llCRS <- CRS("+proj=longlat +ellps=WGS84")
auck_shore <- MapGen2SL("http://www.asdar-book.org/datasets/auckland_mapgen.dat", llCRS)
summary(auck_shore)

###### Spatial Polygons --> p.41
lns <- slot(auck_shore, "lines")
table(sapply(lns, function(x) length(slot(x, "Lines"))))
islands_auck <- sapply(lns, function(x) {crds <- slot(slot(x, "Lines")[[1]], "coords"); identical(crds[1, ], crds[nrow(crds), ])})
table(islands_auck)

getClass("Polygon")
getClass("Polygons")
getClass("SpatialPolygons")

islands_sl <- auck_shore[islands_auck]
list_of_Lines <- slot(islands_sl, "lines")
islands_sp <- SpatialPolygons(lapply(list_of_Lines, function(x) {Polygons(list(Polygon(slot(slot(x, "Lines")[[1]],"coords"))), ID = slot(x, "ID"))}), proj4string = CRS("+proj=longlat +ellps=WGS84"))
summary(islands_sp)
slot(islands_sp, "plotOrder")
order(sapply(slot(islands_sp, "polygons"), function(x) slot(x,"area")), decreasing = TRUE)

###### Spatial Polygons Data Frame Objects p. 44

state.map <- map("state", plot = FALSE, fill = TRUE)
IDs <- sapply(strsplit(state.map$names, ":"), function(x) x[1])
state.sp <- map2SpatialPolygons(state.map, IDs = IDs,proj4string = CRS("+proj=longlat +ellps=WGS84"))

sat <- read.table("http://www.asdar-book.org/datasets/state.sat.data_mod.txt", row.names = 5, header = TRUE)
str(sat)
id <- match(row.names(sat), row.names(state.sp))
row.names(sat)[is.na(id)]
sat1 <- sat[!is.na(id), ]
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)
str(slot(state.spdf, "data"))
str(state.spdf, max.level = 2)

rownames(sat1)[2] <- "Arizona"
SpatialPolygonsDataFrame(state.sp, sat1)

DC <- "district of columbia"
not_dc <- !(row.names(state.spdf) == DC)
state.spdf1 <- state.spdf[not_dc, ]
dim(state.spdf1)
summary(state.spdf1)


###### Holes and Ring Direction
load("C:\\Users\\nina-\\Documents\\playground\\high.RData")
manitoulin_sp <- high[[4]] ## ???????? --> see environments: high
length(slot(manitoulin_sp, "polygons"))   
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"),function(x) slot(x, "hole"))  ### wrong TRUE and FALSE
sapply(slot(slot(manitoulin_sp, "polygons")[[1]], "Polygons"),function(x) slot(x, "ringDir")) ### wrong -1 and 1

manitoulin_sp <- createSPComment(manitoulin_sp)
sapply(slot(manitoulin_sp, "polygons"), comment)

### SpatialGrid and SpatialPixel Objects

getClass("GridTopology")
bb <- bbox(manitoulin_sp)
bb
cs <- c(0.01, 0.01)
cc <- bb[, 1] + (cs/2)
cd <- ceiling(diff(t(bb))/cs)
manitoulin_grd <- GridTopology(cellcentre.offset = cc,cellsize = cs, cells.dim = cd)
manitoulin_grd

getClass("SpatialGrid")
p4s <- CRS(proj4string(manitoulin_sp))
manitoulin_SG <- SpatialGrid(manitoulin_grd, proj4string = p4s)
summary(manitoulin_SG)

auck_el1 <- raster("C:\\Users\\nina-\\Documents\\playground\\70042108\\70042108.tif",el=1,band=1) # readGDAL (4th chapter)
class(auck_el1)
?slot()
slot(auck_el1, "grid")   # doesn't work? p,50
slot(auck_el1, "bbox")
object.size(auck_el1)
object.size(slot(auck_el1, "data"))
is.na(auck_el1$band1) <- auck_el1$band1 <= 0
summary(auck_el1$band1)
auck_el2 <- as(auck_el1, "SpatialPixelsDataFrame")
object.size(auck_el2)
object.size(slot(auck_el2, "grid.index"))
object.size(slot(auck_el2, "coords"))
sum(is.na(auck_el1$band1)) + nrow(slot(auck_el2, "coords"))
prod(slot(slot(auck_el2, "grid"), "cells.dim"))
auck_el_500 <- auck_el2[auck_el2$band1 > 500, ]
summary(auck_el_500)
object.size(auck_el_500)
data(meuse.grid)
mg_SP <- SpatialPoints(cbind(meuse.grid$x, meuse.grid$y))
summary(mg_SP)
mg_SPix0 <- SpatialPixels(mg_SP)
summary(mg_SPix0)
prod(slot(slot(mg_SPix0, "grid"), "cells.dim"))

mg_SPix1 <- as(mg_SP, "SpatialPixels")
summary(mg_SPix1)

###### Raster Objects and the raster Package

r <- raster("C:\\Users\\nina-\\Documents\\playground\\70042108\\70042108.tif")
class(r)
inMemory(r)
object.size(r)
cellStats(r, max)
cellStats(r, min)
inMemory(r)
out <- raster(r)
bs <- blockSize(out) 
out <- writeStart(out, filename = tempfile(), overwrite = TRUE)
for (i in 1:bs$n) {v <- getValues(r, row = bs$row[i], nrows = bs$nrows[i]); v[v <= 0] <- NA; writeValues(out, v, bs$row[i])}
out <- writeStop(out)
cellStats(out, min)

cellStats(out, max)
inMemory(out)
plot(out, col = terrain.colors(100))
r1 <- as(out, "SpatialGridDataFrame")
summary(r1)