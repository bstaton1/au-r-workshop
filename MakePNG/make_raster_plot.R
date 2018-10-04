library(ggmap)
library(dismo)
library(dplyr)
library(sp)
library(rgdal)
library(prettymapr)
library(scales)
library(maptools)

# read in/manipulate bear data
bear = read.csv("Data/Ch6/bear.csv", stringsAsFactors = F)
bear = na.omit(bear[,c("timestamp","tag.local.identifier",
                       "location.long","location.lat")])
colnames(bear) = c("timestamp","ID","x","y")
bear = bear %>%
  group_by(ID) %>%
  filter(n() >= 5)

bear = SpatialPointsDataFrame(
  data = bear,
  coords = bear[,c("x","y")],
  coords.nrs = c(3,4),
  proj4string = CRS("+init=epsg:4326")
)

# read in other spatial objects
slovenia = readOGR(dsn="./Data/Ch6/SVN_adm",layer="SVN_adm0")
railways = readOGR(dsn = "./Data/Ch6/railways", layer = "railways")
stats = readOGR(dsn = "./Data/Ch6/SVN_adm", layer = "SVN_adm1", stringsAsFactors = F)

slovenia = spTransform(slovenia, CRS(proj4string(bear)))

bear = bear[slovenia,]

load(file = "Objects/crossings")

# put the other layers in the same crs as the basemap
base = raster("MakePNG/basemap.tif")
bear = spTransform(bear, basemap@crs)
railways = spTransform(railways, base@crs)
slovenia = spTransform(slovenia, base@crs)
proj4string(crossings) = base@crs

ppi = 600
png("img/FinalMap.png", h =  5 * ppi, w = 5 * ppi, res = "ppi")
# plot the basemap
plot(base)

# draw on the railways and crossing events
lines(railways, lwd = 10, col = "red")
points(crossings, col = alpha("blue", 0.5), cex = 5, pch = 16)

# draw on a legend
legend("bottom", legend = c("Railway", "Crossing Events"),
       lty = c(1,NA), col = c("red", alpha("blue", 0.5)),
       pch = c(NA, 16), lwd = 10, cex = 5, horiz = T, bty = "n")

# draw on other map components: scale bar and legend
addscalebar(plotunit = "latlon", htin = 0.5,
            label.cex = 5, padin = c(0.5,0.5))
addnortharrow(pos = "topleft", scale = 5, padin = c(2, 2.5))
dev.off()



