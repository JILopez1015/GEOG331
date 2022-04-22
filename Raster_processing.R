
#####Raster Exercise
##### JL 4/22/22

#load terra package
library(terra)

#set working directory
setwd("Z:/students/jlopez/")

##read a raster from file
p <- rast("Z:/data/rs_data/20190706_002918_101b_3B_AnalyticMS_SR.tif")

#plot an rgb rendering of the data
plotRGB(p,r=3,g=2,b=1)

#plot an rgb rendering of the data
plotRGB(p, r = 3, g = 2, b = 1,
        scale=65535,      #saying that darkest will be 65535
        stretch = "hist") #stetch increases contrast, looking at distribution 
                          #of bands and using the distribution

#read file with field observations of canopy data
tree <- read.csv("z:/data/rs_data/siberia_stand_data.csv", header= T)
#cc.pct- canopy cover %; agb- above ground biomass

#convert to vector object using terra package
#geom to give coordinates: always do long [x] and lat [y], so it is x,y format
#epsg:4326 is the 
gtree <- vect(tree, geom= c("Long", "Lat"), "epsg:4326")

#to get extent of data
ext(gtree)
ext(p)

#project the tree data to match the coordinate system of the raster layer
#project is repojecting coordinate system
#p is the desired crs, could just paste in the crs we want
gtree2 <- project(gtree,p)
ext(gtree2)

#to check point on the original plot
plot(gtree2, add=T, col= "red")

#create a polygon from the extent of the points
#specify the crs again:epsg:4326
b <- as.lines(ext(gtree), "epsg:4326")

#reproject the polygons to the same projections of the raster
b2 <- project(b, crs(p))
plot(b2, add =T)

#buffer the extent to 200m
b3 <- buffer(b2, width= 200)
plot(b3, add= T)

# use this to crop the raster layer so we can see just our study area
p2 <- crop(p, b3, filename = "20190706_SR_crop.tif", overwrite= TRUE)

#make a plot to see how it looks
plotRGB(p2, r =3, g= 2, b=1, scale=65535, stretch= "lin")
#lin gives a more real world image, hist stretch can wash out the color based on data

#plotting points on cropped image
points(gtree2, col= "red", cex= gtree$cc.pct/50)
#cex is character expansion, so larger vales will have a larger point
#need to run plot again to see full changes

#calculate NDVI [(NIR-Red)/(NIR+Red)]
# using cropped image and comparing NDVI value with canopy cover data

ndvi <- (p2[[4]]-p2[[3]])/(p2[[4]]+p2[[3]])

#ser layer name to avoid confusion
names(ndvi) <- "ndvi"

#create a plot of the ndvi map with sample points on top
png(filename = "ndvi_map.png",  width = 6, height = 4, units = "in", res=300)

plot(ndvi)
points(gtree2, cex=gtree2$cc.pct/50, col="blue")

dev.off()
#to signify off for png plot

#extract ndvi values for each point
nt <- terra::extract(ndvi, gtree2, fun=mean, method='bilinear')

#plot ndvi vs canopy cover
dev.new(8,8)

plot(nt$ndvi, gtree2$cc.pct, pch=16, col= "blue", xlim=c(0,1))

dev.off()











