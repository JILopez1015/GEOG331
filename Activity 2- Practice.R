#GEOG331 Activity 2 Practice script
#JL 1/31/22

#make a vector of tree heights (m)
heights<- c(30,41,20,22)

# converting (m) to (cm)
heights_cm<- heights*100
heights_cm

#access specifically 2-4 tree heights
heights[2:4]

#Matrices

#assistance on matrices
help("matrix")

#setting up matrix w/ 2 col and fill in by rows
Mat.byrow<- matrix(c(1:6), ncol=2, byrow=TRUE)
Mat

#setting up matrix w/ 2 col and fill in by col
Mat.bycol<-matrix(c(1:6), ncol=2, byrow=FALSE)
Mat.bycol

#looking at Mat.bycol row 1, col 2
Mat.bycol[1,2]

#looking at all of col 1 Mat.bycol
Mat.bycol[,1]

#lookin at all of row 1 Mat.bycol
Mat.bycol[1,]
