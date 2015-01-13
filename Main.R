---
title: "test"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r}
summary(cars)
```

You can also embed plots, for example:

```{r, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

## libraries
library(raster)
## load data
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")

# check out the attributes
GewataB2
# some basic statistics using cellStats()
cellStats(GewataB2, stat=max)
cellStats(GewataB2, stat=mean)
# This is equivalent to:
maxValue(GewataB2)
# what is the maximum value of all three bands?
max(c(maxValue(GewataB2), maxValue(GewataB3), maxValue(GewataB4)))
# summary() is useful function for a quick overview
summary(GewataB2)

# put the 3 bands into a rasterBrick object to summarize together
gewata <- brick(GewataB2, GewataB3, GewataB4)
# 3 histograms in one window (automatic, if a rasterBrick is supplied)
hist(gewata)

par(mfrow = c(1, 1)) # reset plotting window
hist(gewata, xlim = c(0, 5000), ylim = c(0, 750000), breaks = seq(0, 5000, by = 100))
pairs(gewata)
ndvi <- overlay(GewataB4, GewataB3, fun=function(x,y){(x-y)/(x+y)},filename = 'data/ndvi1')
plot(ndvi)
load("data/vcfGewata.rda")
vcfGewata
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)
vcfGewata[vcfGewata > 100] <- NA
plot(vcfGewata)
summary(vcfGewata)
hist(vcfGewata)
gewata <- calc(gewata, fun=function(x) x / 10000)
# make a new raster brick of covariates by adding NDVI and VCF layers
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)
names(covs) <- c("band2", "band3", "band4", "NDVI", "VCF")
plot(covs)
# load the training polygons
load("data/trainingPoly.rda")
# superimpose training polygons onto ndvi plot
plot(ndvi)
plot(trainingPoly, add = TRUE)
trainingPoly@data
str(trainingPoly@data$Class)
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field='Code')
# define a colour scale for the classes (as above)
# corresponding to: cropland, forest, wetland
cols <- c("orange", "dark green", "light blue")
# plot without a legend
plot(classes, col=cols, legend=FALSE)
# add a customized legend
legend("topright", legend=c("cropland", "forest", "wetland"), fill=cols, bg="white")
covs <- mask(classes,covs)
trainingbrick <- addLayer(covs,classes)

names(trainingbrick) <- c("band2", "band3", "band4", "NDVI", "VCF","class")
plot(trainingbrick)
valuetable <- getValues(trainingbrick)
valuetable
valuetable <- na.omit(valuetable)
valuetable
valuetable <- as.data.frame(valuetable)
head(valuetable, n = 10)
tail(valuetable, n = 10)
valuetable$class <- factor(valuetable$class, levels = c(1:3))
val_crop <- subset(valuetable, class == 1)
val_forest <- subset(valuetable, class == 2)
val_wetland <- subset(valuetable, class == 3)

# 1. NDVI
par(mfrow = c(3, 1))
hist(val_crop$NDVI, main = "cropland", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 4000), col = "orange")
hist(val_forest$NDVI, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 4000), col = "dark green")
hist(val_wetland$NDVI, main = "wetland", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 4000), col = "light blue")
par(mfrow = c(1, 1))

# 2. VCF
par(mfrow = c(3, 1))
hist(val_crop$VCF, main = "cropland", xlab = "% tree cover", xlim = c(0, 100), ylim = c(0, 7500), col = "orange")
hist(val_forest$VCF, main = "forest", xlab = "% tree cover", xlim = c(0, 100), ylim = c(0, 7500), col = "dark green")
hist(val_wetland$VCF, main = "wetland", xlab = "% tree cover", xlim = c(0, 100), ylim = c(0, 7500), col = "light blue")

par(mfrow = c(1, 1))

# 3. Bands 3 and 4 (scatterplots)
plot(band2 ~ band3, data = val_crop, pch = ".", col = "orange", xlim = c(0, 0.2), ylim = c(0, 0.5))
points(band2 ~ band3, data = val_forest, pch = ".", col = "dark green")
points(band2 ~ band3, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend=c("cropland", "forest", "wetland"), fill=c("orange", "dark green", "light blue"), bg="white")

# construct a random forest model
# covariates (x) are found in columns 1 to 5 of valuetable
# training classes (y) are found in the 'class' column of valuetable
# caution: this step takes fairly long!
# but can be shortened by setting importance=FALSE
library(randomForest)
modelRF <- randomForest(x=valuetable[ ,c(1:5)], y=valuetable$class,
                        importance = TRUE)
                        
                        # inspect the structure and element names of the resulting model
modelRF
class(modelRF)
str(modelRF)
names(modelRF)
# inspect the confusion matrix of the OOB error assessment
modelRF$confusion
# to make the confusion matrix more readable
colnames(modelRF$confusion) <- c("cropland", "forest", "wetland", "class.error")
rownames(modelRF$confusion) <- c("cropland", "forest", "wetland")
modelRF$confusion

varImpPlot(modelRF)


# double check layer and column names to make sure they match
names(covs)

## [1] "band2" "band3" "band4" "NDVI"  "VCF"

names(valuetable)

## [1] "band2" "band3" "band4" "NDVI"  "VCF"   "class"

# predict land cover using the RF model
predLC <- predict(covs, model=modelRF, na.rm=TRUE)

# plot the results
# recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("orange", "dark green", "light blue")
plot(predLC, col=cols, legend=FALSE)
legend("bottomright", 
       legend=c("cropland", "forest", "wetland"), 
       fill=cols, bg="white")