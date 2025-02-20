---
title: 'Lesson 7: Advanced Raster Analysis'
author: "Ben Devries, Jan Verbesselt, Loïc Dutrieux"
date: "12 January, 2015"
output:
  html_document:
    theme: cosmo
    toc: yes
    toc_depth: 4
    number_sections: true
---




# Today's items

## ToDo in the morning

Go through the whole lesson and try to answer the questions below. 
We will address the questions in the lesson and discuss if needed.

## Learning outcomes of today:

Being able to:
- publish a documented report using R
- carry out a supervised and unsupervised classification on a series of raster layers
- construct a raster sieve using the `clump()` function
- deal with thematic (catergorical maps)

# Creating documented reports

A great way to avoid clicking and copy pasting is to create a script that combines text (explanation of your script, introduction, or something else) and a R script and creates a report as an html file.

You can create an `.html` from and `.Rmd` file. See the following link (https://rpubs.com/about/getting-started) for getting started and publishing your html to the www.

Give it a try and see here for some more information:
https://support.rstudio.com/hc/en-us/articles/200552086-Using-R-Markdown

If is possible that you obtain the following error on windows:


```r
Error in function (type, msg, asError = TRUE)  : 
  SSL certificate problem, verify that the CA cert is OK. Details:
error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
Calls: rpubsUpload ... <Anonymous> -> .postForm -> .Call -> <Anonymous> -> fun
Execution halted
```


This can be solved by the following [solution](https://stackoverflow.com/questions/22537180/error-while-publishing-in-r-pubs). More information about creating an `.Rprofile` file [here](http://www.statmethods.net/interface/customizing.html).

# Advanced Raster Analysis

## Introduction to Landsat data used here.

Since being released to the public, the Landsat data archive has become an invaluable tool for environmental monitoring. With a historical archive reaching back to the 1970's, the release of these data has resulted in a spur of time series based methods. In this tutorial, we will work with time series data from the Landsat 7 Enhanced Thematic Mapper (ETM+) sensor. Landsat scenes are delivered via the USGS as a number of image layers representing the different bands captured by the sensors. In the case of the Landsat 7 Enhanced Thematic Mapper (ETM+) sensor, the bands are shown in the figure below. Using different combination of these bands can be useful in describing land features and change processes.

<img src="figure/unnamed-chunk-3.png" title="Landsat 7 ETM+ bands" alt="Landsat 7 ETM+ bands" style="display: block; margin: auto;" />


Part of a landsat scene, including bands 2-4 are included in the data provide here. 
These data have been processed using the LEDAPS framework (http://ledaps.nascom.nasa.gov/), so the values contained in this dataset represent surface reflectance, scaled by 10000 (ie. divide by 10000 to get a reflectance value between 0 and 1).

We will begin exploring these data simply by downloading and visualizing them:

To download the data you can clone the Github repository (https://github.com/GeoScripting-WUR/AdvancedRasterAnalysis.git) to your local computer. All the required data sets are located within the data folder.


```r
## libraries
library(raster)
## load data
load("data/GewataB2.rda")
load("data/GewataB3.rda")
load("data/GewataB4.rda")

# check out the attributes
GewataB2
# some basic statistics using cellStats()
cellStats(GewataB2, stat = max)
cellStats(GewataB2, stat = mean)
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
```

<img src="figure/unnamed-chunk-4.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" style="display: block; margin: auto;" />


When we plot the histogram of the rasterBrick, the scales of the axes and the bin sizes are not equivalent, which could be problematic. This can be solved by adjusting these paramters in `hist()`, which requires extra consideration. The raster `hist()` function inherits arguments from the function of the same name from the `graphics` package. To view additional arguments, type:


```r
`?`(graphics::hist)
```


To ensure that our histograms are of the same scale, we should consider the `xlim`, `ylim` and `breaks` arguments.


```r
par(mfrow = c(1, 1))  # reset plotting window
hist(gewata, xlim = c(0, 5000), ylim = c(0, 750000), breaks = seq(0, 5000, by = 100))
```

<img src="figure/unnamed-chunk-6.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />


Note that the values of these bands have been rescaled by a factor of 10000. This is done for file storage considerations. For example, a value of 0.5643 stored as a float takes up more disk space than a value of 5643 stored as an integer. If you prefer reflectance values in their original scale (from 0 to 1), this can easily be done using raster algebra or `calc()`.

A scatterplot matrix can be helpful in exploring relationships between raster layers. This can be done with the `pairs()` function of the raster package, which (like `hist()`) is a wrapper for the same function found in the `graphics` packages.


```r
pairs(gewata)
```

<img src="figure/unnamed-chunk-7.png" title="plot of chunk unnamed-chunk-7" alt="plot of chunk unnamed-chunk-7" style="display: block; margin: auto;" />


Note that both `hist()` and `pairs()` compute histograms and scatterplots based on a random sample of raster pixels. The size of this sample can be changed with the argument `max.pixels` in either function.

Calling `pairs()` on a rasterBrick reveals potential correlations between the layers themselves. In the case of bands 2-4 of the gewata subset, we can see that band 2 and 3 (in the visual part of the EM spectrum) are highly correlated, while band 4 contains significant non-redundant information. 

**Given what we know about the location of these bands along the EM spectrum, how could these scatterplots be explained?** 

ETM+ band 4 (nearly equivalent to band 5 in the Landsat 8 OLI sensor) is situated in the near infrared (NIR) region of the EM spectrum and is often used to described vegetation-related features.

We observe a strong correlation between two of the Landsat bands of the gewata subset, but a very different distribution of values in band 4 (NIR). This distribution stems from the fact that vegetation reflects very highly in the NIR range, compared to the visual range of the EM spectrum. A commonly used metric for assessing vegetation dynamics, the normalized difference vegetation index (NDVI), explained in the previous lesson, takes advantage of this fact and is computed from Landsat bands 3 (visible red) and 4 (near infra-red).

In the previous lesson, we explored several ways to calculate NDVI, using direct raster algebra, `calc()` or `overlay()`. Since we will be using NDVI again later in this tutorial, let's calculated it again and store it in our workspace using `overlay()`.


```r
ndvi <- overlay(GewataB4, GewataB3, fun = function(x, y) {
    (x - y)/(x + y)
})
plot(ndvi)
```

<img src="figure/unnamed-chunk-8.png" title="plot of chunk unnamed-chunk-8" alt="plot of chunk unnamed-chunk-8" style="display: block; margin: auto;" />


Aside from the advantages of `calc()` and `overlay()` regarding memory usage, an additional advantage of these functions is the fact that the result can be written immediately to file by including the `filename = "..."` argument, which will allow you to write your results to file immediately, after which you can reload in subsequent sessions without having to repeat your analysis.

## Classifying raster data

One of the most important tasks in analysis of remote sensing image analysis is image classification. In classifying the image, we take the information contained in the various bands (possibly including other synthetic bands such as NDVI or principle components). In this tutorial we will explore two approaches for image classification: 
  - supervised (random forest) classification
  - unsupervised (k-means).

### Supervised classification: Random Forest

The Random Forest classification algorithm is an ensemble learning method that is used for both classification and regression. In our case, we will use the method for classification purposes. Here, the random forest method takes random subsets from a training dataset and constructs classification trees using each of these subsets. Trees consist of *branches* and *leaves*. 

Branches represent nodes of the decision trees, which are often thresholds defined for the measured (known) variables in the dataset. Leaves are the class labels assigned at the termini of the trees. Sampling many subsets at random will result in many trees being built. Classes are then assigned based on classes assigned by all of these trees based on a majority rule, as if each class assigned by a decision tree were considered to be a *vote*. 

Figure [1](fig:random) gives a simple demonstration of how the random forest method works in principle. For a an introduction to the random forest algorithm, see this [presentation](http://www.slideshare.net/0xdata/jan-vitek-distributedrandomforest522013).


<img src="figure/random.png" title="Fig 1. Schematic showing how the Random Forest method constructs classification trees from random subsets of a training dataset. Each tree determines the labels assigned based on the training dataset. Once all trees are assembled, classes are assigned to unknown pixels based on the class which receives the majority of votes based on all the decision trees constructed." alt="Fig 1. Schematic showing how the Random Forest method constructs classification trees from random subsets of a training dataset. Each tree determines the labels assigned based on the training dataset. Once all trees are assembled, classes are assigned to unknown pixels based on the class which receives the majority of votes based on all the decision trees constructed." style="display: block; margin: auto;" />


One major advantage of the Random Forest method is the fact that an *Out of the Bag* (OOB) error estimate and an estimate of variable performance are performed. For each classification tree assembled, a fraction of the training data are left out and used to compute the error for each tree by predicting the class associated with that value and comparing with the already known class. This process results in a confusion matrix, which we will explore in our analysis. In addition an importance score is computed for each variable in two forms: the mean decrease in accuracy for each variable, and the Gini impurity criterion, which will also be explored in our analysis.

We should first prepare the data on which the classification will be done. So far, we have prepared three bands from a ETM+ image in 2001 (bands 2, 3 and 4) as a rasterBrick, and have also calculated NDVI. In addition, there is a Vegetation Continuous Field (VCF) product available for the same period (2000).

For more information on the Landsat VCF product, see [here](http://glcf.umd.edu/data/landsatTreecover/). This product is also based on Landsat ETM+ data, and represents an estimate of tree cover (in %). Since this layer could also be useful in classifying land cover types, we will also include it as a potential covariate in the random forest classification.


```r
# load the data and check it out @ can we show a bit more about this data
# set - where does it come from @ can we provide an option to download it
# themselve for any location?
load("data/vcfGewata.rda")
vcfGewata
```

```
## class       : RasterLayer 
## dimensions  : 1177, 1548, 1821996  (nrow, ncol, ncell)
## resolution  : 30, 30  (x, y)
## extent      : 808755, 855195, 817635, 852945  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : vcf2000Gewata 
## values      : 0, 254  (min, max)
```

```r
plot(vcfGewata)
```

<img src="figure/unnamed-chunk-91.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />

```r
summary(vcfGewata)
```

```
##         vcf2000Gewata
## Min.                0
## 1st Qu.            32
## Median             64
## 3rd Qu.            75
## Max.              254
## NA's             8289
```

```r
hist(vcfGewata)
```

<img src="figure/unnamed-chunk-92.png" title="plot of chunk unnamed-chunk-9" alt="plot of chunk unnamed-chunk-9" style="display: block; margin: auto;" />


In the `vcfGewata` rasterLayer there are some values much greater than 100 (the maximum tree cover), which are flags for water, cloud or cloud shadow pixels. To avoid these values, we can assign a value of `NA` to these pixels so they are not used in the classification.


```r
vcfGewata[vcfGewata > 100] <- NA
plot(vcfGewata)
```

<img src="figure/unnamed-chunk-101.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />

```r
summary(vcfGewata)
```

```
##         vcf2000Gewata
## Min.                0
## 1st Qu.            32
## Median             64
## 3rd Qu.            75
## Max.              100
## NA's            13712
```

```r
hist(vcfGewata)
```

<img src="figure/unnamed-chunk-102.png" title="plot of chunk unnamed-chunk-10" alt="plot of chunk unnamed-chunk-10" style="display: block; margin: auto;" />


To perform the classification in R, it is best to assemble all covariate layers (ie. those layers contaning predictor variable values) into one rasterBrick object. In this case, we can simply append these new layers (NDVI and VCF) to our existing rasterBrick (currently consisting of bands 2, 3, and 4).

First, let's rescale the original reflectance values to their original scale. This step is not required for the RF classification, but it might help with the interpretation, if you are used to thinking of reflectance as a value between 0 and 1. (On the other hand, for very large raster bricks, it might be preferable to leave them in their integer scale, but we won't go into more detail about that here.)


```r
gewata <- calc(gewata, fun = function(x) x/10000)
# make a new raster brick of covariates by adding NDVI and VCF layers
covs <- addLayer(gewata, ndvi, vcfGewata)
plot(covs)
```


You'll notice that we didn't give our NDVI layer a name yet. It's good to make sure that the raster layer names make sense, so you don't forget which band is which later on. Let's change all the layer names (make sure you get the order right!)~.


```r
names(covs) <- c("band2", "band3", "band4", "NDVI", "VCF")
plot(covs)
```

<img src="figure/unnamed-chunk-12.png" title="plot of chunk unnamed-chunk-12" alt="plot of chunk unnamed-chunk-12" style="display: block; margin: auto;" />


For this exercise, we will do a very simple classification for 2001 using three classes: forest, cropland and wetland. While for other purposes it is usually better to define more classes (and possibly fuse classes later), a simple classification like this one could be useful, for example, to construct a forest mask for the year 2001.


```r
# load the training polygons
load("data/trainingPoly.rda")
# superimpose training polygons onto ndvi plot
plot(ndvi)
plot(trainingPoly, add = TRUE)
```

<img src="figure/unnamed-chunk-13.png" title="plot of chunk unnamed-chunk-13" alt="plot of chunk unnamed-chunk-13" style="display: block; margin: auto;" />


The training classes are labelled as string labels. For this exercise, we will need to work with integer classes, so we will need to first 'relabel' our training classes. There are several approaches that could be used to convert these classes to integer codes. In this case, we will first make a function that will reclassify the character strings representing land cover classes into integers based on the existing factor levels.


```r
# inspect the data slot of the trainingPoly object
trainingPoly@data
```

```
##    OBJECTID    Class
## 0         1  wetland
## 1         2  wetland
## 2         3  wetland
## 3         4  wetland
## 4         5  wetland
## 5         6   forest
## 6         7   forest
## 7         8   forest
## 8         9   forest
## 9        10   forest
## 10       11 cropland
## 11       12 cropland
## 12       13 cropland
## 13       14 cropland
## 14       15 cropland
## 15       16 cropland
```

```r
# the 'Class' column is actually an ordered factor type
trainingPoly@data$Class
```

```
##  [1] wetland  wetland  wetland  wetland  wetland  forest   forest  
##  [8] forest   forest   forest   cropland cropland cropland cropland
## [15] cropland cropland
## Levels: cropland forest wetland
```

```r
str(trainingPoly@data$Class)
```

```
##  Factor w/ 3 levels "cropland","forest",..: 3 3 3 3 3 2 2 2 2 2 ...
```

```r
# we can convert to integer by using the as.numeric() function, which takes
# the factor levels
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)
trainingPoly@data
```

```
##    OBJECTID    Class Code
## 0         1  wetland    3
## 1         2  wetland    3
## 2         3  wetland    3
## 3         4  wetland    3
## 4         5  wetland    3
## 5         6   forest    2
## 6         7   forest    2
## 7         8   forest    2
## 8         9   forest    2
## 9        10   forest    2
## 10       11 cropland    1
## 11       12 cropland    1
## 12       13 cropland    1
## 13       14 cropland    1
## 14       15 cropland    1
## 15       16 cropland    1
```


To train the raster data, we need to convert our training data to the same type using the ```rasterize()``` function. This function takes a spatial object (in this case a polygon object) and transfers the values to raster cells defined by a raster object. Here, we will define a new raster containing those values.


```r
# assign 'Code' values to raster cells (where they overlap)
classes <- rasterize(trainingPoly, ndvi, field = "Code")
# define a colour scale for the classes (as above) corresponding to:
# cropland, forest, wetland
cols <- c("orange", "dark green", "light blue")
# plot without a legend
plot(classes, col = cols, legend = FALSE)
# add a customized legend
legend("topright", legend = c("cropland", "forest", "wetland"), fill = cols, 
    bg = "white")
```





Note: there is a handy `progress="text"` argument, which can be passed to many of the raster package functions and can help to monitor processing. Try passing this argument to the`rasterize()` command above.

Our goal in preprocessing these data is to have a table of values representing all layers (covariates) with *known* values/classes. To do this, we will first need to create a version of our rasterBrick only representing the training pixels. Here the `mask()` function from the raster package will be very useful.




Now it's time to add all of these values to a `data.frame` representing all training data. 
This data.frame will be used as an input into the RandomForest classification function. 
We will use `getValues()` to extract all of the values from the layers of the rasterBrick.


```r
# extract all values into a matrix
valuetable <- getValues(trainingbrick)
```


If you print `valuetable` to the console, you will notice that alot of the rows are filled with `NA`. This is because all raster cell values have been taken, include those with NA values. We can get rid of these rows by using the `na.omit()` function.


```r
valuetable <- na.omit(valuetable)
```


Now we will convert to a `data.frame` and inspect the first and last 10 rows.


```r
valuetable <- as.data.frame(valuetable)
head(valuetable, n = 10)
```

```
##     band2  band3  band4   NDVI VCF class
## 1  0.0354 0.0281 0.2527 0.7999  77     2
## 2  0.0417 0.0301 0.2816 0.8069  74     2
## 3  0.0418 0.0282 0.2857 0.8203  72     2
## 4  0.0397 0.0282 0.2651 0.8077  71     2
## 5  0.0355 0.0263 0.2237 0.7896  77     2
## 6  0.0396 0.0281 0.2693 0.8110  75     2
## 7  0.0375 0.0300 0.2817 0.8075  76     2
## 8  0.0396 0.0263 0.2610 0.8169  76     2
## 9  0.0354 0.0263 0.2320 0.7964  76     2
## 10 0.0333 0.0263 0.2113 0.7786  73     2
```

```r
tail(valuetable, n = 10)
```

```
##        band2  band3  band4   NDVI VCF class
## 36211 0.0451 0.0293 0.2984 0.8212  76     2
## 36212 0.0406 0.0275 0.2561 0.8061  76     2
## 36213 0.0361 0.0293 0.2179 0.7629  75     2
## 36214 0.0406 0.0313 0.2222 0.7531  74     2
## 36215 0.0405 0.0313 0.2222 0.7531  73     2
## 36216 0.0406 0.0293 0.2646 0.8006  79     2
## 36217 0.0429 0.0293 0.2774 0.8089  70     2
## 36218 0.0451 0.0333 0.2900 0.7940  77     2
## 36219 0.0406 0.0293 0.2689 0.8035  81     2
## 36220 0.0429 0.0293 0.2434 0.7851  73     2
```


Now that we have our training dataset as a `data.frame`, let's convert the class column into a factor (since the values as integers don't really have a meaning).


```r
valuetable$class <- factor(valuetable$class, levels = c(1:3))
```


Now we have a convenient reference table which contains, for each of the three defined classes, all known values for all covariates. Let's visualize the distribution of some of these covariates for each class. To make this easier, we will create 3 different data.frames for each of the classes. This is just for plotting purposes, and we will not use these in the actual classification.


```r
val_crop <- subset(valuetable, class == 1)
val_forest <- subset(valuetable, class == 2)
val_wetland <- subset(valuetable, class == 3)

# 1. NDVI
par(mfrow = c(3, 1))
hist(val_crop$NDVI, main = "cropland", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 
    4000), col = "orange")
hist(val_forest$NDVI, main = "forest", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 
    4000), col = "dark green")
hist(val_wetland$NDVI, main = "wetland", xlab = "NDVI", xlim = c(0, 1), ylim = c(0, 
    4000), col = "light blue")
```

<img src="figure/unnamed-chunk-21.png" title="plot of chunk unnamed-chunk-21" alt="plot of chunk unnamed-chunk-21" style="display: block; margin: auto;" />

```r
par(mfrow = c(1, 1))
```



```r
# 2. VCF
par(mfrow = c(3, 1))
hist(val_crop$VCF, main = "cropland", xlab = "% tree cover", xlim = c(0, 100), 
    ylim = c(0, 7500), col = "orange")
hist(val_forest$VCF, main = "forest", xlab = "% tree cover", xlim = c(0, 100), 
    ylim = c(0, 7500), col = "dark green")
hist(val_wetland$VCF, main = "wetland", xlab = "% tree cover", xlim = c(0, 100), 
    ylim = c(0, 7500), col = "light blue")
```

<img src="figure/unnamed-chunk-22.png" title="plot of chunk unnamed-chunk-22" alt="plot of chunk unnamed-chunk-22" style="display: block; margin: auto;" />

```r
par(mfrow = c(1, 1))
```



```r
# 3. Bands 3 and 4 (scatterplots)
plot(band4 ~ band3, data = val_crop, pch = ".", col = "orange", xlim = c(0, 
    0.2), ylim = c(0, 0.5))
points(band4 ~ band3, data = val_forest, pch = ".", col = "dark green")
points(band4 ~ band3, data = val_wetland, pch = ".", col = "light blue")
legend("topright", legend = c("cropland", "forest", "wetland"), fill = c("orange", 
    "dark green", "light blue"), bg = "white")
```

<img src="figure/unnamed-chunk-23.png" title="plot of chunk unnamed-chunk-23" alt="plot of chunk unnamed-chunk-23" style="display: block; margin: auto;" />


*Try to produce the same scatterplot plot as in #3 looking at the relationship between other bands (e.g. bands 2 and 3, band 4 and VCF, etc.)*

We can see from these distributions that these covariates may do well in classifying forest pixels, but we may expect some confusion between cropland and wetland (although the individual bands may help to separate these classes). When performing this classification on large datasets and with a large amount of training data, now may be a good time to save this table using the `write.csv()` command, in case something goes wrong after this point and you need to start over again.

Now it is time to build the Random Forest model using the training data contained in the table of values we just made. For this, we will use the `randomForest` package in R, which is an excellent resource for building such types of models. Using the `randomForest()` function, we will build a model based on a matrix of predictors or covariates (ie. the first 5 columns of valuetable) related to the response (the `class` column of valuetable).


```r
# construct a random forest model covariates (x) are found in columns 1 to 5
# of valuetable training classes (y) are found in the 'class' column of
# valuetable caution: this step takes fairly long!  but can be shortened by
# setting importance=FALSE
library(randomForest)
modelRF <- randomForest(x = valuetable[, c(1:5)], y = valuetable$class, importance = TRUE)
```



```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```


Since the random forest method involves the building and testing of many classification trees (the 'forest'), it is a computationally expensive step (and could take alot of memory for especially large training datasets). When this step is finished, it would be a good idea to save the resulting object with the `save()` command. Any R object can be saved as an `.rda` file and reloaded into future sessions using `load()`.

The resulting object from the `randomForest()` function is a specialized object of class "randomForest", which is a large list-type object packed full of information about the model output. Elements of this object can be called and inspected like any list object.


```r
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
```


Since we set `importance=TRUE`, we now also have information on the statistical importance of each of our covariates which we can visualize using the `varImpPlot()` command.


```r
varImpPlot(modelRF)
```

<img src="figure/unnamed-chunk-26.png" title="plot of chunk unnamed-chunk-26" alt="plot of chunk unnamed-chunk-26" style="display: block; margin: auto;" />

The figure above shows the variable importance plots for a Random Forest model showing the mean decrease in accuracy (left) and the decrease in Gini Impurity Coefficient (right) for each variable.

These two plots give two different reports on variable importance (see `?importance()`). 

First, the mean decrease in accuracy indicates the amount by which the classification accuracy decreased based on the `OOB` assessment. Second, the Gini impurity coefficient gives a measure of class homogeneity. More specifically, the decrease in the Gini impurity coefficient when including a particular variable is shown in the plot. From [Wikipedia](http://en.wikipedia.org/wiki/Decision_tree_learning): "Gini impurity is a measure of how often a randomly chosen element from the set would be incorrectly labeled if it were randomly labeled according to the distribution of labels in the subset".

In this case, it seems that Gewata bands 3 and 4 have the highest impact on accuracy, while bands 3 and 2 score highest with the Gini impurity criterion. For especially large datasets, it may be helpful to know this information, and leave out less important variables for subsequent runs of the `randomForest()` function.

**Question**:
Since the VCF layer included NA's (which have also been excluded in our results) and scores relatively low according to the mean accuracy decrease criterion, try to construct an alternate random forest model as above, but excluding this layer. What effect does this have on the overall accuracy of the results (hint: compare the confusion matrices of the original and new outputs). What affect does leaving this variable out have on the processing time (hint: use `system.time()`)?

Now we can apply this model to the rest of the image and assign classes to all pixels. Note that for this step, the names of the raster layers in the input brick (here `covs`) must correspond exactly to the column names of the training table. We will use the `predict()` function from the `raster` package to predict class values based on the random forest model we have just constructed. This function uses a pre-defined model to predict values of raster cells based on other raster layers. This model can be derived by a linear regression, for example. In our case, we will use the model provided by the `randomForest()` function we applied earlier.






```r
# double check layer and column names to make sure they match
names(covs)
```

```
## [1] "band2" "band3" "band4" "NDVI"  "VCF"
```

```r
names(valuetable)
```

```
## [1] "band2" "band3" "band4" "NDVI"  "VCF"   "class"
```



```r
# predict land cover using the RF model
predLC <- predict(covs, model = modelRF, na.rm = TRUE)
```



```r
# plot the results recall: 1 = cropland, 2 = forest, 3 = wetland
cols <- c("orange", "dark green", "light blue")
plot(predLC, col = cols, legend = FALSE)
legend("bottomright", legend = c("cropland", "forest", "wetland"), fill = cols, 
    bg = "white")
```

<img src="figure/unnamed-chunk-30.png" title="plot of chunk unnamed-chunk-30" alt="plot of chunk unnamed-chunk-30" style="display: block; margin: auto;" />


Note that the `predict()` function also takes arguments that can be passed to `writeRaster()` (eg. `filename = ""`, so it is a good idea to write to file as you perform this step (rather than keeping all output in memory).

### Unsupervised classification k-means

In the absence of training data, an unsupervised classification can be carried out. Unsupervised classification methods assign classes based on inherent structures in the data without resorting to training of the algorithm. One such method, the k-means method, divides data into clusters based on Euclidean distances from cluster means in a feature space.

[More information on the theory behind k-means clustering](http://home.deib.polimi.it/matteucc/Clustering/tutorial_html/kmeans.html\#macqueen).

We will use the same layers (from the `covs` rasterBrick) as in the Random Forest classification for this classification exercise. As before, we need to extract all values into a `data.frame`. But this time we will extract all values, since we are not limited to a training dataset.


```r
valuetable <- getValues(covs)
head(valuetable)
```


Now we will construct a kmeans object using the `kmeans()` function. Like the Random Forest model, this object packages useful information about the resulting class membership. In this case, we will set the number of clusters to three, presumably corresponding to the three classes defined in our random forest classification.





```r
km <- kmeans(na.omit(valuetable), centers = 3, iter.max = 100, nstart = 10)
```



```r
# km contains the clusters (classes) assigned to the cells
head(km$cluster)
```

```
## [1] 2 2 1 1 1 1
```

```r
unique(km$cluster)  # displays unique values
```

```
## [1] 2 1 3
```



As in the random forest classification, we used the `na.omit()` argument to avoid any `NA` values in the valuetable (recall that there is a region of NAs in the VCF layer). These NAs are problematic in the `kmeans()` function, but omitting them gives us another problem: the resulting vector of clusters (from 1 to 3) is shorter than the actual number of cells in the raster. 

In other words: how do we know which clusters to assign to which cells? To answer that question, we need to have a kind of `mask` raster, indicating where the `NA` values throughout the `cov` rasterBrick are located.


```r
# create a blank raster with default values of 0
rNA <- setValues(raster(covs), 0)
# loop through layers of covs assign a 1 to rNA wherever an NA is enountered
# in covs
for (i in 1:nlayers(covs)) {
    rNA[is.na(covs[[i]])] <- 1
}
# convert rNA to an integer vector
rNA <- getValues(rNA)
```


We now have a vector indicating with a value of 1 where the `NA`'s in the `cov` brick are. Now that we know where the 'original' NAs are located, we can go ahead and assign the cluster values to a raster. 
At these `NA` locations, we will not assign any of the cluster values, instead assigning an `NA`.

First, we will insert these values into the original valuetable data.frame.


```r
# convert valuetable to a data.frame
valuetable <- as.data.frame(valuetable)
# if rNA is a 0, assign the cluster value at that position
valuetable$class[rNA == 0] <- km$cluster
# if rNA is a 1, assign an NA at that position
valuetable$class[rNA == 1] <- NA
```


Now we are finally ready to assign these cluster values to a raster. This will represent our final classified raster.


```r
# create a blank raster
classes <- raster(covs)
# assign values from the 'class' column of valuetable
classes <- setValues(classes, valuetable$class)
plot(classes, legend = FALSE, col = c("dark green", "orange", "light blue"))
```

<img src="figure/unnamed-chunk-35.png" title="plot of chunk unnamed-chunk-35" alt="plot of chunk unnamed-chunk-35" style="display: block; margin: auto;" />


These classes are much more difficult to interpret than those resulting from the random forest classification. We can see from the figure above that there is particularly high confusion between (what we might assume to be) the cropland and wetland classes. Clearly, with a good training dataset, a **supervised classification** can provide a reasonably accurate land cover classification. 

However, unsupervised classification methods like k-means are useful for study areas for which little to no *a priori* data exist.

**Question**
Assuming there are no training data available, is there a way we could improve the k-means classification performed in this example? Which one is computationally faster between random forest and k-means (hint: try the `system.time()` function)?

## Applying a raster sieve by clumping 

Although the land cover raster we created with the Random Forest method above is limited in the number of thematic classes it has, and we observed some confusion between wetland and cropland classes, it could be useful for constructing a forest mask (since that class performed quite well). To do so, we have to fuse (and remove) non-forest classes, and then clean up the remaining pixels by apply a sieve. To do this, we will make use of the `clump()` function (detecting patches of connected cells) in the raster package.





We now have a forest mask that can be used to isolate forest pixels for further analysis. Forest pixels (from the random forest classification) have a value of 1, and non-forest pixels have a value of `NA`.

For some applications, however, we may only be interested in larger forest areas. We may especially want to remove single forest pixels, as they may be a result of errors, or may not fit our definition of *forest*.

In this section, we will construct 2 types of sieves to remove these types of pixels, following 2 definitions of *adjacency*. In the first approach, the so-called *Queen's Case*, neighbours in all 8 directions are considered to be adjacent. If any pixel cell has no neighbours in any of these 8 directions, we will remove that pixel by assigning an `NA` value. 

First, we will use the `clump()` function in the raster package to identify clumps of raster cells. 
This function arbitrarily assigns an ID to these clumps.


```r
# Group raster cells into clumps based on the Queen's Case
if (!file.exists(fn <- "data/clumformask.grd")) {
    forestclumps <- clump(formask, directions = 8, filename = fn)
} else {
    forestclumps <- raster(fn)
}
plot(forestclumps)
```


When we inspect the frequency table with `freq()`, we can see the number of raster cells included in each of these clump IDs.


```r
# assign freqency table to a matrix
clumpFreq <- freq(forestclumps)
head(clumpFreq)
tail(clumpFreq)
```


We can use the `count` column of this frequency table to select clump `ID`s with only 1 pixel - these are the pixel ``islands" that we want to eventually remove from our original forest mask.


```r
# Coerce freq table to data.frame
clumpFreq <- as.data.frame(clumpFreq)
# which rows of the data.frame are only represented by one cell?
str(which(clumpFreq$count == 1))
# which values do these correspond to?
str(clumpFreq$value[which(clumpFreq$count == 1)])
# put these into a vector of clump ID's to be removed
excludeID <- clumpFreq$value[which(clumpFreq$count == 1)]
# make a new forest mask to be sieved
formaskSieve <- formask
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[forestclumps %in% excludeID] <- NA
# zoom in to a small extent to check the results Note: you can define your
# own zoom by using e <- drawExtent()
e <- extent(c(811744.8, 812764.3, 849997.8, 850920.3))
opar <- par(mfrow = c(1, 2))  # allow 2 plots side-by-side
plot(formask, ext = e, col = "dark green", legend = FALSE)
plot(formaskSieve, ext = e, col = "dark green", legend = FALSE)
```

<img src="figure/unnamed-chunk-39.png" title="plot of chunk unnamed-chunk-39" alt="plot of chunk unnamed-chunk-39" style="display: block; margin: auto;" />

```r
par(opar)  # reset plotting window
```


We have successfully removed all *island* pixels from the forest mask using the `clump()` function. We can adjust our sieve criteria to only only directly adjacent (NESW) neighbours: the so-called *Rook's Case*. To accomplish this, simply repeat the code above, but supply the argument `directions=4` when calling `clump()`.




We could take this approach further and apply a minimum mapping unit (MMU) to our forest mask. 

**Question:**
How could you adjust the above sieve to remove all forest pixels with area below 0.5 hectares? Consider the fact that Landsat pixels are 30m by 30m, and that one hectare is equal to 10000m<sup>2</sup>.

## Working with thematic rasters

As we have seen with the land cover rasters we derived using the random forest or k-means methods above, the values of a raster may be categorical, meaning they relate to a thematic class (e.g. 'forest' or 'wetland') rather than a quantitative value (e.g. NDVI or % Tree Cover). The raster dataset 'lulcGewata' is a raster with integer values representing Land Use and Land Cover (LULC) classes from a 2011 classification (using SPOT5 and ASTER source data).


```r
load("data/lulcGewata.rda")
# check out the distribution of the values
freq(lulcGewata)
```

```
##      value  count
## [1,]     1 396838
## [2,]     2  17301
## [3,]     3    943
## [4,]     4  13645
## [5,]     5 470859
## [6,]     6 104616
## [7,]    NA 817794
```

```r
hist(lulcGewata)
```

<img src="figure/unnamed-chunk-41.png" title="plot of chunk unnamed-chunk-41" alt="plot of chunk unnamed-chunk-41" style="display: block; margin: auto;" />


This is a raster with integer values between 1 and 6, but for this raster to be meaningful at all, we need a lookup or attribute table to identify these classes. A data.frame defining these classes is also included in the lesson repository:


```r
load("data/LUTGewata.rda")
LUTGewata
```

```
##   ID             Class
## 1  1          cropland
## 2  2            bamboo
## 3  3         bare soil
## 4  4 coffee plantation
## 5  5            forest
## 6  6           wetland
```


This `data.frame` represents a lookup table for the raster we just loaded. The `$ID` column corresponds to the values taken on by the lulc raster, and the `$Class` column describes the LULC classes assigned. In `R` it is possible to add a attribute table to a raster. In order to do this, we need to coerce the raster values to a factor from an integer and add a raster attribute table.


```r
lulc <- as.factor(lulcGewata)
# assign a raster attribute table (RAT)
levels(lulc) <- LUTGewata
lulc
```

```
## class       : RasterLayer 
## dimensions  : 1177, 1548, 1821996  (nrow, ncol, ncell)
## resolution  : 30, 30  (x, y)
## extent      : 808755, 855195, 817635, 852945  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=utm +zone=36 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
## data source : in memory
## names       : LULC2011_Gewata 
## values      : 1, 6  (min, max)
## attributes  :
##        ID    Class
##  from:  1 cropland
##  to  :  6  wetland
```


In some cases it might be more useulf to visualize only one class at a time. The `layerize()` function in the raster package does this by producing a rasterBrick object with each layer representing the class membership of each class as a boolean.


```r
classes <- layerize(lulc)
# layer names follow the order of classes in the LUT
names(classes) <- LUTGewata$Class
plot(classes, legend = FALSE)
```


Now each class is represented by a separate layer representing class membership of each pixel with 0's and 1's. If we want to construct a forest mask as we did above, this is easily done by extracting the fifth layer of this rasterBrick and replacing 0's with NA's.


```r
forest <- raster(classes, 5)
# is equivalent to:
forest <- classes[[5]]
# or (since the layers are named):
forest <- classes$forest
# replace 0's (non-forest) with NA's
forest[forest == 0] <- NA
plot(forest, col = "dark green", legend = FALSE)
```

<img src="figure/unnamed-chunk-45.png" title="plot of chunk unnamed-chunk-45" alt="plot of chunk unnamed-chunk-45" style="display: block; margin: auto;" />


# Today's summary

We learned about:

- Dealing with Landsat data
- How to:
    - perform a supervised and unsupervised classification using R scripts
    - clump and sieve connected cells within a raster
    - deal with thematic (i.e. categorical raster data) by assigning raster attribute tables.

# Excercise of today

Today we learned about how a `model` object in R can be used to predict raster values. This is a good example of how we can use the strengths of R's statistical packages in a spatial context. Of course, there are many more types of `model` objects than what is returned by `randomForest()`. The most basic `model` object is a product of a linear regression returned by `lm()`.

The VCF Tree Cover product that we used in the random forest classification is a product of a spatial model of tree cover, based several input data sources. Using Landsat band reflectance from one point in time, how well can we predict tree cover, using the existing VCF product as a response variable?

**Task**

Create a model object using `lm()` using Landsat band reflectance values as predictors for tree cover (VCF). Using the resulting `model` object, predict VCF values for the Gewata area.

The remaining Landsat bands for the Gewata area are available in the data folder of this lesson's repository:


```r
load("data/GewataB1.rda")
load("data/GewataB5.rda")
load("data/GewataB7.rda")
## band 6 (thermal infra-red) will be excluded from this exercise
```


- produce one or more plots that demonstrate the relationship between the Landsat bands and the VCF tree cover. What can you conclude from this/these plot(s)?
- create an `lm()` model and show a summary (e.g. using `summary()`) of the model object you created. Which predictors (bands) are probably most important in predicting tree cover?
- plot the predicted tree cover raster and compare with the original VCF raster.
- compute the RMSE between your predicted and the actual tree cover values ([hint](https://www.kaggle.com/wiki/RootMeanSquaredError))
- are the differences between the predicted and actual tree cover the same for all of the 3 classes we used for the random forest classfication? Using the training polygons from the random forest classification, calculate the RMSE separately for each of the classes and compare. *Hint* - see `?zonal()`.


# More information

- [Random Forest Classification: R versus Python!](http://blog.yhathq.com/posts/comparing-random-forests-in-python-and-r.html)

- https://rpubs.com/about/getting-started
- https://support.rstudio.com/hc/en-us/articles/200552086-Using-R-Markdown
- [All the Rstudio documentation](https://support.rstudio.com/hc/en-us/categories/200035113-Documentation)
