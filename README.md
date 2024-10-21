# Lab3
Lab assignment 3 Spatial Autocorrelation Tutorial

---


## Introduction

Measuring spatial autocorrelation is a common task of many spatial analysts. Spatial autocorrelation is the measure of a variables relationship to itself through space. When a varible exhibts positive spatial autocorrelation it has a clustered pattern. Negative spatial autocorrelation indicates dispersed distribution and data sets with random distributions have no spatial autocorrelation. While this might be confusing at first it becomes easy to understand as soon as you imagine spatial autocorrelation in the real world. Positive spatial autocorrelation of income for example looks like wealthy neighbourhoods and poor neighbourhoods. In a city with no spatial autocorrelation of income, wealthy and poor people would be distributed evenly throughout every neighbourhood. As soon as you begin to imagine all the interesting implications of spatial autocorrelation in the world it becomes immediately obvious that we need tools to measure it. 

One of these tools is the morans I statistic. By comparing a given location to its neighbours the morans I statistic is able to measure the spatial auto correlation of a variable (Haining, 2001). There are both global and Local versions of the Morans I statistic both of which we will cover in this tutorial. 

For this tutorial we will be using 2 files one is a cvs file of Canadian Census data from 2016 and the other is a shp file of the associated census dessemination areas.  


Inorder to use a package durring an R session you must load the package into the session so it is available in your working directory to use. You can do this by "librarying" the package. Prior to this ensure all needed packages are downloaded you can use the code below to download and then library the needed packages. 

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
install.packages("knitr")
install.packages("tmap")
install.packages("spdep")
install.packages("raster")
install.packages("shinyjs")
install.packages("e1071")
install.packages("st")
install.packages("sda")
install.packages("entropy")
install.packages("corpcor")
install.packages("fdrtool")

#Load in libraries:
library("knitr")
library("tmap")
library("spdep")
library("raster")
library("shinyjs")
library("e1071")
library("sf")
library("sda")
library("corpcor")
library("entropy")
library("fdrtool")
library("st")

```
A working directory is file folder that contains all the files you need for a particular R project. After you set your working directory in R any files you reference R will assume by default are within the working directory. Once you've set the working diretory you can read the files you need into R as dataframes. The two files we need for this project are the cvs file containing the census data we are using and the shape file of census tract boundaries.  

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#Set working directory
dir <- "~/Documents/Geog418.2/Assignment3_Data"
setwd(dir)

#From the working dir read in the csv
csv <- read.csv("~/Documents/Geog418.2/Assignment3_Data/ucgsJQnBVLvP_data.csv")

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("~/Documents/Geog418.2/Assignment3_Data/lda_000a16a_e.shp") 


```
Before we go further we need to clean our data. This is an important step because it will help us better understand what data we are analysing and it will cut down on processing time. First we will assign new collumn names to our census data frame. You can see by opening the cvs file that currently the the collumns are numbered rather than named so it is difficult to understand what data we are working with. You can see our new collumn names in the code bellow which we will use to apply them. 


```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
          "CD name", "DA name", "Population", "Land area", 
          "Median total income", "Income Sample Size", "French Knowledge", 
          "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols
```
Next we will remove any irrelevant data which in this case is GEO UIDs that are less than 8 numbers long. 

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)
```
Following this we will merge our Census data with the spatial polygon dataframe. 

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)
```
Then we will select the city we are interested in and create a seperate subset that only includes data from this city.
```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#Subset for Victoria
Municp <- subset(census_DAs, census_DAs$CMANAME == "Victoria")
```
Next we will convert the value we are interested in comparing to income into a rate that will make it easier to spatially analyse. What was previously the number of respondants with French language knowlege now becomes the percentage of respondents with French language knowledge.
```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```
To further clean the data we need to remove any polygons where the value for either median total income or knowledge of French is NA. This is important becase missing data can skew the results of analyses. We use the following code to do this:

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$ PercFrench)),]
```


Now we can start with descriptive statistics for Median total Income and and French knowledge. The code bellow will calculate those values and produce a table for the results. 

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$"Median total income")
stdevIncome <- sd(Income_noNA$"Median total income")
skewIncome <- skewness(Income_noNA$"Median total income")

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$"PercFrench")
stdevFrench <- sd(French_noNA$"PercFrench")
skewFrench <- skewness(French_noNA$"PercFrench")

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```
Using our cleaned data we will create two maps of Median Income and French knowledge within census tracts of the City of Vicotria. Inorder to make mapes we need to use the tmap package we downloaded earlier. The tmaptools::palette_explorer tool found bellow is useful for visualising the various colour options for your map but you have to close the pallet explorer window before the rest of the code will run. To create the maps we use the functions tm_shape, tm_polygons and tm_layout. We create the title through the expression title =. The style of data classification and pallete used are selected using the expressions style = "" and palette="" respectively. 


```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "YlOrBr", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "YlOrBr", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```
![IncomeFrench](https://github.com/user-attachments/assets/843031c7-d27a-4d05-9d2d-05922a5f8eed)

## Neighbourhood matrix

Before we preform a Morans I statistic we need to decide how we will weight the neighbourhoods. Using the queens weight method all neighbourhoods sorrounding a particular neighbourhood are used in the calculation. Using the Rooks weight method only neighbourhoods directly adjacent to the neighbourhood in question are used. Its easier to understand this if you think about how a rook and a queen move on a chess board. A rook can only move up or down, left or right, while a queen may also move diagonally. Using the poly2nb() function in the ‘spdep’ package we establish neighbour links and ultimately decide which polygons are considered to be "neighbours". We can change our selection of weighting methods through directions like: ‘queen = TRUE’ or ‘queen = FALSE’."

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)

# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net) <- crs(Income_noNA)

#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(st_centroid(Income_noNA)))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(French_noNA))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(French_noNA))
crs(French.net2) <- crs(French_noNA)
```
To visulaize the differnt weighting methods we can create a map. The maps show which neighbourhoods are considered to be "neighbours" to other neighbourhoods under the queen and rook weighting methods respectively with the thrid pane of the map showing the different methods overlayed on top of each other. To create this map we use the tm_shape, tm_borders and tm_lines functions of the tmap package. The colours of the map can be manipulated anywhere the expression col='' is found. In order to arange 3 different maps within one figure we use the tmap_arrange function. 
```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Victoria census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net) + tm_lines(col='red')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='black') + 
  tm_shape(Income.net) + tm_lines(col='red', lwd = 2) +
  tm_shape(Income.net2) + tm_lines(col='blue', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```
![QueenRook](https://github.com/user-attachments/assets/fccfcac5-e45d-4ff0-82d6-f354f6ac014d)
After decideding whether to use queen or rook weighting methods we must decide bettween three types of weighting schemes. In this example we will use the W weighting scheme which employs a row standardized weighting scheme, with each neighbour given equal weights that sum to 1 (Cliff & Ord 1981;Tiefelsdorf et al., 1999). the other options are a B weighting scheme which gives a weight of 1 to all neighbours and a weight of 0 to every other polygon or a C weighting scheme which gives equal weight to all polygons regardless of whether or not they are neighbours (Cliff & Ord 1981;Tiefelsdorf et al., 1999). 

To create the weights matrix we will use the “nb2listw” function from the “spdep” library and apply it to the vri.nb variable we created earlier using the poly2nb() function. Its important to add "zero.policy=TRUE" so that our code will still function even if wee have a polygon with no neighbours. We select the W weighting scheme using the expression "style = W"

```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```


## Global Moran’s I

All of our previous work sets us up for calculating the Global Moran’s I statistic to measure spatial autocorrelation. Because this is a global statistic it provides us with a single value which describes the spatial autocorrelation of the entire study area. the equation looks like this:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here we are using the queen wighting method with the W weighting type. Where $x$ is the variable we are interested in (income or french knowledge), $x_i$ is its value at polygon (i) and $x_j$ is a neighbour to polygon (i). Both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$ are multiplied by The W spatial weighting scheme.Like other spatial statistics the denominator is used to standardize the resulting values. 

When interpreting the results it is important to know that positive spatial autocorrelation is indicated by relatively high values of I, and negative spatial autocorrelation is indicated by relatively low values of I. We can judge our Morans I value against our Expected Morans I value for a random distribution. The formula for this is as follows:

$$
E(I) = \frac{-1}{n-1}
$$


```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```
The calculations above produce the following results:
Morans I income= 0.531
Expected Morans I income = -0.0017
Variance for income= 0.00061
Morans I French= 0.391
Expected Morans I French = -0.0017
Variance for French= 0.00059

Because the Morans I values for both income and french knowledge are somewhat high relative to the negative expected values for random distribution this indicates there is some degree of spatial autocorrelation for both variables. 


```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

The range we calculate above tells us what values we would expect for a perfectly negatively spatially autocorrelated dataset (minRange) and a perfectly positively spatially autocorrelated dataset (maxrange). The results for income are -0.56 (min) and 1.04 (max). We can compare this to the Morans I  for income: 0.531. This value is closer to the high end of the range again indicating positive spatial autocorrelation. 

To determine whether these patterns are statistically significant we can conduct a Z-test. Here our null hypothesis is that our data is not significantly positively spatially autocorrelated, and the alternate hypothesis is that our data is significantly positively spatially autocorrelated. Using an $\alpha$ value of 0.05, if our Z-score falls below 1.96, we can say that the results are not signficiant and we fail to reject the null. A value greater than +1.96 would imply that our data is significantly positively spatially autocorrelated and the alternate hypothesis is correct.

Here is the code for calculating our Z score:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))

```
The results from these calculations are as follows: 
zFrench = 16.10
zIncome = 21.5
This indicates that both income and french knowledge are significantly positively spatially autocorrelated.

## Local spatial autocorrelation

While Global spatial autocorrelation tests provide a single value to decribe the spatial autocorrelation of an entire study area local spatial autocorrelation tests produce multiple values each describing the spatial autocorrelation of a small region of the study area. 

We can take our global Morans I formula and alter is slightly to make the local Morans I formula.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Here we we input our variable and weighting schemes again and apply the localmoran() function to produce our local spatial autocorrelation results. 


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$"Median total income", Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$"PercFrench", French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Because Local Morans I produces seperate values for each polygon within the dataset it is well suited to visulization through maps. Using the same methods we employed previously we will map our local morans I results. 

```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)

```
![Lisa](https://github.com/user-attachments/assets/872038e6-ee1d-4de2-b18c-dbbad075ec7f)

Grey polygons indicate random distribution, while blue indicate negative spatial autocorelation and red indicates positive spatial autocorrelation.


These maps are interesting but graphing is especially useful for captureing the trends of spatial autocorrelation within the data. Using the code below we can create scatter plots of our values.

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```
![Income](https://github.com/user-attachments/assets/3e993181-b701-4aa0-994c-340f9997201e)
![French](https://github.com/user-attachments/assets/dec1666c-53c8-4d40-a19d-3f104507e7ba)



 Diamonds represent statistically significant points, and the regression line shows the overall trend. Both plots demonstrate positive spatial autocorrelation. 




## Summary

In this tutorial we learned some basics for using R and experiemented with some statistical tests for spatial autocorrelation. After this tutorial you should be able to confidently navigate spatial autocorrelation in R and create helpful maps. I hope you found this tutorial useful! 

## References
Cliff, A. D., Ord, J. K. 1981 Spatial processes, Pion, p. 21; Bivand RS, Wong DWS 2018 Comparing implementations of global and local indicators of spatial association. TEST, 27(3), 716–748 doi:10.1007/s11749-018-0599-x

Haining, R. P. (2001). Spatial autocorrelation. International Encyclopedia of the Social &amp; Behavioral Sciences, 14763–14768. https://doi.org/10.1016/b0-08-043076-7/02511-0 

Tiefelsdorf, M., Griffith, D. A., Boots, B. 1999 A variance-stabilizing coding scheme for spatial link matrices, Environment and Planning A, 31, pp. 165–180; Kelejian, H. H., and I. R. Prucha. 2010. Specification and estimation of spatial autoregressive models with autoregressive and heteroskedastic disturbances. Journal of Econometrics, 157: pp. 53–67.
