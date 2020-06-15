# Assignment: 2.1 Assignment: 2014 American Community Survey
# Name: Myers,Cody
# Date: 2020-06-13

setwd("C:/Users/myers/OneDrive/Desktop/dsc520")

library(ggplot2)
library(pastecs)
library(moments)

data <- read.csv("acs-14-1yr-s0201.csv")

#1) Data elements and their types
#ID : chr
#ID2 : int
#Geography : chr
#PopGroupID : int
#POPGROUP.display.label : chr
#RacesReported : int
#HSDegree : num
#BachDegree : num

#2a)
str(data)
#'data.frame':	136 obs. of  8 variables:
#$ Id                    : chr  "0500000US01073" "0500000US04013" "0500000US04019" "0500000US06001" ...
#$ Id2                   : int  1073 4013 4019 6001 6013 6019 6029 6037 6059 6065 ...
#$ Geography             : chr  "Jefferson County, Alabama" "Maricopa County, Arizona" "Pima County, Arizona" "Alameda County, California" ...
#$ PopGroupID            : int  1 1 1 1 1 1 1 1 1 1 ...
#$ POPGROUP.display.label: chr  "Total population" "Total population" "Total population" "Total population" ...
#$ RacesReported         : int  660793 4087191 1004516 1610921 1111339 965974 874589 10116705 3145515 2329271 ...
#$ HSDegree              : num  89.1 86.8 88 86.9 88.8 73.6 74.5 77.5 84.6 80.6 ...
#$ BachDegree            : num  30.5 30.2 30.8 42.8 39.7 19.7 15.4 30.3 38 20.7 ...

#2b)
nrow(data)
#136

#2c)
ncol(data)
#8

#3)
ggplot(data, aes(HSDegree)) + geom_histogram(bins = 10) + xlab("Count of percentage of HS Degree recipeints") + ylab("People with a degree (%)") + ggtitle("Count of percentages of HS Degree recipients")

#4a) Yes, the data distribution is unimodal, we see a singular peak around the 90% mark. 

#4b) No, the data distribution is not symmetrical. The distrubtion leans right, to the higher bins.

#4c) Yes, the distribution is bell shaped, near the peak of the distribution, on either side is a 
#    steep drop, where the curve evens out.

#4d) No, the distribution is not normal as the peak is not centered with the bins.

#4e) Yes, the distribuition is skewed to the right.

#4f) 
ggplot(data, aes(HSDegree)) + geom_histogram(bins = 10, aes(y = ..density..)) + geom_density() + xlab("Count of percentage of HS Degree recipeints") + ylab("People with a degree (%)") + ggtitle("Count of percentages of HS Degree recipients")

#4g) 

#5)
ggplot(data, aes(HSDegree)) + geom_density()

#6a) 
#The distribution is not normal because there is no tail to the right side of the peak. We do not see the peak of the data
#centered.

#6b)
#Yes, the distribution is skewed to the right as the average of HS Degrees based by the location is fairly high.

#7)
stat.desc(data)
#         Id          Id2 Geography PopGroupID POPGROUP.display.label RacesReported     HSDegree
#  NA 1.360000e+02        NA        136                     NA  1.360000e+02 1.360000e+02
#nbr.null NA 0.000000e+00        NA          0                     NA  0.000000e+00 0.000000e+00
#nbr.na   NA 0.000000e+00        NA          0                     NA  0.000000e+00 0.000000e+00
#min      NA 1.073000e+03        NA          1                     NA  5.002920e+05 6.220000e+01
#max      NA 5.507900e+04        NA          1                     NA  1.011671e+07 9.550000e+01
#range    NA 5.400600e+04        NA          0                     NA  9.616413e+06 3.330000e+01
#sum      NA 3.649306e+06        NA        136                     NA  1.556385e+08 1.191800e+04
#median   NA 2.611200e+04        NA          1                     NA  8.327075e+05 8.870000e+01
#mean     NA 2.683313e+04        NA          1                     NA  1.144401e+06 8.763235e+01
#SE.mean  NA 1.323036e+03        NA          0                     NA  9.351028e+04 4.388598e-01
#CI.mean  NA 2.616557e+03        NA          0                     NA  1.849346e+05 8.679296e-01
#var      NA 2.380576e+08        NA          0                     NA  1.189207e+12 2.619332e+01
#std.dev  NA 1.542911e+04        NA          0                     NA  1.090508e+06 5.117941e+00
#coef.var NA 5.750024e-01        NA          0                     NA  9.529072e-01 5.840241e-02
#BachDegree
#nbr.val   136.0000000
#nbr.null    0.0000000
#nbr.na      0.0000000
#min        15.4000000
#max        60.3000000
#range      44.9000000
#sum      4822.7000000
#median     34.1000000
#mean       35.4610294
#SE.mean     0.8154527
#CI.mean     1.6127146
#var        90.4349886
#std.dev     9.5097313
#coef.var    0.2681741  

#8)
pop_sd <- sd(data$HSDegree)*sqrt((length(data$HSDegree)-1)/(length(data$HSDegree)))
pop_mean <- mean(data$HSDegree)
z <- (90 - pop_mean)/ pop_sd
#0.464327

skewness(data$HSDegree)
#-1.69341

kurtosis(data$HSDegree)
#7.462191

#Based upon the results we see above, our skewness tells us that we have an asymmetric distribtution and therefore
#do not have a normal distribution has previously described. For our kurtosis measurement, it fits the description
#of our distribution as we have one very long tail, where our data is fairly similar prior to the introduction of the peak.
#Another way to validate our previous claims and the distribution curve is to look at the z-scores. The z-score 
#calculated for this is 0.464327, which indicates that not all of our data points are identical to the mean of our distribution
#This is consistent with our findings as we see there are values outside of our peak.