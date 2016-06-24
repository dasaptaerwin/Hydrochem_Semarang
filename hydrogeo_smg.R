# INTRODUCTION

# METHOD
## Data collecting


## Data prep
df <- as.data.frame(read.csv("data_smg.csv")) # loading as data frame
head(df)             # checking header
is.na(df)            # checking NAs in df
df2 <- df[c(2,5:18)] # subsetting df, exclude var with NAs 
head(df2)
is.na(df2)           # checking NAs in df2
str(df2)             # checking data type in df2 
is.numeric(df2)      # checking data type in df2 
rownames(df2) <- df2$location # setting col location as row names
str(df2)             # checking data type in df2 

## Data analysis


# RESULTS AND DISCUSSIONS
## Compute correl matrix based on df2
df2 <- as.matrix(df2[,2:15])
install.packages("Hmisc")

### using Hmisc
library(Hmisc)
correl <- rcorr(df2)            # making correl matrix
correl

## visualise correl matrix
### using PerformanceAnalytics
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(df2, histogram=TRUE, pch=19)


## using ggcorrplot
install.packages("ggcorrplot")
library(ggcorrplot)
correl <- round(cor(df2), 1)    # rounding correl matrix
head(correl[, 1:14])            # view headers
p.mat <- cor_pmat(df2)          # compute p-values
head(p.mat[, 1:14])             # view headers
ggcorrplot(correl)              # making heatmap

## HCA
devtools::install_github("kassambara/factoextra")
install.packages("cluster")
library(cluster)
library(factoextra)

### k means method
kmeans(df2, centers, iter.max = 10, nstart = 1)
km2 <- kmeans(df2, 2, nstart = 25)
km3 <- kmeans(df2, 3, nstart = 25)
km2$cluster                     # extracting cluster number
km2$centers                     # extracting cluster means (or centers)
plotkm2 <- plot(df2, col = km2$cluster, pch = 19, frame = FALSE,
     main = "K-means with k = 2")
points(km2$centers, col = 1:2, pch = 8, cex = 3)

km3$cluster                     # extracting cluster number
km3$centers                     # extracting cluster means (or centers)
plotkm3 <- plot(df2, col = km3$cluster, pch = 19, frame = FALSE,
     main = "K-means with k = 3")
points(km3$centers, col = 1:2, pch = 8, cex = 3)

