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
### Cluster analysis
### PCA
### Multiregress
### Spatial

# RESULTS AND DISCUSSIONS
## Compute correl matrix based on df2
df2 <- as.matrix(df2[,2:15])

### using Hmisc
install.packages("Hmisc")
library(Hmisc)
correl <- rcorr(df2)            # making correl matrix
correl

## visualise correl matrix
### using PerformanceAnalytics
install.packages("PerformanceAnalytics")    
library(PerformanceAnalytics)
chart.Correlation(df2, histogram=TRUE, pch=19) # visual PA


## using ggcorrplot
install.packages("ggcorrplot")
library(ggcorrplot)
correl <- round(cor(df2), 1)    # rounding correl matrix
head(correl[, 1:14])            # view headers
p.mat <- cor_pmat(df2)          # compute p-values
head(p.mat[, 1:14])             # view headers
ggcorrplot(correl)              # making heatmap

## CLUSTER ANALYSIS
install.packages("devtools")
library(devtools)
install.packages("factoextra")
#install_github("kassambara/factoextra")
install.packages("cluster")
library(cluster)
library(factoextra)

### k means method
km2 <- kmeans(df2, 2, nstart = 25) # kmeans with 2 centers
km3 <- kmeans(df2, 3, nstart = 25) # kmeans with 3 centers
km2$cluster                        # extracting cluster number
km2$centers                        # extracting cluster means (or centers)
plotkm2 <- plot(df2, 
                col = km2$cluster, 
                pch = 19, 
                frame = T,
                main = "K-means with k = 2") # notes: need longer axis x 
points(km2$centers, 
       col = 1:2, 
       pch = 8, cex = 3)

km3$cluster                     # extracting cluster number
km3$centers                     # extracting cluster means (or centers)
plotkm3 <- plot(df2, 
                col = km3$cluster, 
                pch = 19, 
                frame = T,
                main = "K-means with k = 3")
points(km3$centers, 
       col = 1:2, 
       pch = 8, 
       cex = 3)

### evaluating cluster
df2 <- scale(df2)
head(df2)
fviz_nbclust(df2, 
             kmeans, method = "wss") +
             geom_vline(xintercept = 3, 
                        linetype = 2)   # determining optimal no cluster
km3.res <- kmeans(df2, 3, nstart = 25)  # running kmeans with 4 cluster
print(km3.res)                          # print output
fviz_cluster(km3.res, data = df2)       # vis output

pam.res <- pam(scale(df2), 3)           # running pam cluster with 3 cluster
pam.res$medoids                         # extract medoids
clusplot(pam.res, 
         main = "Cluster plot, k = 3", 
         color = TRUE)
plot(silhouette(pam.res),  col = 2:5) 
fviz_silhouette(silhouette(pam.res)) 
clarax <- clara(df2, 3, samples = 5)    # using clara method
fviz_cluster(clarax, 
             stand = FALSE, 
             geom = "point", 
             label=T,
             pointsize = 1)

### HCA
distdf2.res <- dist(df2, 
                    method = "euclidean")
hcadf2 <- hclust(distdf2.res, 
                 method = "complete")
plot(hcadf2, 
     hang = -1)       # dendogram vis
rect.hclust(hcadf2, 
            k = 3, 
            border = 2:4) # dendogram vis with grouping

### using nbclust pack to evaluate no of cluster
install.packages("NbClust") # for more precise no of cluster
library("NbClust")
resdf2.nb <- NbClust(df2, 
                     distance = "euclidean",
                     min.nc = 2, max.nc = 10, 
                     method = "complete", 
                     index ="gap") 
resdf2.nb                 # print the results
resdf2.nb$All.index       # All gap statistic values
resdf2.nb$Best.nc         # Best number of clusters
resdf2.nb$Best.partition  # calculate best partition
nbdf2 <- NbClust(df2, 
                 distance = "euclidean", 
                 min.nc = 2,
                 max.nc = 10, 
                 method = "complete", 
                 index ="all")
nbdf2
fviz_nbclust(nbdf2) + theme_minimal()
#dev.off()                # delete the '#' sign whenever 
                          # you want to clean the plot screen
distdf2.res <- dist(df2, 
                    method = "euclidean")
hcadf2 <- hclust(distdf2.res, 
                 method = "complete")
plot(hcadf2, 
     hang = -1)           # dendogram vis
rect.hclust(hcadf2, 
            k = 2, 
            border = 2:4) # dendogram vis with grouping

### Multiple regression for variable selection
#### using base package
# compare models
fit1 <- lm(tds ~ wat_level+
              elev+
              ph+
              k+
              ca+
              mg+
              na+
              so4+
              cl+
              hco3, data=df2)
step(fit1)                # stepwise regression
fit2 <- glm(tds ~ wat_level+
             elev+
             ph+
             k+
             ca+
             mg+
             na+
             so4+
             cl+
             hco3, data=df2) 
#family=binomial)
step(fit2,
     direction="backward",
     k=2)    



#### using MASS
library(MASS)
fit <- lm(tds ~ wat_level+
             elev+
             ph+
             k+
             ca+
             mg+
             na+
             so4+
             cl+
             hco3, data=df2)
lm$coefficients
step <- stepAIC(lm, direction="both")
step$anova # display results 

#### using randomforest
install.packages("randomForest")
library(randomForest)
rfModel <- randomForest(tds ~ wat_level+
                          elev+
                          ph+
                          k+
                          ca+
                          mg+
                          na+
                          so4+
                          cl+
                          hco3, data=df2, 
                         ntree = 500, 
                         mtry = 2,
                         importance = TRUE,
                         do.trace = 100, 
                         proximity=TRUE)
print(rfModel)
plot(rfModel)


(round(importance(rfModel), 3))
varImpPlot(rfModel,type=2)
dev.off()

#### using caret
install.packages("caret")
library(caret)
varImp(caret)
install.packages("pbkrtest")

#### using rpart
install.packages("rpart")
library(rpart)
install.packages("rattle")
library(rattle)					# Fancy tree plot
install.packages("rpart.plot")
library(rpart.plot)				# Enhanced tree plots
install.packages("RColorBrewer")
library(RColorBrewer)				# Color selection for fancy tree plot
install.packages("party")
library(party)					# Alternative decision tree algorithm
install.packages("partykit")
library(partykit)				# Convert rpart object to BinaryTree
library(caret)		
fit <- rpart(tds ~ wat_level+
               elev+
               ph+
               k+
               ca+
               mg+
               na+
               so4+
               cl+
               hco3, data=df2)
plot(fit)
text(fit)
prp(fit)			
fancyRpartPlot(fit)		
fit <- ctree(tds ~ wat_level+
               elev+
               ph+
               k+
               ca+
               mg+
               na+
               so4+
               cl+
               hco3, data=df2)
plot(fit, main="Conditional Inference Tree")

#### using mgcv
install.packages("mgcv")
library(mgcv)
fit.gam <- gam(tds ~ wat_level+
               elev+
               ph+
               k+
               ca+
               mg+
               na+
               so4+
               cl+
               hco3, data=df2,
           family=gaussian,
           select=TRUE,
           method="REML")                     # build gam model
summary(fit.gam)
plot(fit.gam,all.terms=TRUE, pages=1)         # plot gam results 
gam.check(fit.gam)                            # check gam model

as.data.frame(pred.tds <- predict(fit.gam))   # predict tds an save in pred.tds data frame. 
plot(jitter(residuals(fit.gam)) ~ predict(fit.gam)) # plot residuals vs prediction