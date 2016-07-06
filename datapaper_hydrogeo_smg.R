# Data prep
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
#install.packages("dplyr")
#install.packages("piperR")

