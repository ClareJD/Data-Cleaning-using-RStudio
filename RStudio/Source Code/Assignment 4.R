#Load data

df<- read.csv("/cloud/project/HollywoodsMostProfitableStories.csv") 

#Take a look at the data

View(df) 

#Load library

install.packages("tidyverse") 

#Import library 

library(tidyverse) 

# Check data types

str(df)

# Check for missing values

colSums(is.na(df))

#Drop missing values

na.omit(df)

# Check to make sure that the column has been removed 

head(df) 

# Missing values is still present, changing sytnax to try a different method

df %>% drop_na()

# Missing values is still present, changing sytnax to try a different method

df[complete.cases(df), ]

# Missing values is still present, changing sytnax to try a different method

df <- df[rowSums(is.na(df)) == 0, ] 

#Check for duplicates 

dim(df[duplicated(df$Film),])[1] 

#Round off values to 2 places

df$Profitability <- round(df$Profitability ,digit=2) 

df$Worldwide.Gross <- round(df$Worldwide.Gross ,digit=2) 

#View(df)

dim(df) 

#Check for outliers using a boxplot

library(ggplot2)

#Create a boxplot that highlights the outliers

ggplot(df, aes(x=Profitability, y=Worldwide.Gross)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 1000))

#Remove outliers in 'Profitability'

Q1 <- quantile(df$Profitability, .25) 

Q3 <- quantile(df$Profitability, .75) 

IQR <- IQR(df$Profitability) 

no_outliers <- subset(df, df$Profitability> (Q1 - 1.5*IQR) & df$Profitability< (Q3 + 1.5*IQR)) 

dim(no_outliers) 

# Remove outliers in 'Worldwide.Gross' 

WGQ1 <- quantile(no_outliers$Worldwide.Gross, .25) 

WGQ3 <- quantile(no_outliers$Worldwide.Gross, .75) 

WGIQR <- IQR(no_outliers$Worldwide.Gross) 

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (WGQ1 - 1.5*WGIQR) & no_outliers$Worldwide.Gross< (WGQ3 + 1.5*WGIQR)) 

dim(df1) 

#Summary Statistics/Univariate Analysis

summary(df1)

#Bivariate analysis

#Create Scatterplot

ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#Create bar chart

ggplot(df1, aes(x=Year)) + geom_bar() 

#Export clean data

write.csv(df1, "clean_df.csv")

