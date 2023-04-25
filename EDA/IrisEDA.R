      
        ### EDA in R Course ###

rm(list=ls())
pacman :: p_load(pacman,stats, dplyr)

# default IRIS ds
ds <- iris
str(ds)

sl <- ds$Sepal.Length
species <- ds$Species

    # 1. Data Summarization

  ## Central Tendencies

## Mean
mean(sl)

## Median
median(sl)

## Mode
mode <- function(x){
  ta <- table(x)
  tam <- max(ta)
  if(all(ta==tam))
    mod <- NA
  else
    if(is.numeric(x))
      mod <- as.numeric(names(ta)[ta==tam])
  else
    mod <- names(ta)[ta==tam]
  return(mod)
}

mode(sl)

    ## Measure of Variability

## Std. Dev
sd(sl)

## Variance
var(sl)

## IQR
quantile(sl)


    # 2. Data Viz

## numeric data -> histogram
hist(sl)

## categorical data -> barplot
table(species)
barplot(table(species))


    # 3. Data Normalization

## select all numeric variables in the dataset
ds_num <- select(ds, c(1,2,3,4))
str(ds_num)

## normalize ds (Z-Scores) - all variables on the same scale
Zscore_ds <- scale(ds_num)

## check all he variables converted to Z-Scores
View(Zscore_ds)
