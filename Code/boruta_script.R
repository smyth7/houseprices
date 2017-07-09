# https://www.kaggle.com/jimthompson/boruta-feature-importance-analysis/data/notebook

## ----setup,message=FALSE,warning=FALSE-----------------------------------
library(caret)
library(data.table)
library(Boruta)
library(plyr)
library(dplyr)
library(pROC)

ROOT.DIR <- "~/Documents/Work/houseprices"

ID.VAR <- "Id"
TARGET.VAR <- "SalePrice"


## ----DataRetrieval-------------------------------------------------------

# retrive data for analysis
sample.df <- read.csv(file.path(ROOT.DIR,"Data/Raw/train.csv"),stringsAsFactors = FALSE)

## ----QuickDataProfile----------------------------------------------------
# extract only candidate feture names
candidate.features <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.type <- sapply(candidate.features,function(x){class(sample.df[[x]])})
table(data.type)

print(data.type)

# deterimine data types
explanatory.attributes <- setdiff(names(sample.df),c(ID.VAR,TARGET.VAR))
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[[x]])})

# categorize data types in the data set?
unique.classes <- unique(data.classes)

attr.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.data.types) <- unique.classes


## ----PrepareData---------------------------------------------------------
# pull out the response variable
response <- sample.df$SalePrice

# remove identifier and response variables
sample.df <- sample.df[candidate.features]

# for numeric set missing values to -1 for purposes of the random forest run
for (x in attr.data.types$integer){
  sample.df[[x]][is.na(sample.df[[x]])] <- -1
}

for (x in attr.data.types$character){
  sample.df[[x]][is.na(sample.df[[x]])] <- "*MISSING*"
}


## ----borutarun-----------------------------------------------------------

set.seed(13)
bor.results <- Boruta(sample.df,response,
                   maxRuns=101,
                   doTrace=0)

## ----borutaresults1,echo=FALSE-------------------------------------------
cat("\nSummary of Boruta run:\n")
print(bor.results)

## ----echo=FALSE----------------------------------------------------------
cat("\n\nRelevant Attributes:\n")
getSelectedAttributes(bor.results)

## ----borutaplot,echo=FALSE,fig.width=9,fig.height=7----------------------
plot(bor.results)

## ----detailattrdump,echo=FALSE-------------------------------------------
cat("\n\nAttribute Importance Details:\n")
options(width=125)
arrange(cbind(attr=rownames(attStats(bor.results)), attStats(bor.results)),desc(medianImp))

