#==============================================================================;
#PROGRAM DETAILS;
#==============================================================================;
#Program:   Readability_test.R;
#Version:   1.0;
#Author:    S. Brad Daughdrill;
#Date:      02.17.2013; 
#Purpose:   Orange Model of underlying option securities;
#==============================================================================;

#==============================================================================;
#INITIAL SETUP;
cat("SECTION: INITIAL SETUP", "\n")
#==============================================================================;

#String as factors is False -- used for read.csv
options(StringsAsFactors=FALSE)

options(install.packages.check.source = FALSE)

#Default maxprint option
#options(max.print=99999)   
#Maximum maxprint option
options(max.print=500)

#Memory limit default
#memory.limit(size=2047)
#Increase memory limit to 3000 mb (3 gb)
#memory.limit(size=3000)

#Limit History so that it never contains more than 50 lines
Sys.setenv(R_HISTSIZE=99999)


#Clear workspace
rm(list=ls(all=TRUE))

#Set working directory
setwd("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/R")  #Home
#setwd("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/R")  #Work

#Create output directory
output_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/R/",winslash = "\\", mustWork = NA)  #Home
#output_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/R/",winslash = "\\", mustWork = NA)  #Work

#Data data directory
data_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)  #Home
#data_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)  #Work

#Create function directory
function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/",winslash = "\\", mustWork = NA)  #Home
#function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/R/",winslash = "\\", mustWork = NA)  #Work

#Create package directory
#package_directory <- normalizePath("C:/Users/Brad/Documents/R/win-library/2.15/",winslash = "\\", mustWork = NA)  #Home
#package_directory <- normalizePath("C:/Users/bdaughdr/Documents/R/win-library/2.15/",winslash = "\\", mustWork = NA)  #Work

#Create treetag directory
treetag_directory <- normalizePath("C:/TreeTagger",winslash = "\\", mustWork = NA)  #Home


#==============================================================================;
#INPUT START DATE AND END DATE;
#==============================================================================;

#Start_File_Name  <- "Final_1999_QTR_1_2"
#End_File_Name    <- "Final_2012_QTR_3_4"

#==============================================================================;
#INPUT OBSERVATIONS TO KEEP (MONTHS)
#==============================================================================;

#observations_to_keep   <- 84   #CHANGE TO 36 or 60 as needed (or other #)

#==============================================================================;
#LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
#==============================================================================;

#First-time install libraries
#source(file=paste(function_directory,"install_libraries.R",sep=""),echo=FALSE)

#Load add-on packages
library(cwhmisc)        #Package for Miscellaneous Functions for maths, plotting, printing, statistics, strings, and tools
#library(dynlm)          #Package for Dynamic linear models and time series regression
library(fastmatch)      #Package for Fast match() function
library(foreign)        #Package for Reading Data from Minitab,S,SAS,SPSS,...
library(formatR)        #Package for ..........
library(gdata)           #Package for ..........
#library(gmodels)        #Package for Various R programming tools for model fitting
#library(googleVis)      #Interface between R and the Google Visualisation API
library(gtools)         #Package for ..........
#library(gWidgets)       #Package for gWidgets API for building toolkit-independent, interactive GUIs
#library(gWidgetstcltk)  #Package for Toolkit implementation of gWidgets for tcltk package (or gWidgetsGtk2 or gWidgetsrJava or gWidgetsWWW or gWidgetsQt)
library(Hmisc)          #Package for data imputation
library(koRpus)         #Package for ..........
#library(lmtest)         #Package for testing Linear Regression Models
library(mitools)        #Package for imputation of missing data
#library(PERregress)     #Package for Regression Functions and Datasets
#library(plm)            #Package for creating Linear Models for Panel Data
library(plyr)           #Package for splitting, applying and combining data
#library(quantmod)       #Package for Quantitative Financial Modelling Framework
library(R.oo)           #Package for trim() function
#library(RODBC)          #Package for ODBC Database Access in R
#library(rpanel)         #Package for Simple Interactive Controls with the tcltk Package.
#library(RSiteSearch)    #Package for RSiteSearch
#library(sandwich)       #Package for Robust Covariance Matrix Estimators
library(sqldf)          #Package for performing SQL selects on R Data Frames
library(stringr)        #Package for additional string functions
#library(tcltk)          #Package for Tcl/Tk Interface
#library(tseries)        #Package for time series analysis & computational finance
library(XML)
#library(zoo)            #Package for performing time-series analysis


#==============================================================================;
#FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
#==============================================================================;

#External Functions
source(file=paste(function_directory,"functions.R",sep=""),echo=FALSE)

# import_across_row_function <- function(x,data_temp, file_temp,row_NA_first_temp,temp_row_count,temp_headers) {
#   
#   file_temp <- as.character(file_temp)
#   
#   cat("COLUMN ",as.numeric(grep(x,sample_data_cols2)),"\n")
#   cat("ROW ",row_NA_first_temp," IS FIRST EMPTY ROW", "\n")
#   #cat("X ",x, "\n")
#   #cat("file_temp ",file_temp, "\n")
#   #cat("temp_row_count ",temp_row_count, "\n")
#   #cat("temp_headers ",temp_headers, "\n")
#   #cat("data_temp col yr ",data_temp[,names(data_temp)==x], "\n")
#   
#   if (row_NA_first_temp==1)
#   {
#     #cat("IF", "\n")
#     
#     xxx_col_num  <- as.numeric(match(x,temp_headers))
#     
#     xxx1 <- as.character(read.csv(file=file_temp,header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:temp_row_count,xxx_col_num])
#     xxx2 <- as.character(data_temp[(temp_row_count+1):nrow(data_temp),names(data_temp)==x])
#     data_temp[,names(data_temp)==x] <- append(xxx1,xxx2)
#     
#   } else if (row_NA_first_temp>1)
#   {
#     #cat("ELSE", "\n")
#     
#     #temp <- data_temp[1:row_NA_first_temp,]
#     
#     
#   } else
#   {
#     cat("ERROR!!", "\n")
#     
#   }
#   
#   return(data_temp)
# }

#Function to merge across rows
merge_cols_function <- function(col_one,col_two,separator){
  if (is.na(col_one) & is.na(col_two))
  {
    #cat("Both NA", "\n")
    return(NA)
    
  } else if (!(is.na(col_one)) & is.na(col_two))
  {
    #cat("Col_two NA", "\n")
    return(col_one)
    
  } else if (is.na(col_one) & !(is.na(col_two)))
  {
    #cat("Col_one NA", "\n")
    return(col_two)
    
  } else
  {
    #cat("Neither NA", "\n")
    return(paste(col_one, col_two, sep = separator))
    
  }
  #return(temp)
}

#Function to add path to file names
format_function <- function(temp_df,temp_df_col){
  
  for (i in 1:ncol(temp_df))
  {
    #i <- 1
    
    if (names(temp_df)[i] %in% temp_df_col[temp_df_col[,names(temp_df_col)=="isnum"]==1,names(temp_df_col)=="colnames"])
    {
      #cat("Is a number", "\n")
      temp_df[,i] <- unknownToNA(temp_df[,i], unknown=c("","NA","NA_character_","NA_Real_",  NA),force=TRUE)
      temp_df[,i] <- as.numeric(temp_df[,i])
      
    } else if (names(temp_df)[i] %in% temp_df_col[temp_df_col[,names(temp_df_col)=="ischar"]==1,names(temp_df_col)=="colnames"])
    {
      #cat("Is a character", "\n")
      temp_df[,i] <- unknownToNA(temp_df[,i], unknown=c("","NA","NA_character_", "NA_Real_",  NA),force=TRUE)
      temp_df[,i] <- as.character(temp_df[,i])
      
    } else if (names(temp_df)[i] %in% temp_df_col[temp_df_col[,names(temp_df_col)=="isdate"]==1,names(temp_df_col)=="colnames"])
    {
      temp_df[,i] <- unknownToNA(temp_df[,i], unknown=c("","NA","NA_character_","NA_Real_",  NA),force=TRUE)
      
      #if (inherits(temp_df[,i], "Date"))
      #{
      #cat("Is already date", "\n") 
      #year    <- as.numeric(format(temp_df[,1], "%Y"))
      #month   <- as.numeric(format(temp_df[,1], "%m"))
      
      #} else
      #{
      #cat("Is not already date", "\n") 
      temp_df[,i] <- as.character(temp_df[,i])
      temp_df[,i] <- as.Date(temp_df[,i],format = "%m/%d/%Y")
      #temp_df[,i] <- as.Date(temp_df[,i],format = "%Y-%m-%d")
      
      #}
      
    } else if (names(temp_df)[i] %in% temp_df_col[temp_df_col[,names(temp_df_col)=="isfactor"]==1,names(temp_df_col)=="colnames"])
    {
      #cat("Is a factor", "\n")
      temp_df[,i] <- unknownToNA(temp_df[,i], unknown=c("","NA","NA_character_", "NA_Real_",  NA),force=TRUE)
      temp_df[,i] <- as.factor(temp_df[,i])
      
    } else
    {
      #cat("ERROR!!", "\n")
      
    }
  }
  
  return(temp_df)
}
create_stats_function <- function(temp_df,count_col){
  
  return(data.frame(temp_df, uTotal=nrow(temp_df),gTotal=sum(temp_df[,names(temp_df)==count_col]),Total_Percentage=((temp_df[,names(temp_df)==count_col])/(sum(temp_df[,names(temp_df)==count_col])))))
}
create_stats_yr_function <- function(temp_df,count_col,split_val){
  
  #temp_df <- sqldf(query_global_year_word_grand_percentiles)
  #count_col <- "Count_word_grand"
  #split_val <- "yr"
  
  temp <- data.frame(temp_df, uTotal=as.numeric(NA),gTotal=as.numeric(NA),Total_Percentage=as.numeric(NA))
  temp[,names(temp)=="uTotal"] <- as.numeric(ddply(temp_df, split_val, function(x) data.frame(x,uTotal=nrow(x)))$uTotal)
  temp[,names(temp)=="gTotal"] <- as.numeric(ddply(temp_df, split_val, function(x) data.frame(x,gTotal=sum(x[,names(x)==count_col])))$gTotal)
  temp[,names(temp)=="Total_Percentage"] <- as.numeric(ddply(temp_df, split_val, function(x) data.frame(x,Total_Percentage=(x[,names(x)==count_col]/sum(x[,names(x)==count_col]))))$Total_Percentage)
  return(temp)
}


#==============================================================================;
#PREALLOCATE DATA;
cat("SECTION: PREALLOCATE DATA", "\n")
#==============================================================================;

ptm <- proc.time()

#Create base column table
temp_data_cols <- as.data.frame(matrix(NA, ncol = 7, nrow = 200))
colnames(temp_data_cols) <- c("order","isnum","ischar","isdate","isfactor","colnames","desc")
temp_data_cols[,1] <- as.numeric(temp_data_cols[,1])
temp_data_cols[,2] <- as.numeric(temp_data_cols[,2])
temp_data_cols[,3] <- as.numeric(temp_data_cols[,3])
temp_data_cols[,4] <- as.numeric(temp_data_cols[,4])
temp_data_cols[,5] <- as.numeric(temp_data_cols[,5])
temp_data_cols[,6] <- as.character(temp_data_cols[,6])
temp_data_cols[,7] <- as.character(temp_data_cols[,7])

#Files table
files_cols_count <- 2
files_cols <- temp_data_cols[1:files_cols_count,]
files_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filename",stringsAsFactors = FALSE)
files_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filepath",stringsAsFactors = FALSE)
files <- as.data.frame(matrix(NA, ncol = files_cols_count, nrow = 10))
colnames(files) <- files_cols[,6]

#Create cutoffs (FIX THIS)
percentiles <- as.data.frame(matrix(NA, ncol = 8, nrow = 3))
colnames(percentiles) <- c("Confidence_Level","Confidence_Pct","Confidence_lbl","Significance_Level","Significance_Pct","Significance_lbl","Column_lbl","Column_DV")
percentiles[,names(percentiles)=="Confidence_Level"] <- format(as.double(c(0.990,0.950,0.900)), digits=3)
percentiles[,names(percentiles)=="Confidence_Pct"] <- as.double(percentiles[,names(percentiles)=="Confidence_Level"])*100
percentiles[,names(percentiles)=="Confidence_lbl"] <- formatC(percentiles[,names(percentiles)=="Confidence_Pct"],format="f", digits=1,width=4,  flag="0")
percentiles[,names(percentiles)=="Significance_Level"] <- 1-as.double(percentiles[,names(percentiles)=="Confidence_Level"])
percentiles[,names(percentiles)=="Significance_Pct"] <- as.double(percentiles[,names(percentiles)=="Significance_Level"])*100
percentiles[,names(percentiles)=="Significance_lbl"] <- formatC(percentiles[,names(percentiles)=="Significance_Pct"], format="f",digits=1, width=5,  flag="0")
percentiles[,names(percentiles)=="Confidence_lbl"] <-  paste(gsub(pattern="\\.", replacement="", x=percentiles[,names(percentiles)=="Confidence_lbl"]),"pct",sep="")
percentiles[,names(percentiles)=="Significance_lbl"] <- paste(gsub(pattern="\\.", replacement="", x=percentiles[,names(percentiles)=="Significance_lbl"]),"pct",sep="")
percentiles[,names(percentiles)=="Column_lbl"] <- paste("Word_Cutoff_",percentiles[,names(percentiles)=="Confidence_lbl"],sep="")
percentiles[,names(percentiles)=="Column_DV"] <- paste("Word_DV_",percentiles[,names(percentiles)=="Confidence_lbl"],sep="")
Word_Cutoff_vector <- grep(pattern = 'Word_Cutoff_*', percentiles[,names(percentiles)=="Column_lbl"], ignore.case = FALSE, perl = FALSE, value = TRUE)
Word_DV_vector <- grep(pattern = 'Word_DV_*', percentiles[,names(percentiles)=="Column_DV"], ignore.case = FALSE, perl = FALSE, value = TRUE)

#Readability columns table
readbl_vars_cols_count <- 3
readbl_vars_cols <- temp_data_cols[1:readbl_vars_cols_count,]
readbl_vars_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="column",stringsAsFactors = FALSE)
readbl_vars_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="suffix",stringsAsFactors = FALSE)
readbl_vars_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token_table",stringsAsFactors = FALSE)
readbl_vars <- as.data.frame(matrix(NA, ncol = readbl_vars_cols_count, nrow = 2))
colnames(readbl_vars) <- readbl_vars_cols[,6]

#Readability statistics table
readbl_all_df_cols_count <- 5
readbl_all_df_cols <- temp_data_cols[1:readbl_all_df_cols_count,]
readbl_all_df_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index",stringsAsFactors = FALSE)
readbl_all_df_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="flavour",stringsAsFactors = FALSE)
readbl_all_df_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="raw",stringsAsFactors = FALSE)
readbl_all_df_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="grade",stringsAsFactors = FALSE)
readbl_all_df_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="age",stringsAsFactors = FALSE)
readbl_all_df <- as.data.frame(matrix(NA, ncol = readbl_all_df_cols_count, nrow = 44))
colnames(readbl_all_df) <- readbl_all_df_cols[,6]

#Sample data input columns
sample_data_input_cols_count <- 44
sample_data_input_cols <- temp_data_cols[1:sample_data_input_cols_count,]
sample_data_input_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="cusip8",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="crsp_fundno",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="chgdt",desc="input",stringsAsFactors = FALSE)             #DATE
sample_data_input_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="chgenddt",desc="input",stringsAsFactors = FALSE)          #DATE
sample_data_input_cols[6,] <- data.frame(order=6,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="crsp_portno",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[7,] <- data.frame(order=7,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="crsp_cl_grp",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="fund_name",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[9,] <- data.frame(order=9,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="nasdaq",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[10,] <- data.frame(order=10,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ncusip",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[11,] <- data.frame(order=11,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="first_offer_dt",desc="input",stringsAsFactors = FALSE)  #DATE
sample_data_input_cols[12,] <- data.frame(order=12,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgmt_name",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[13,] <- data.frame(order=13,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgmt_cd",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[14,] <- data.frame(order=14,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgr_name",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[15,] <- data.frame(order=15,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgr_dt",desc="input",stringsAsFactors = FALSE)          #DATE
sample_data_input_cols[16,] <- data.frame(order=16,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="adv_name",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[17,] <- data.frame(order=17,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="open_to_inv",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[18,] <- data.frame(order=18,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="retail_fund",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[19,] <- data.frame(order=19,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="inst_fund",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[20,] <- data.frame(order=20,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="m_fund",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[21,] <- data.frame(order=21,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index_fund_flag",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[22,] <- data.frame(order=22,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="vau_fund",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[23,] <- data.frame(order=23,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="et_flag",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[24,] <- data.frame(order=24,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="fyear",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[25,] <- data.frame(order=25,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="accession_num",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[26,] <- data.frame(order=26,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="GVKEY",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[27,] <- data.frame(order=27,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CIK",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[28,] <- data.frame(order=28,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="FDATE",desc="input",stringsAsFactors = FALSE)           #DATE
sample_data_input_cols[29,] <- data.frame(order=29,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="FINDEXDATE",desc="input",stringsAsFactors = FALSE)      #DATE
sample_data_input_cols[30,] <- data.frame(order=30,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="LINDEXDATE",desc="input",stringsAsFactors = FALSE)      #DATE
sample_data_input_cols[31,] <- data.frame(order=31,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Form",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[32,] <- data.frame(order=32,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CoName",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[33,] <- data.frame(order=33,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Fname",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[34,] <- data.frame(order=34,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="PortName",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[35,] <- data.frame(order=35,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[36,] <- data.frame(order=36,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[37,] <- data.frame(order=37,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[38,] <- data.frame(order=38,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[39,] <- data.frame(order=39,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[40,] <- data.frame(order=40,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_strategy_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[41,] <- data.frame(order=41,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[42,] <- data.frame(order=42,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Process_IS",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[43,] <- data.frame(order=43,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Process_R",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[44,] <- data.frame(order=44,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="File",desc="input",stringsAsFactors = FALSE)

#Sample data statistics columns
sample_data_statistics_cols_count <- 27
sample_data_statistics_cols <- temp_data_cols[1:sample_data_statistics_cols_count,]
sample_data_statistics_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="lines",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="sentences",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[3,] <- data.frame(order=3,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="words",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="all_chars",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="chars_no_space",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[6,] <- data.frame(order=6,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="letters_only",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[7,] <- data.frame(order=7,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="digits",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[8,] <- data.frame(order=8,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="punct",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[9,] <- data.frame(order=9,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="conjunctions",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[10,] <- data.frame(order=10,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="prepositions",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[11,] <- data.frame(order=11,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="pronouns",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[12,] <- data.frame(order=12,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="foreign",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[13,] <- data.frame(order=13,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="num_syll",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[14,] <- data.frame(order=14,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="normalized_space",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[15,] <- data.frame(order=15,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Flesch_Kincaid",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[16,] <- data.frame(order=16,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="ARI",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[17,] <- data.frame(order=17,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Coleman_Liau",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[18,] <- data.frame(order=18,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="SMOG",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[19,] <- data.frame(order=19,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="FOG_hard_words",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[20,] <- data.frame(order=20,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="TTR",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[21,] <- data.frame(order=21,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="sntc_per_word",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[22,] <- data.frame(order=22,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="avg_sentc_length",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[23,] <- data.frame(order=23,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="avg_word_length",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[24,] <- data.frame(order=24,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="avg_syll_word",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[25,] <- data.frame(order=25,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="sntc_per100",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[26,] <- data.frame(order=26,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="syll_per100",desc="stats",stringsAsFactors = FALSE)
sample_data_statistics_cols[27,] <- data.frame(order=27,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="lett_per100",desc="stats",stringsAsFactors = FALSE)
#Double this data.frame because investment objective/strategy & principal_risks
sample_data_statistics_cols <- rbind(sample_data_statistics_cols,sample_data_statistics_cols)
#Reorder order column
sample_data_statistics_cols[,1] <- rep(1:nrow(sample_data_statistics_cols), 1)
#Add suffix to column headers
sample_data_statistics_cols[1:27,6] <-  unlist(mapply(merge_cols_function,col_one=sample_data_statistics_cols[1:27,6],col_two="_iois",separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))
sample_data_statistics_cols[28:nrow(sample_data_statistics_cols),6] <- unlist(mapply(merge_cols_function,col_one=sample_data_statistics_cols[28:nrow(sample_data_statistics_cols),6],col_two="_pr",separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))

#Create sample data table
sample_data_all_cols_count <- 98
#Sample data all columns
sample_data_all_cols <- rbind(sample_data_input_cols,sample_data_statistics_cols)
#Reorder order column
sample_data_all_cols[,1] <- rep(1:nrow(sample_data_all_cols), 1)
sample_data_all <- as.data.frame(matrix(NA, ncol = sample_data_all_cols_count, nrow = 400000))
colnames(sample_data_all) <- sample_data_all_cols[,6]

#Tokens table
tokens_all_cols_count <- 5
tokens_all_cols <- temp_data_cols[1:tokens_all_cols_count,]
tokens_all_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ID",stringsAsFactors = FALSE)
tokens_all_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",stringsAsFactors = FALSE)
tokens_all_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token",stringsAsFactors = FALSE)
tokens_all_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="desc",stringsAsFactors = FALSE)
tokens_all_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Remove",stringsAsFactors = FALSE)
readbl_vars[,3] <- c("tokens_all_ios_f","tokens_all_pr_f")
tokens_all_ios_f <- as.data.frame(matrix(NA, ncol = tokens_all_cols_count, nrow = 4000000))
tokens_all_pr_f <- as.data.frame(matrix(NA, ncol = tokens_all_cols_count, nrow = 4000000))
tokens_all_temp <- as.data.frame(matrix(NA, ncol = tokens_all_cols_count, nrow = 4000000))
colnames(tokens_all_ios_f) <- tokens_all_cols[,6]
colnames(tokens_all_pr_f) <- tokens_all_cols[,6]
colnames(tokens_all_temp) <- tokens_all_cols[,6]

#Format data
sample_data_all <- format_function(sample_data_all,sample_data_all_cols)
tokens_all_ios_f <- format_function(tokens_all_ios_f,tokens_all_cols)
tokens_all_pr_f <- format_function(tokens_all_pr_f,tokens_all_cols)
tokens_all_temp <- format_function(tokens_all_temp,tokens_all_cols)
readbl_all_df <- format_function(readbl_all_df,readbl_all_df_cols)
files <- format_function(files,files_cols)

#==============================================================================;
#IMPORT DATA;
cat("SECTION: IMPORT DATA", "\n")
#==============================================================================;

files[,1] <- c("1999.csv","2000.csv","2001.csv","2002.csv","2003.csv","2004.csv","2005.csv","2006.csv","2007.csv","2008.csv")
files[,2] <-  unlist(mapply(merge_cols_function,col_one=data_directory,col_two=files[,1],separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))

sample_date_headers <- names(sample_data_all)

#for (j in 1:2)
for (j in 1:nrow(files))
{
  #j <- 1
  #j <- 2
  
  sample_data_NA_row_index <- which(rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))==ncol(sample_data_all))
  sample_data_NA_row_first <- as.numeric(min(sample_data_NA_row_index))
  
  csv_j_row_count <- as.numeric(nrow(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)))
  csv_j_headers <- names(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE))
  
  #sample_data_cols2 <- sample_data_cols[1:2]
  #zzz1 <- sapply(sample_data_cols,import_across_row_function, 
  #               data_temp=sample_data_all, file_temp=files[j,2], row_NA_first_temp=sample_data_NA_row_first,temp_row_count=csv_j_row_count,temp_headers=csv_j_headers,
  #               simplify = FALSE, USE.NAMES = FALSE)
  #zzz2 <- ldply(zzz1, data.frame)
  #colnames(zzz1) <- sample_data_cols
  
  if (sample_data_NA_row_first==1)
  {
    sample_data_file_col_num <- as.numeric(match("File",names(sample_data_all)))
    sample_data_all[1:csv_j_row_count,sample_data_file_col_num] <- files[j,1]
    
    for (k in 1:ncol(sample_data_all))
      #for (k in 1:nrow(sample_data_input_cols))
    {
      #k <- 1
      csv_k_col_num  <- as.numeric(match(sample_data_all_cols[k,6],csv_j_headers))
      
      if(!(is.na(csv_k_col_num)))
      {
        #cat(sample_data_all_cols[k,6]," is in ",files[j,2], "\n")
        
        #xxx1 <- as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:csv_j_row_count,csv_k_col_num])
        #xxx2 <- as.character(sample_data_all[(csv_j_row_count+1):nrow(sample_data_all),k])
        #sample_data_all[,names(sample_data_all)==sample_data_all_cols[k,6]] <- append(xxx1,xxx2)
        
        sample_data_all[,k] <- append(as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:csv_j_row_count,csv_k_col_num]),
                                      as.character(sample_data_all[(csv_j_row_count+1):nrow(sample_data_all),k]))
        
      } else
      {
        #cat(sample_data_all_cols[k,6]," not in ",files[j,2], "\n")
      }
    }
    
  } else if (sample_data_NA_row_first>1)
  {
    sample_data_file_col_num <- as.numeric(match("File",names(sample_data_all)))
    sample_data_all[sample_data_NA_row_first:(sample_data_NA_row_first+csv_j_row_count-1),sample_data_file_col_num] <- files[j,1]
    
    for (k in 1:ncol(sample_data_all))
      #for (k in 1:nrow(sample_data_input_cols))
    {
      #k <- 1
      #k <- 70
      csv_k_col_num  <- as.numeric(match(sample_data_all_cols[k,6],csv_j_headers))
      
      if(!(is.na(csv_k_col_num)))
      {
        #cat(sample_data_all_cols[k,6]," is in ",files[j,2], "\n")
        
        #yyy1 <- as.character(sample_data_all[1:(sample_data_NA_row_first-1),k])
        #yyy2 <- as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:(csv_j_row_count),csv_k_col_num])
        #yyy3 <- as.character(sample_data_all[(sample_data_NA_row_first+csv_j_row_count):nrow(sample_data_all),k])
        #sample_data_all[,names(sample_data_all)==sample_data_all_cols[k,6]] <- append(append(yyy1,yyy2),yyy3)
        
        sample_data_all[,k] <- append(append(as.character(sample_data_all[1:(sample_data_NA_row_first-1),k]),
                                             as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:(csv_j_row_count),csv_k_col_num])),
                                      as.character(sample_data_all[(sample_data_NA_row_first+csv_j_row_count):nrow(sample_data_all),k]))
        
      } else
      {
        #cat(sample_data_all_cols[k,6]," not in ",files[j,2], "\n")
      }
    }
    
  } else
  {
    cat("ERROR!!", "\n")
    
  }
  
  #==============================================================================;
  #CLEAN DATA;
  #==============================================================================;
  
  #lapply(1:nrow(sample_data_all), function(x), Equity_Data_import_split[[x]], envir = .GlobalEnv))
  
  #xxx <- apply(sample_data_all,1,function(x) html2txt(x[1]))
  
  #x <- paste("i", "s", "n", "&", "a", "p", "o", "s", ";", "t", sep = "") 
  #xmlValue(getNodeSet(htmlParse(x, asText = TRUE), "//p")[[1]]) 
  
  #Format data
  #sample_data_all <- format_function(sample_data_all,sample_data_all_cols)
  
  #==============================================================================;
  #OUTPUT DATA;
  #==============================================================================;
  
  #Create temp data.frame of Smaple data where not NA after each iteration
  assign(paste("sample_data", formatC(j, width=6, format="d", flag="0"), sep = ""), sample_data_all[!(rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))==ncol(sample_data_all)),], envir = .GlobalEnv)
  
  #Create temp data.frame of for each CSV file
  temp_output_name <- paste("input_csv", formatC(j, width=6, format="d", flag="0"), sep = "")
  assign(temp_output_name, sample_data_all[sample_data_all[,sample_data_file_col_num]==files[j,1] & !is.na(sample_data_all[,sample_data_file_col_num]),], envir = .GlobalEnv)
  write.csv(sample_data_all[sample_data_all[,sample_data_file_col_num]==files[j,1] & !is.na(sample_data_all[,sample_data_file_col_num]),], file = paste(data_directory,temp_output_name,".csv",sep=""))
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  #Initiate garbage collection
  capture.output(gc(),file='NUL')
  
  progress_function(outer_loop_count=j, outer_loop_start_val=1, outer_loop_end_val=nrow(files), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
}

sample_data_all <- format_function(sample_data_all,sample_data_all_cols)

write.csv(sample_data_all[!(rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))==ncol(sample_data_all)),], file = paste(data_directory,"sample_data_all.csv",sep=""))
