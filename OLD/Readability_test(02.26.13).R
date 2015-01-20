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

#==============================================================================;
#PREALLOCATE DATA;
cat("SECTION: PREALLOCATE DATA", "\n")
#==============================================================================;

ptm <- proc.time()

#Create base column table
temp_data_cols <- as.data.frame(matrix(NA, ncol = 6, nrow = 200))
colnames(temp_data_cols) <- c("order","isnum","ischar","isdate","isfactor","colnames")
temp_data_cols[,1] <- as.numeric(temp_data_cols[,1])
temp_data_cols[,2] <- as.numeric(temp_data_cols[,2])
temp_data_cols[,3] <- as.numeric(temp_data_cols[,3])
temp_data_cols[,4] <- as.numeric(temp_data_cols[,4])
temp_data_cols[,5] <- as.numeric(temp_data_cols[,5])
temp_data_cols[,6] <- as.character(temp_data_cols[,6])

#Sample data table
sample_data_cols_count <- 44
sample_data <- as.data.frame(matrix(NA, ncol = sample_data_cols_count, nrow = 500000))
#sample_data <- as.data.frame(matrix(NA, ncol = sample_data_cols_count, nrow = 50000))
sample_data_cols <- temp_data_cols[1:sample_data_cols_count,]
sample_data_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",stringsAsFactors = FALSE)
sample_data_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="cusip8",stringsAsFactors = FALSE)
sample_data_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="crsp_fundno",stringsAsFactors = FALSE)
sample_data_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="chgdt",stringsAsFactors = FALSE)             #DATE
sample_data_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="chgenddt",stringsAsFactors = FALSE)          #DATE
sample_data_cols[6,] <- data.frame(order=6,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="crsp_portno",stringsAsFactors = FALSE)
sample_data_cols[7,] <- data.frame(order=7,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="crsp_cl_grp",stringsAsFactors = FALSE)
sample_data_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="fund_name",stringsAsFactors = FALSE)
sample_data_cols[9,] <- data.frame(order=9,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="nasdaq",stringsAsFactors = FALSE)
sample_data_cols[10,] <- data.frame(order=10,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ncusip",stringsAsFactors = FALSE)
sample_data_cols[11,] <- data.frame(order=11,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="first_offer_dt",stringsAsFactors = FALSE)  #DATE
sample_data_cols[12,] <- data.frame(order=12,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgmt_name",stringsAsFactors = FALSE)
sample_data_cols[13,] <- data.frame(order=13,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgmt_cd",stringsAsFactors = FALSE)
sample_data_cols[14,] <- data.frame(order=14,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgr_name",stringsAsFactors = FALSE)
sample_data_cols[15,] <- data.frame(order=15,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgr_dt",stringsAsFactors = FALSE)          #DATE
sample_data_cols[16,] <- data.frame(order=16,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="adv_name",stringsAsFactors = FALSE)
sample_data_cols[17,] <- data.frame(order=17,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="open_to_inv",stringsAsFactors = FALSE)
sample_data_cols[18,] <- data.frame(order=18,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="retail_fund",stringsAsFactors = FALSE)
sample_data_cols[19,] <- data.frame(order=19,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="inst_fund",stringsAsFactors = FALSE)
sample_data_cols[20,] <- data.frame(order=20,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="m_fund",stringsAsFactors = FALSE)
sample_data_cols[21,] <- data.frame(order=21,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index_fund_flag",stringsAsFactors = FALSE)
sample_data_cols[22,] <- data.frame(order=22,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="vau_fund",stringsAsFactors = FALSE)
sample_data_cols[23,] <- data.frame(order=23,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="et_flag",stringsAsFactors = FALSE)
sample_data_cols[24,] <- data.frame(order=24,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="fyear",stringsAsFactors = FALSE)
sample_data_cols[25,] <- data.frame(order=25,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="accession_num",stringsAsFactors = FALSE)
sample_data_cols[26,] <- data.frame(order=26,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="GVKEY",stringsAsFactors = FALSE)
sample_data_cols[27,] <- data.frame(order=27,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CIK",stringsAsFactors = FALSE)
sample_data_cols[28,] <- data.frame(order=28,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="FDATE",stringsAsFactors = FALSE)           #DATE
sample_data_cols[29,] <- data.frame(order=29,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="FINDEXDATE",stringsAsFactors = FALSE)      #DATE
sample_data_cols[30,] <- data.frame(order=30,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="LINDEXDATE",stringsAsFactors = FALSE)      #DATE
sample_data_cols[31,] <- data.frame(order=31,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Form",stringsAsFactors = FALSE)
sample_data_cols[32,] <- data.frame(order=32,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CoName",stringsAsFactors = FALSE)
sample_data_cols[33,] <- data.frame(order=33,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Fname",stringsAsFactors = FALSE)
sample_data_cols[34,] <- data.frame(order=34,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="PortName",stringsAsFactors = FALSE)
sample_data_cols[35,] <- data.frame(order=35,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective",stringsAsFactors = FALSE)
sample_data_cols[36,] <- data.frame(order=36,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy",stringsAsFactors = FALSE)
sample_data_cols[37,] <- data.frame(order=37,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks",stringsAsFactors = FALSE)
sample_data_cols[38,] <- data.frame(order=38,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_f",stringsAsFactors = FALSE)
sample_data_cols[39,] <- data.frame(order=39,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy_f",stringsAsFactors = FALSE)
sample_data_cols[40,] <- data.frame(order=40,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_strategy_f",stringsAsFactors = FALSE)
sample_data_cols[41,] <- data.frame(order=41,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks_f",stringsAsFactors = FALSE)
sample_data_cols[42,] <- data.frame(order=42,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Process_IS",stringsAsFactors = FALSE)
sample_data_cols[43,] <- data.frame(order=43,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Process_R",stringsAsFactors = FALSE)
sample_data_cols[44,] <- data.frame(order=44,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="File",stringsAsFactors = FALSE)
colnames(sample_data) <- sample_data_cols[,6]

#Sample data statistics table
sample_data_statistics_cols_count <- 54
sample_data_statistics <- as.data.frame(matrix(NA, ncol = sample_data_statistics_cols_count, nrow = 500000))
#sample_data_statistics <- as.data.frame(matrix(NA, ncol = sample_data_statistics_cols_count, nrow = 50000))
sample_data_statistics_cols <- temp_data_cols[1:(sample_data_statistics_cols_count/2),]
sample_data_statistics_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="lines",stringsAsFactors = FALSE)
sample_data_statistics_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="sentences",stringsAsFactors = FALSE)
sample_data_statistics_cols[3,] <- data.frame(order=3,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="words",stringsAsFactors = FALSE)
sample_data_statistics_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="all_chars",stringsAsFactors = FALSE)
sample_data_statistics_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="chars_no_space",stringsAsFactors = FALSE)
sample_data_statistics_cols[6,] <- data.frame(order=6,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="letters_only",stringsAsFactors = FALSE)
sample_data_statistics_cols[7,] <- data.frame(order=7,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="digits",stringsAsFactors = FALSE)
sample_data_statistics_cols[8,] <- data.frame(order=8,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="punct",stringsAsFactors = FALSE)
sample_data_statistics_cols[9,] <- data.frame(order=9,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="conjunctions",stringsAsFactors = FALSE)
sample_data_statistics_cols[10,] <- data.frame(order=10,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="prepositions",stringsAsFactors = FALSE)
sample_data_statistics_cols[11,] <- data.frame(order=11,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="pronouns",stringsAsFactors = FALSE)
sample_data_statistics_cols[12,] <- data.frame(order=12,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="foreign",stringsAsFactors = FALSE)
sample_data_statistics_cols[13,] <- data.frame(order=13,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="num_syll",stringsAsFactors = FALSE)
sample_data_statistics_cols[14,] <- data.frame(order=14,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="normalized_space",stringsAsFactors = FALSE)
sample_data_statistics_cols[15,] <- data.frame(order=15,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Flesch_Kincaid",stringsAsFactors = FALSE)
sample_data_statistics_cols[16,] <- data.frame(order=16,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="ARI",stringsAsFactors = FALSE)
sample_data_statistics_cols[17,] <- data.frame(order=17,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Coleman_Liau",stringsAsFactors = FALSE)
sample_data_statistics_cols[18,] <- data.frame(order=18,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="SMOG",stringsAsFactors = FALSE)
sample_data_statistics_cols[19,] <- data.frame(order=19,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="FOG_hard_words",stringsAsFactors = FALSE)
sample_data_statistics_cols[20,] <- data.frame(order=20,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="TTR",stringsAsFactors = FALSE)
sample_data_statistics_cols[21,] <- data.frame(order=21,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="sntc_per_word",stringsAsFactors = FALSE)
sample_data_statistics_cols[22,] <- data.frame(order=22,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="avg_sentc_length",stringsAsFactors = FALSE)
sample_data_statistics_cols[23,] <- data.frame(order=23,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="avg_word_length",stringsAsFactors = FALSE)
sample_data_statistics_cols[24,] <- data.frame(order=24,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="avg_syll_word",stringsAsFactors = FALSE)
sample_data_statistics_cols[25,] <- data.frame(order=25,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="sntc_per100",stringsAsFactors = FALSE)
sample_data_statistics_cols[26,] <- data.frame(order=26,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="syll_per100",stringsAsFactors = FALSE)
sample_data_statistics_cols[27,] <- data.frame(order=27,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="lett_per100",stringsAsFactors = FALSE)
#Double this data.frame because investment objective/strategy & principal_risks
sample_data_statistics_cols2 <- temp_data_cols[1:sample_data_statistics_cols_count,]
sample_data_statistics_cols2 <- rbind(sample_data_statistics_cols,sample_data_statistics_cols)
sample_data_statistics_cols2[,1] <- rep(1:54, 1)
sample_data_statistics_cols2[1:27,6] <-  unlist(mapply(merge_cols_function,col_one=sample_data_statistics_cols2[1:27,6],col_two="_iois",separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))
sample_data_statistics_cols2[28:54,6] <-  unlist(mapply(merge_cols_function,col_one=sample_data_statistics_cols2[28:54,6],col_two="_pr",separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))
colnames(sample_data_statistics) <- sample_data_statistics_cols2[,6]

#Tokens table
tokens_all_cols_count <- 4
tokens_all <- as.data.frame(matrix(NA, ncol = tokens_all_cols_count, nrow = 10000000))
#tokens_all <- as.data.frame(matrix(NA, ncol = tokens_all_cols_count, nrow = 1000000))
tokens_all_cols <- temp_data_cols[1:tokens_all_cols_count,]
tokens_all_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ID",stringsAsFactors = FALSE)
tokens_all_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token",stringsAsFactors = FALSE)
tokens_all_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="desc",stringsAsFactors = FALSE)
tokens_all_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Remove",stringsAsFactors = FALSE)
colnames(tokens_all) <- tokens_all_cols[,6]

#Readability table
readbl_all_df_cols_count <- 5
readbl_all_df <- as.data.frame(matrix(NA, ncol = readbl_all_df_cols_count, nrow = 44))
readbl_all_df_cols <- temp_data_cols[1:readbl_all_df_cols_count,]
readbl_all_df_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index",stringsAsFactors = FALSE)
readbl_all_df_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="flavour",stringsAsFactors = FALSE)
readbl_all_df_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="raw",stringsAsFactors = FALSE)
readbl_all_df_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="grade",stringsAsFactors = FALSE)
readbl_all_df_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="age",stringsAsFactors = FALSE)
colnames(readbl_all_df) <- readbl_all_df_cols[,6]

#Files table
files_cols_count <- 2
files <- as.data.frame(matrix(NA, ncol = files_cols_count, nrow = 10))
files_cols <- temp_data_cols[1:files_cols_count,]
files_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filename",stringsAsFactors = FALSE)
files_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filepath",stringsAsFactors = FALSE)
colnames(files) <- files_cols[,6]

#Format data
sample_data <- format_function(sample_data,sample_data_cols)
sample_data_statistics <- format_function(sample_data_statistics,sample_data_statistics_cols)
tokens_all <- format_function(tokens_all,tokens_all_cols)
readbl_all_df <- format_function(readbl_all_df,readbl_all_df_cols)
files <- format_function(files,files_cols)

#==============================================================================;
#IMPORT DATA;
cat("SECTION: IMPORT DATA", "\n")
#==============================================================================;

files[,1] <- c("1999.csv","2000.csv","2001.csv","2002.csv","2003.csv","2004.csv","2005.csv","2006.csv","2007.csv","2008.csv")
files[,2] <-  unlist(mapply(merge_cols_function,col_one=data_directory,col_two=files[,1],separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))

for (j in 1:nrow(files))
{
  #j <- 1
  #j <- 2
  
  row_NA_index_data <- which(rowSums(is.na(sample_data[,1:ncol(sample_data)]))==ncol(sample_data))
  row_NA_first_data <- as.numeric(min(row_NA_index_data))
  
  temp_row_count <- as.numeric(nrow(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)))
  temp_headers <- names(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE))
  
  #sample_data_cols2 <- sample_data_cols[1:2]
  #zzz1 <- sapply(sample_data_cols,import_across_row_function, 
  #               data_temp=sample_data, file_temp=files[j,2], row_NA_first_temp=row_NA_first_data,temp_row_count=temp_row_count,temp_headers=temp_headers,
  #               simplify = FALSE, USE.NAMES = FALSE)
  #zzz2 <- ldply(zzz1, data.frame)
  #colnames(zzz1) <- sample_data_cols
  
  if (row_NA_first_data==1)
  {
    temp_sample_data_col_num <- as.numeric(match("File",names(sample_data)))
    sample_data[1:temp_row_count,temp_sample_data_col_num] <- files[j,1]
    
    for (k in 1:nrow(sample_data_cols))
    {
      #k <- 1
      temp_csv_col_num  <- as.numeric(match(sample_data_cols[k,6],temp_headers))
      
      if(!(is.na(temp_csv_col_num)))
      {
        #cat(sample_data_cols[k,6]," is in ",files[j,2], "\n")
        
        xxx1 <- as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:temp_row_count,temp_csv_col_num])
        xxx2 <- as.character(sample_data[(temp_row_count+1):nrow(sample_data),names(sample_data)==sample_data_cols[k,6]])
        sample_data[,names(sample_data)==sample_data_cols[k,6]] <- append(xxx1,xxx2)
        
        #sample_data[,names(sample_data)==sample_data_cols[k,6]] <- append(as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:temp_row_count,temp_csv_col_num]),
        #                                                                as.character(sample_data[(temp_row_count+1):nrow(sample_data),names(sample_data)==sample_data_cols[k]]))
        
      } else
      {
        #cat(sample_data_cols[k,6]," not in ",files[j,2], "\n")
        
      }

    }
    
  } else if (row_NA_first_data>1)
  {
    temp_sample_data_col_num <- as.numeric(match("File",names(sample_data)))
    sample_data[row_NA_first_data:(row_NA_first_data+temp_row_count-1),temp_sample_data_col_num] <- files[j,1]

    for (k in 1:nrow(sample_data_cols))
    {
      #k <- 1
      temp_csv_col_num  <- as.numeric(match(sample_data_cols[k,6],temp_headers))
      
      if(!(is.na(temp_csv_col_num)))
      {
        #cat(sample_data_cols[k,6]," is in ",files[j,2], "\n")
        
        yyy1 <- sample_data[1:(row_NA_first_data-1),names(sample_data)==sample_data_cols[k,6]]
        yyy2 <- as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:(temp_row_count),temp_csv_col_num])
        yyy3 <- as.character(sample_data[(row_NA_first_data+temp_row_count):nrow(sample_data),names(sample_data)==sample_data_cols[k,6]])
        
        sample_data[,names(sample_data)==sample_data_cols[k,6]] <- append(append(yyy1,yyy2),yyy3)
        
      
      } else
      {
        #cat(sample_data_cols[k,6]," not in ",files[j,2], "\n")
        
      }
 
    }
    
  } else
  {
    cat("ERROR!!", "\n")
    
  }
  
  #==============================================================================;
  #CLEAN DATA;
  #==============================================================================;
  
  #lapply(1:nrow(sample_data), function(x), Equity_Data_import_split[[x]], envir = .GlobalEnv))
  
  #xxx <- apply(sample_data,1,function(x) html2txt(x[1]))
  
  #x <- paste("i", "s", "n", "&", "a", "p", "o", "s", ";", "t", sep = "") 
  #xmlValue(getNodeSet(htmlParse(x, asText = TRUE), "//p")[[1]]) 
  
  #Format data
  sample_data <- format_function(sample_data,sample_data_cols)
  
  #==============================================================================;
  #OUTPUT DATA;
  #==============================================================================;
  
  #Create temp data.frame of Smaple data where not NA after each iteration
  assign(paste("sample_data", formatC(j, width=6, format="d", flag="0"), sep = ""), sample_data[!(rowSums(is.na(sample_data[,1:ncol(sample_data)]))==ncol(sample_data)),], envir = .GlobalEnv)
  
  #Create temp data.frame of for each CSV file
  temp_output_name <- paste("input_csv", formatC(j, width=6, format="d", flag="0"), sep = "")
  #assign(temp_output_name, sample_data[sample_data[,names(sample_data)=="File"]==files[j,1] & !is.na(sample_data[,names(sample_data)=="File"]),], envir = .GlobalEnv)
  write.csv(sample_data[sample_data[,names(sample_data)=="File"]==files[j,1] & !is.na(sample_data[,names(sample_data)=="File"]),], file = paste(data_directory,temp_output_name,".csv",sep=""))
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  #Initiate garbage collection
  capture.output(gc(),file='NUL')
  
  progress_function(outer_loop_count=j, outer_loop_start_val=1, outer_loop_end_val=nrow(files), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
}

#==============================================================================;
#CLEAN DATA;
#==============================================================================;
#sample_data <- sample_data000010

#Find most recent sample_dataXXXXXX
#sample_data_vector <- ls(pattern = 'sample_data[0-9]_*')
#sample_data_last <- max(sample_data_vector)

#Merge investment_objective_f and investment_strategy_f and place in investment_objective_strategy_f
sample_data[,names(sample_data)=="investment_objective_strategy_f"] <- unlist(mapply(merge_cols_function,col_one=sample_data[,names(sample_data)=="investment_objective_f"],col_two=sample_data[,names(sample_data)=="investment_strategy_f"],separator="\n\n", SIMPLIFY = FALSE,USE.NAMES = FALSE))

#Replace unformatted text with N/A's
sample_data[,names(sample_data)=="investment_objective"] <- rep(NA, nrow(sample_data))
sample_data[,names(sample_data)=="investment_strategy"] <- rep(NA, nrow(sample_data))
sample_data[,names(sample_data)=="principal_risks"] <- rep(NA, nrow(sample_data))
sample_data[,names(sample_data)=="investment_objective_f"] <- rep(NA, nrow(sample_data))
sample_data[,names(sample_data)=="investment_strategy_f"] <- rep(NA, nrow(sample_data))

#Remove all NAs rows left
sample_data2 <- sample_data[!(rowSums(is.na(sample_data[,1:ncol(sample_data)]))==ncol(sample_data)),]

#test <- sample_data[245:260,]
#test_iois <- test[,names(sample_data)=="investment_objective_strategy_f"]
#test_pr <- test[,names(sample_data)=="principal_risks_f"]

#==============================================================================;
#GET STATISTICS;
cat("SECTION: GET STATISTICS", "\n")
#==============================================================================;

for (i in 1:nrow(sample_data))
{
  #i <- 1
  #i <- 2
  
  str_id <- paste("", formatC(i, width=6, format="d", flag="0"), sep = "")
  
  sample_cell <- as.character(sample_data[i,1])
  
  temp_text <- unlist(strsplit(sample_cell, "\n"))
  temp_text_df <- as.data.frame(temp_text)
  names(temp_text_df)[1] <- "temp_text"
  
  fileConn<-file(paste(data_directory,"temptext.txt",sep=""))
  writeLines(temp_text, fileConn)
  close(fileConn)
  
  #==============================================================================;
  #kRp.tagged-class;
  #==============================================================================;
  
  tagged_text <- treetag("../Data/temptext.txt", treetagger="manual",lang="en", TT.options=c(path="C:/TreeTagger", preset="en"),debug=FALSE)
  #tagged_text <- treetag("../Data/temptext.txt", treetagger = "manual", rm.sgml = TRUE,lang = "en", sentc.end = c(".", "!", "?", ";", ":"),encoding = NULL, TT.options = c(path="C:/TreeTagger", preset="en"), debug = FALSE,TT.tknz = TRUE, format = "file")
  
  #tagged_text_tokens <- tagged_text@TT.res
  #tagged_text_desc <- tagged_text@desc
  
  #==============================================================================;
  #kRp.hyphen-class;
  #==============================================================================;
  
  hyph_text_en <- hyphen(tagged_text,quiet=TRUE)
  
  #hyph_text_en_desc <- hyph_text_en@desc
  
  #==============================================================================;
  #kRp.readability-class;
  #==============================================================================;
  
  readbl_text <- suppressWarnings(readability(tagged_text, hyphen=hyph_text_en, index="all",quiet=TRUE))
  
  #readbl_text_tokens <- readbl_text@TT.res
  #readbl_text_desc <- readbl_text@desc
  
  readbl_all_df[1:44,] <- summary(readbl_text)[1:44,]
  
  #==============================================================================;
  #FILL IN GLOBAL DICTIONARY;
  #==============================================================================;
  
  row_NA_index_dict <- which(rowSums(is.na(tokens_all[,1:ncol(tokens_all)]))==ncol(tokens_all))
  row_NA_first_dict <- as.numeric(min(row_NA_index_dict))
  
  if (row_NA_first_dict==1)
  {
    tokens_all[1:nrow(readbl_text@TT.res),names(tokens_all)=="ID"] <- str_id
    tokens_all[,names(tokens_all)=="token"] <- append(as.character(readbl_text@TT.res[1:nrow(readbl_text@TT.res),names(readbl_text@TT.res)=="token"]),
                                                      as.character(tokens_all[(nrow(readbl_text@TT.res)+1):nrow(tokens_all),names(tokens_all)=="token"]))  
    tokens_all[,names(tokens_all)=="desc"] <- append(as.character(readbl_text@TT.res[1:nrow(readbl_text@TT.res),names(readbl_text@TT.res)=="desc"]),
                                                     as.character(tokens_all[(nrow(readbl_text@TT.res)+1):nrow(tokens_all),names(tokens_all)=="desc"])) 
    
    
  } else if (row_NA_first_dict>1)
  {
    tokens_all[row_NA_first_dict:(row_NA_first_dict+nrow(readbl_text@TT.res)-1),names(tokens_all)=="ID"] <- str_id
    tokens_all[,names(tokens_all)=="token"] <- append(append(as.character(tokens_all[1:(row_NA_first_dict-1),names(tokens_all)=="token"]),
                                                             as.character(readbl_text@TT.res[1:nrow(readbl_text@TT.res),names(readbl_text@TT.res)=="token"])),
                                                      as.character(tokens_all[(row_NA_first_dict+nrow(readbl_text@TT.res)):nrow(tokens_all),names(tokens_all)=="token"]))
    tokens_all[,names(tokens_all)=="desc"] <- append(append(as.character(tokens_all[1:(row_NA_first_dict-1),names(tokens_all)=="desc"]),
                                                            as.character(readbl_text@TT.res[1:nrow(readbl_text@TT.res),names(readbl_text@TT.res)=="desc"])),
                                                     as.character(tokens_all[(row_NA_first_dict+nrow(readbl_text@TT.res)):nrow(tokens_all),names(tokens_all)=="desc"])) 
    
  } else
  {
    cat("ERROR!!", "\n")
    
  }
  
  #==============================================================================;
  #COMBINE STATISTICS DATA;
  #==============================================================================;
  sample_data_statistics[i,names(sample_data_statistics)=="lines"]  <- as.data.frame(tagged_text@desc$lines)
  sample_data_statistics[i,names(sample_data_statistics)=="sentences"]  <- as.data.frame(readbl_text@desc$sentences)
  sample_data_statistics[i,names(sample_data_statistics)=="words"]  <- as.data.frame(readbl_text@desc$words)
  sample_data_statistics[i,names(sample_data_statistics)=="all_chars"]  <- as.data.frame(readbl_text@desc$all.chars)
  sample_data_statistics[i,names(sample_data_statistics)=="chars_no_space"]  <- as.data.frame(tagged_text@desc$chars.no.space)
  sample_data_statistics[i,names(sample_data_statistics)=="letters_only"]  <- as.data.frame(tagged_text@desc$letters.only)
  sample_data_statistics[i,names(sample_data_statistics)=="digits"]  <- as.data.frame(tagged_text@desc$digits)
  sample_data_statistics[i,names(sample_data_statistics)=="punct"]  <- as.data.frame(readbl_text@desc$punct)
  sample_data_statistics[i,names(sample_data_statistics)=="conjunctions"]  <- as.data.frame(readbl_text@desc$conjunctions)
  sample_data_statistics[i,names(sample_data_statistics)=="prepositions"]  <- as.data.frame(readbl_text@desc$prepositions)
  sample_data_statistics[i,names(sample_data_statistics)=="pronouns"]  <- as.data.frame(readbl_text@desc$pronouns)
  sample_data_statistics[i,names(sample_data_statistics)=="foreign"]  <- as.data.frame(readbl_text@desc$foreign)
  sample_data_statistics[i,names(sample_data_statistics)=="num_syll"]  <- as.data.frame(hyph_text_en@desc$num.syll)
  sample_data_statistics[i,names(sample_data_statistics)=="normalized_space"] <- as.data.frame(tagged_text@desc$normalized.space)
  sample_data_statistics[i,names(sample_data_statistics)=="Flesch_Kincaid"]  <- as.numeric(readbl_all_df[readbl_all_df[,1]=="Flesch-Kincaid" & readbl_all_df[,2]=="", names(readbl_all_df)=="grade"])
  sample_data_statistics[i,names(sample_data_statistics)=="ARI"]  <- as.numeric(readbl_all_df[readbl_all_df[,1]=="ARI" & readbl_all_df[,2]=="", names(readbl_all_df)=="grade"])
  sample_data_statistics[i,names(sample_data_statistics)=="Coleman_Liau"]  <- as.numeric(readbl_all_df[readbl_all_df[,1]=="Coleman-Liau" & readbl_all_df[,2]=="", names(readbl_all_df)=="grade"])
  sample_data_statistics[i,names(sample_data_statistics)=="SMOG"]  <- as.numeric(readbl_all_df[readbl_all_df[,1]=="SMOG" & readbl_all_df[,2]=="", names(readbl_all_df)=="grade"])
  sample_data_statistics[i,names(sample_data_statistics)=="FOG_hard_words"]  <-  as.data.frame(readbl_text@desc$FOG.hard.words)
  sample_data_statistics[i,names(sample_data_statistics)=="TTR"]  <-  as.data.frame(readbl_text@desc$TTR)
  sample_data_statistics[i,names(sample_data_statistics)=="sntc_per_word"]  <- as.data.frame(readbl_text@desc$sntc.per.word)
  sample_data_statistics[i,names(sample_data_statistics)=="avg_sentc_length"]  <- as.data.frame(readbl_text@desc$avg.sentc.length)
  sample_data_statistics[i,names(sample_data_statistics)=="avg_word_length"]  <- as.data.frame(readbl_text@desc$avg.word.length)
  sample_data_statistics[i,names(sample_data_statistics)=="avg_syll_word"]  <- as.data.frame(readbl_text@desc$avg.syll.word)
  sample_data_statistics[i,names(sample_data_statistics)=="sntc_per100"]  <- as.data.frame(readbl_text@desc$sntc.per100) 
  sample_data_statistics[i,names(sample_data_statistics)=="syll_per100"]  <- as.data.frame(readbl_text@desc$syll.per100)
  sample_data_statistics[i,names(sample_data_statistics)=="lett_per100"]  <- as.data.frame(readbl_text@desc$lett.per100)
  sample_data_statistics[i,names(sample_data_statistics)=="investment_strategy"] <- as.character(sample_data[i,1])
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  #Initiate garbage collection
  capture.output(gc(),file='NUL')
  
  progress_function(outer_loop_count=1, outer_loop_start_val=1, outer_loop_end_val=1, inner_loop_count=i, inner_loop_start_val=1, inner_loop_end_val=nrow(sample_data))
  
}

#==============================================================================;
#COMBINE INDIVIDUAL TOKEN;
cat("COMBINE INDIVIDUAL TOKEN", "\n")
#==============================================================================;

#Remove all NAs rows left
tokens_all <- tokens_all[!(rowSums(is.na(tokens_all[,1:ncol(tokens_all)]))==ncol(tokens_all)),]

#Find which rows to remove
tokens_all[!(tokens_all$desc %in% c("Cardinal number","Comma","Sentence ending punctuation","Symbol",
                                    "Opening bracket","Closing bracket","Quote","End quote")) 
           & !(tokens_all$token %in% c("%","&")), "Remove"] <- 0
tokens_all[(tokens_all$desc %in% c("Cardinal number","Comma","Sentence ending punctuation","Symbol",
                                   "Opening bracket","Closing bracket","Quote","End quote")) 
           |(tokens_all$token %in% c("%","&")), "Remove"] <- 1


query_tokens <- "select distinct  ID, Upper(token) token, Count(token) Count
from             tokens_all 
where            Remove=0
group by         ID, Upper(token)"

tokens_all2 <- sqldf(query_tokens)

#Create uTotal and gTotal
tokens_all2 <- ddply(tokens_all2, "ID", function(x) data.frame(x, uTotal=nrow(x),gTotal=sum(x$Count)) )

#Output file
write.csv(tokens_all, file = paste(data_directory,"tokens_all.csv",sep=""))

#==============================================================================;
#DONE;
cat("DONE", "\n")
#==============================================================================;

#Remove temporary data.frames
#rm(i,row_NA_index_dict,row_NA_first_dict,str_id,sample_cell, temp_text,temp_text_df,fileConn,tagged_text,hyph_text_en)
#rm(readbl_text,readbl_all_df,sample_data,sample_data_statistics,query_tokens)

proc.time() - ptm