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

#Create output directory
output_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/R/",winslash = "\\", mustWork = NA)  #Home

#Data data directory
data_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)  #Home

#Create function directory
function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/",winslash = "\\", mustWork = NA)  #Home

#Create package directory
package_directory <- normalizePath("C:/Users/Brad/Documents/R/win-library/2.15/",winslash = "\\", mustWork = NA)  #Home

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
library(gmodels)        #Package for Various R programming tools for model fitting
#library(googleVis)      #Interface between R and the Google Visualisation API
library(gWidgets)       #Package for gWidgets API for building toolkit-independent, interactive GUIs
library(gWidgetstcltk)  #Package for Toolkit implementation of gWidgets for tcltk package (or gWidgetsGtk2 or gWidgetsrJava or gWidgetsWWW or gWidgetsQt)
library(Hmisc)          #Package for data imputation
library(koRpus)         #Package for ..........
library(lmtest)         #Package for testing Linear Regression Models
library(mitools)        #Package for imputation of missing data
library(PERregress)     #Package for Regression Functions and Datasets
library(plm)            #Package for creating Linear Models for Panel Data
library(plyr)           #Package for splitting, applying and combining data
library(quantmod)       #Package for Quantitative Financial Modelling Framework
library(R.oo)           #Package for trim() function
library(RODBC)          #Package for ODBC Database Access in R
library(rpanel)         #Package for Simple Interactive Controls with the tcltk Package.
library(RSiteSearch)    #Package for RSiteSearch
library(sandwich)       #Package for Robust Covariance Matrix Estimators
library(sqldf)          #Package for performing SQL selects on R Data Frames
library(stringr)        #Package for additional string functions
library(tcltk)          #Package for Tcl/Tk Interface
library(tseries)        #Package for time series analysis & computational finance
library(zoo)            #Package for performing time-series analysis


#==============================================================================;
#EXTERNAL FUNCTIONS;
cat("SECTION: EXTERNAL FUNCTIONS", "\n")
#==============================================================================;

#Reference functions file
source(file=paste(function_directory,"functions.R",sep=""),echo=FALSE)

#==============================================================================;
#IMPORT DATA;
cat("SECTION: IMPORT DATA", "\n")
#==============================================================================;

Sample_Data <- read.csv(file = paste(data_directory,"Sample_Data.csv",sep=""),header=TRUE,na.strings = "NA")

#==============================================================================;
#ALLOCATE ADDITTONAL DATA;
cat("SECTION: IMPORT DATA", "\n")
#==============================================================================;

Sample_Data_Statistics <- as.data.frame(matrix(NA, ncol = 28, nrow = nrow(Sample_Data)))
names(Sample_Data_Statistics)[1] <- "lines"
names(Sample_Data_Statistics)[2] <- "sentences"
names(Sample_Data_Statistics)[3] <- "words"
names(Sample_Data_Statistics)[4] <- "all_chars"
names(Sample_Data_Statistics)[5] <- "chars_no_space"
names(Sample_Data_Statistics)[6] <- "letters_only"
names(Sample_Data_Statistics)[7] <- "digits"
names(Sample_Data_Statistics)[8] <- "punct"
names(Sample_Data_Statistics)[9] <- "conjunctions"
names(Sample_Data_Statistics)[10] <- "prepositions"
names(Sample_Data_Statistics)[11] <- "pronouns"
names(Sample_Data_Statistics)[12] <- "foreign"
names(Sample_Data_Statistics)[13] <- "num_syll"
names(Sample_Data_Statistics)[14] <- "normalized_space"
names(Sample_Data_Statistics)[15] <- "Flesch_Kincaid"
names(Sample_Data_Statistics)[16] <- "ARI"
names(Sample_Data_Statistics)[17] <- "Coleman_Liau"
names(Sample_Data_Statistics)[18] <- "SMOG"
names(Sample_Data_Statistics)[19] <- "FOG_hard_words"
names(Sample_Data_Statistics)[20] <- "TTR"
names(Sample_Data_Statistics)[21] <- "sntc_per_word"
names(Sample_Data_Statistics)[22] <- "avg_sentc_length"
names(Sample_Data_Statistics)[23] <- "avg_word_length"
names(Sample_Data_Statistics)[24] <- "avg_syll_word"
names(Sample_Data_Statistics)[25] <- "sntc_per100"
names(Sample_Data_Statistics)[26] <- "syll_per100"
names(Sample_Data_Statistics)[27] <- "lett_per100"
names(Sample_Data_Statistics)[28] <- "investment_strategy"

#Find column number for each column
lines_col_num  <- match("lines",names(Sample_Data_Statistics))
sentences_col_num  <- match("sentences",names(Sample_Data_Statistics))
words_col_num  <- match("words",names(Sample_Data_Statistics))
all_chars_col_num  <- match("all_chars",names(Sample_Data_Statistics))
chars_no_space_col_num  <- match("chars_no_space",names(Sample_Data_Statistics))
letters_only_col_num  <- match("letters_only",names(Sample_Data_Statistics))
digits_col_num  <- match("digits",names(Sample_Data_Statistics))
punct_col_num  <- match("punct",names(Sample_Data_Statistics))
conjunctions_col_num  <- match("conjunctions",names(Sample_Data_Statistics))
prepositions_col_num  <- match("prepositions",names(Sample_Data_Statistics))
pronouns_col_num  <- match("pronouns",names(Sample_Data_Statistics))
foreign_col_num  <- match("foreign",names(Sample_Data_Statistics))
num_syll_col_num  <- match("num_syll",names(Sample_Data_Statistics))
normalized_space_col_num  <- match("normalized_space",names(Sample_Data_Statistics))
Flesch_Kincaid_col_num  <- match("Flesch_Kincaid",names(Sample_Data_Statistics))
ARI_col_num  <- match("ARI",names(Sample_Data_Statistics))
Coleman_Liau_col_num  <- match("Coleman_Liau",names(Sample_Data_Statistics))
SMOG_col_num  <- match("SMOG",names(Sample_Data_Statistics))
FOG_hard_words_col_num  <- match("FOG_hard_words",names(Sample_Data_Statistics))
TTR_col_num  <- match("TTR",names(Sample_Data_Statistics))
sntc_per_word_col_num  <- match("sntc_per_word",names(Sample_Data_Statistics))
avg_sentc_length_col_num  <- match("avg_sentc_length",names(Sample_Data_Statistics))
avg_word_length_col_num  <- match("avg_word_length",names(Sample_Data_Statistics))
avg_syll_word_col_num  <- match("avg_syll_word",names(Sample_Data_Statistics))
sntc_per100_col_num  <- match("sntc_per100",names(Sample_Data_Statistics))
syll_per100_col_num  <- match("syll_per100",names(Sample_Data_Statistics))
lett_per100_col_num  <- match("lett_per100",names(Sample_Data_Statistics))
investment_strategy_col_num  <- match("investment_strategy",names(Sample_Data_Statistics))


#==============================================================================;
#GET STATISTICS;
cat("SECTION: IMPORT DATA", "\n")
#==============================================================================;

for (i in 1:nrow(Sample_Data))
{
  #i <- 2
  
  Sample_Cell <- as.character(Sample_Data[i,1])
  
  temp_text <- unlist(strsplit(Sample_Cell, "\n"))
  temp_text_df <- as.data.frame(temp_text)
  names(temp_text_df)[1] <- "temp_text"
  
  fileConn<-file(paste(data_directory,"temptext.txt",sep=""))
  writeLines(temp_text, fileConn)
  close(fileConn)
  
  
  #set.kRp.env(TT.cmd="manual", lang="en",TT.options=c(path="~/./bin/treetagger/", preset="en"))
  #set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="en")
  #set.kRp.env(TT.cmd="C:/TreeTagger/bin/tag-english.bat", lang="en")
  #set.kRp.env(TT.cmd="C:/TreeTagger/bin/tree-tagger.exe", lang="en")
  
  #get.kRp.env(TT.cmd=TRUE)
  #tagged_text <- treetag("../Data/temptext.txt", treetagger="kRp.env",lang="en", debug=TRUE)
  
  #tagged_text <- treetag(temp_text, format = "obj", treetagger="manual",lang="en", TT.options=c(path="C:/TreeTagger", preset="en"), debug=TRUE)
  #tagged_text <- treetag("../Data/temptext.txt", treetagger="manual",lang="en", TT.options=c(path="C:/TreeTagger", preset="en"),debug=FALSE)
  tagged_text <- treetag("../Data/temptext.txt", treetagger = "manual", rm.sgml = TRUE,lang = "en", sentc.end = c(".", "!", "?", ";", ":"),encoding = NULL, TT.options = c(path="C:/TreeTagger", preset="en"), debug = FALSE,TT.tknz = TRUE, format = "file")
  
  #tagged_text <- tokenize("../Data/temptext.txt",format="file", lang="en")
  #tagged_text <- tokenize(temp_text, format = "obj",lang="en")
  
  tagged_text_tokens <- tagged_text@TT.res
  
  #==============================================================================;
  #kRp.tagged-class;
  #==============================================================================;
  
  tagged_text_desc <- tagged_text@desc
  
  lines <- as.data.frame(tagged_text_desc$lines)
  digits <- as.data.frame(tagged_text_desc$digits)
  chars_no_space <- as.data.frame(tagged_text_desc$chars.no.space)
  letters_only <- as.data.frame(tagged_text_desc$letters.only)
  normalized_space <- as.data.frame(tagged_text_desc$normalized.space)
  #char_distrib <- as.data.frame(tagged_text_desc$char.distrib)
  names(lines)[1] <- "lines"
  names(digits)[1] <- "digits"
  names(chars_no_space)[1] <- "chars_no_space"
  names(letters_only)[1] <- "letters_only"
  names(normalized_space)[1] <- "normalized_space"
  #names(char_distrib)[1] <- "char_distrib"
  
  #==============================================================================;
  #kRp.hyphen-class;
  #==============================================================================;
  
  hyph_text_en <- hyphen(tagged_text)
  
  hyph_text_en_desc <- hyph_text_en@desc
  num_syll <- as.data.frame(hyph_text_en_desc$num.syll)
  names(num_syll)[1] <- "num_syll"
  
  #==============================================================================;
  #kRp.readability-class;
  #==============================================================================;
  
  readbl_text <- readability(tagged_text, hyphen=hyph_text_en, index="all")
  
  readbl_text_desc <- readbl_text@desc
  sentences <- as.data.frame(readbl_text_desc$sentences)
  words <- as.data.frame(readbl_text_desc$words)
  all_chars <- as.data.frame(readbl_text_desc$all.chars)
  punct <- as.data.frame(readbl_text_desc$punct)
  conjunctions <- as.data.frame(readbl_text_desc$conjunctions)
  prepositions <- as.data.frame(readbl_text_desc$prepositions)
  pronouns <- as.data.frame(readbl_text_desc$pronouns)
  foreign <- as.data.frame(readbl_text_desc$foreign)
  TTR <- as.data.frame(readbl_text_desc$TTR)
  avg_sentc_length <- as.data.frame(readbl_text_desc$avg.sentc.length)
  avg_word_length <- as.data.frame(readbl_text_desc$avg.word.length)
  avg_syll_word <- as.data.frame(readbl_text_desc$avg.syll.word)
  sntc_per_word <- as.data.frame(readbl_text_desc$sntc.per.word)
  sntc_per100 <- as.data.frame(readbl_text_desc$sntc.per100)
  lett_per100 <- as.data.frame(readbl_text_desc$lett.per100)
  syll_per100 <- as.data.frame(readbl_text_desc$syll.per100)
  FOG_hard_words <- as.data.frame(readbl_text_desc$FOG.hard.words)
  #syllables <- as.data.frame(readbl_text_desc$syllables)
  #letters <- as.data.frame(readbl_text_desc$letters)
  #lttr_distrib <- as.data.frame(readbl_text_desc$lttr.distrib)
  #syll_distrib <- as.data.frame(readbl_text_desc$syll.distrib)
  #syll_uniq_distrib <- as.data.frame(readbl_text_desc$syll.uniq.distrib)
  #Bormuth_NOL <- as.data.frame(readbl_text_desc$Bormuth.NOL)
  #Dale_Chall_NOL <- as.data.frame(readbl_text_desc$Dale.Chall.NOL)
  #Harris_Jacobson_NOL <- as.data.frame(readbl_text_desc$Harris.Jacobson.NOL)
  #Spache_NOL <- as.data.frame(readbl_text_desc$Spache.NOL)
  names(sentences)[1] <- "sentences"
  names(words)[1] <- "words"
  names(all_chars)[1] <- "all_chars"
  names(punct)[1] <- "punct"
  names(conjunctions)[1] <- "conjunctions"
  names(prepositions)[1] <- "prepositions"
  names(pronouns)[1] <- "pronouns"
  names(foreign)[1] <- "foreign"
  names(TTR)[1] <- "TTR"
  names(avg_sentc_length)[1] <- "avg_sentc_length"
  names(avg_word_length)[1] <- "avg_word_length"
  names(avg_syll_word)[1] <- "avg_syll_word"
  names(sntc_per_word)[1] <- "sntc_per_word"
  names(sntc_per100)[1] <- "sntc_per100"
  names(lett_per100)[1] <- "lett_per100"
  names(syll_per100)[1] <- "syll_per100"
  names(FOG_hard_words)[1] <- "FOG_hard_words"
  #names(syllables)[1] <- "syllables"
  #names(letters)[1] <- "letters"
  #names(lttr_distrib)[1] <- "lttr_distrib"
  #names(syll_distrib)[1] <- "syll_distrib"
  #names(syll_uniq_distrib)[1] <- "syll_uniq_distrib"
  #names(Bormuth_NOL)[1] <- "Bormuth_NOL"
  #names(Dale_Chall_NOL)[1] <- "Dale_Chall_NOL"
  #names(Harris_Jacobson_NOL)[1] <- "Harris_Jacobson_NOL"
  #names(Spache_NOL)[1] <- "Spache_NOL"
  
  readbl_all_df <- as.data.frame(summary(readbl_text))
  
  ARI <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="ARI" & readbl_all_df[,2]=="",], select = c(grade)))
  Coleman_Liau <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="Coleman-Liau" & readbl_all_df[,2]=="",], select = c(grade)))
  Flesch_Kincaid <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="Flesch-Kincaid" & readbl_all_df[,2]=="",], select = c(grade)))
  SMOG <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="SMOG" & readbl_all_df[,2]=="",], select = c(grade)))
  names(ARI)[1] <- "ARI"
  names(Coleman_Liau)[1] <- "Coleman_Liau"
  names(Flesch_Kincaid)[1] <- "Flesch_Kincaid"
  names(SMOG)[1] <- "SMOG"
  
  
  #==============================================================================;
  #Combine statistics;
  #==============================================================================;
  
  
  counts <- cbind(lines, sentences, words, all_chars, chars_no_space, letters_only, digits, punct, conjunctions, prepositions, pronouns, foreign, num_syll, normalized_space)
  counts <- cbind(counts, Flesch_Kincaid, ARI, Coleman_Liau, SMOG, FOG_hard_words)
  counts <- cbind(counts, TTR, sntc_per_word, avg_sentc_length,avg_word_length, avg_syll_word, sntc_per100, syll_per100, lett_per100)
  rm(lines, sentences, words, all_chars, chars_no_space, letters_only, digits, punct, conjunctions, prepositions, pronouns, foreign, num_syll, normalized_space)
  rm(Flesch_Kincaid, ARI, Coleman_Liau, SMOG, FOG_hard_words)
  rm(TTR, sntc_per_word, avg_sentc_length,avg_word_length, avg_syll_word, sntc_per100, syll_per100, lett_per100)
  
  #==============================================================================;
  #Combine with data;
  #==============================================================================;
  
  Sample_Data_Statistics[i,lines_col_num]  <- as.data.frame(tagged_text_desc$lines)
  Sample_Data_Statistics[i,sentences_col_num]  <- as.data.frame(readbl_text_desc$sentences)
  Sample_Data_Statistics[i,words_col_num]  <- as.data.frame(readbl_text_desc$words)
  Sample_Data_Statistics[i,all_chars_col_num]  <- as.data.frame(readbl_text_desc$all.chars)
  Sample_Data_Statistics[i,chars_no_space_col_num]  <- as.data.frame(tagged_text_desc$chars.no.space)
  Sample_Data_Statistics[i,letters_only_col_num]  <- as.data.frame(tagged_text_desc$letters.only)
  Sample_Data_Statistics[i,digits_col_num]  <- as.data.frame(tagged_text_desc$digits)
  Sample_Data_Statistics[i,punct_col_num]  <- as.data.frame(readbl_text_desc$punct)
  Sample_Data_Statistics[i,conjunctions_col_num]  <- as.data.frame(readbl_text_desc$conjunctions)
  Sample_Data_Statistics[i,prepositions_col_num]  <- as.data.frame(readbl_text_desc$prepositions)
  Sample_Data_Statistics[i,pronouns_col_num]  <- as.data.frame(readbl_text_desc$pronouns)
  Sample_Data_Statistics[i,foreign_col_num]  <- as.data.frame(readbl_text_desc$foreign)
  Sample_Data_Statistics[i,num_syll_col_num]  <- as.data.frame(hyph_text_en_desc$num.syll)
  Sample_Data_Statistics[i,normalized_space_col_num ] <- as.data.frame(tagged_text_desc$normalized.space)
  Sample_Data_Statistics[i,Flesch_Kincaid_col_num]  <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="Flesch-Kincaid" & readbl_all_df[,2]=="",], select = c(grade)))
  Sample_Data_Statistics[i,ARI_col_num]  <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="ARI" & readbl_all_df[,2]=="",], select = c(grade)))
  Sample_Data_Statistics[i,Coleman_Liau_col_num]  <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="Coleman-Liau" & readbl_all_df[,2]=="",], select = c(grade)))
  Sample_Data_Statistics[i,SMOG_col_num]  <- as.numeric(subset(readbl_all_df[readbl_all_df[,1]=="SMOG" & readbl_all_df[,2]=="",], select = c(grade)))
  Sample_Data_Statistics[i,FOG_hard_words_col_num]  <-  as.data.frame(readbl_text_desc$FOG.hard.words)
  Sample_Data_Statistics[i,TTR_col_num]  <-  as.data.frame(readbl_text_desc$TTR)
  Sample_Data_Statistics[i,sntc_per_word_col_num]  <- as.data.frame(readbl_text_desc$sntc.per.word)
  Sample_Data_Statistics[i,avg_sentc_length_col_num]  <- as.data.frame(readbl_text_desc$avg.sentc.length)
  Sample_Data_Statistics[i,avg_word_length_col_num]  <- as.data.frame(readbl_text_desc$avg.word.length)
  Sample_Data_Statistics[i,avg_syll_word_col_num]  <- as.data.frame(readbl_text_desc$avg.syll.word)
  Sample_Data_Statistics[i,sntc_per100_col_num]  <- as.data.frame(readbl_text_desc$sntc.per100) 
  Sample_Data_Statistics[i,syll_per100_col_num]  <- as.data.frame(readbl_text_desc$syll.per100)
  Sample_Data_Statistics[i,lett_per100_col_num]  <- as.data.frame(readbl_text_desc$lett.per100)
  Sample_Data_Statistics[i,investment_strategy_col_num] <- as.character(Sample_Data[i,1])
  
  
}