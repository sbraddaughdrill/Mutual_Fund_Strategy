# TODO: Add comment
# 
# Author:  Brad
# File:    Data_Combination.R
# Version: 1.0
# Date:    03.17.2013
# Purpose: Combine all data sources
#
###############################################################################

###############################################################################
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

rm(list=ls(all=TRUE))                              #Clear workspace

Sys.setenv(R_HISTSIZE=500)                         #Limit History so that it never contains more than 50 lines

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source=FALSE)
options(StringsAsFactors=FALSE)                    #String as factors is False -- used for read.csv
#options(max.print=99999)                                   
options(max.print=500)                               #Default maxprint option

#memory.limit(size=2047)                              #Memory limit default
#memory.limit(size=3000)                              #Increase memory limit to 3000 mb (3 gb)

#Set location (HOME=1,WORK=2)
Location <- 1
#Location <- 2

if (Location==1)
{
  #HOME
  
  setwd("C:/Research_temp/")                                                                                                  #Set working directory
  input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/Data/",winslash="\\", mustWork=NA)      #Create data input directory
  output_directory <- normalizePath("C:/Research_temp/",winslash="\\", mustWork=NA)                                       #Create data output directory
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/",winslash="\\", mustWork=NA)                     #Create function directory
  #package_directory <- normalizePath("C:/Users/Brad/Documents/R/win-library/2.15/",winslash="\\", mustWork=NA)            #Create package directory
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=NA)                                          #Create treetag directory
  
} else if (Location==2)
{
  #WORK
  
  setwd("C:/Research_temp/")                                                                                                  #Set working directory
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/Data/",winslash="\\", mustWork=NA)  #Create data input directory
  output_directory <- normalizePath("C:/Research_temp/",winslash="\\", mustWork=NA)                                       #Create data output directory
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/R/",winslash="\\", mustWork=NA)                 #Create function directory
  #package_directory <- normalizePath("C:/Users/bdaughdr/Documents/R/win-library/2.15/",winslash="\\", mustWork=NA)        #Create package directory
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=NA)                                          #Create treetag directory
  
} else
{
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)

import_crsp_file_no_year <- function(table_name,fundnos){
  q_import_crsp_table <- ""
  q_import_crsp_table <- paste(q_import_crsp_table, "select       a.*                                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "from         ",table_name," as a                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "where        a.crsp_fundno in (select distinct    b.crsp_fundno           ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "                               from               ", fundnos ," as b)     ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "order by     a.crsp_fundno                                                ", sep=" ")
  q_import_crsp_table <- gsub(" {2,}", " ", q_import_crsp_table)
  table <- runsql(q_import_crsp_table,crsp_db)
  for (i in 1:ncol(table))
  {
    table[,i] <- unknownToNA(table[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
    table[,i] <- ifelse(is.na(table[,i]),NA, table[,i])
  } 
  return(table)
}

import_crsp_file_year <- function(table_name,fundnos,year_cutoff_low,year_cutoff_high){
  q_import_crsp_table <- ""
  q_import_crsp_table <- paste(q_import_crsp_table, "select       a.*                                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "from         ",table_name," as a                                          ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "where        a.crsp_fundno in (select distinct    b.crsp_fundno           ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "                               from               ", fundnos ," as b)     ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "and          yr >=  " , year_cutoff_low,"                                 ", sep=" ")
  q_import_crsp_table <- paste(q_import_crsp_table, "and          yr <=  " , year_cutoff_high,"                                ", sep=" ") 
  q_import_crsp_table <- paste(q_import_crsp_table, "order by     a.crsp_fundno                                                ", sep=" ")
  q_import_crsp_table <- gsub(" {2,}", " ", q_import_crsp_table)
  table <- runsql(q_import_crsp_table,crsp_db)
  for (i in 1:ncol(table))
  {
    table[,i] <- unknownToNA(table[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
    table[,i] <- ifelse(is.na(table[,i]),NA, table[,i])
  } 
  return(table)
}

aggregate_crsp_variable <- function(tna_data,other_variable_data,variable){
  
  #tna_data <- monthly_tna_full
  #other_variable_data <- monthly_ret
  #variable <- "mret"
  #variable <- "mnav"
  
  
  other_variable_data <- other_variable_data[,c("crsp_fundno","yr","month",variable)]
  
  temp_data <- merge(tna_data, other_variable_data, 
                     by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
  
  temp_data <- temp_data[c("wficn","crsp_fundno","yr","month","mtna",variable)]
  
  temp_data <- temp_data[order(temp_data[,"wficn"], 
                               temp_data[,"crsp_fundno"],                                         
                               temp_data[,"yr"], 
                               temp_data[,"month"]),] 
  
  temp_data_trim <- subset(temp_data,select=-c(crsp_fundno))
  
  temp_data_trim <- temp_data_trim[c("wficn","yr","month","mtna",variable)]
  
  temp_data_trim <- temp_data_trim[order(temp_data_trim[,"wficn"], 
                                         temp_data_trim[,"yr"], 
                                         temp_data_trim[,"month"]),] 
  
  #test0 <- temp_data_trim[temp_data_trim[,"wficn"]==100166 & temp_data_trim[,"yr"]==2000,]
  
  sumtna <- as.data.frame(data.table(temp_data_trim)[, list(summtna=sum(mtna)),by="wficn,yr,month"],stringsAsFactors=FALSE)
  
  #rm2(temp_data_trim)
  
  temp_data_sum_tna <- merge(temp_data[,-match(variable,names(temp_data))], sumtna, 
                             by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
  
  #rm2(sumtna)
  
  temp_data_sum_tna <- temp_data_sum_tna[c("wficn","crsp_fundno","yr","month","mtna","summtna")]
  
  temp_data_sum_tna <- temp_data_sum_tna[order(temp_data_sum_tna[,"wficn"], 
                                               temp_data_sum_tna[,"crsp_fundno"], 
                                               temp_data_sum_tna[,"yr"], 
                                               temp_data_sum_tna[,"month"]),] 
  
  #test1 <- temp_data_sum_tna[temp_data_sum_tna[,"wficn"]==100166 & temp_data_sum_tna[,"yr"]==2000,]
  
  temp_data_weights0 <- as.data.frame(data.table(temp_data_sum_tna)[, list(weight=mtna/summtna),by="crsp_fundno,yr,month"],stringsAsFactors=FALSE)
  
  #rm2(temp_data_sum_tna)
  
  temp_data_weights <- merge(subset(temp_data,select=-c(mtna)), temp_data_weights0, 
                             by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                             all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
  
  #rm2(temp_data,temp_data_weights0)
  
  temp_data_weights <- temp_data_weights[c("wficn","crsp_fundno","yr","month","weight",variable)]
  
  temp_data_weights <- temp_data_weights[order(temp_data_weights[,"wficn"], 
                                               temp_data_weights[,"crsp_fundno"], 
                                               temp_data_weights[,"yr"], 
                                               temp_data_weights[,"month"]),] 
  
  #test2 <- temp_data_weights[temp_data_weights[,"wficn"]==100166 & temp_data_weights[,"yr"]==2000,]
  
  temp_weighted <- temp_data_weights[,"weight"]* temp_data_weights[,variable]
  
  temp_data_agg1 <- cbind(temp_data_weights,temp_weighted)
  
  #test3 <- temp_data_agg1[temp_data_agg1[,"wficn"]==100166 & temp_data_agg1[,"yr"]==2000,]
  
  #rm2(temp_data_weights,temp_weighted)
  
  temp_data_agg1_trim <- temp_data_agg1[,-match(c("crsp_fundno","weight",variable),names(temp_data_agg1))]
  
  #rm2(temp_data_agg1)
  
  temp_data_agg1_trim <- temp_data_agg1_trim[order(temp_data_agg1_trim[,"wficn"], 
                                                   temp_data_agg1_trim[,"yr"], 
                                                   temp_data_agg1_trim[,"month"]),] 
  
  temp_data_agg <- as.data.frame(data.table(temp_data_agg1_trim)[, list(agg_temp=sum(temp_weighted)),
                                                                 by="wficn,yr,month"],stringsAsFactors=FALSE)
  
  colnames(temp_data_agg)[4] <- paste(variable,"_agg",sep="")
  
  #test4 <- temp_data_agg[temp_data_agg[,"wficn"]==100166 & temp_data_agg[,"yr"]==2000,]
  
  #rm2(temp_data_agg1_trim)
  
  return(temp_data_agg)
  
}

create_lags <- function(data,variable){
  
  #data <- monthly_agg
  #variable <- "mnav_agg"
  
  text0 <- paste0(variable, ",")
  text1 <- paste0(variable, "lag1=shift(",variable,",-1),")
  text2 <- paste0(variable, "lag2=shift(",variable,",-2),")
  text3 <- paste0(variable, "lag3=shift(",variable,",-3),")
  text4 <- paste0(variable, "lag4=shift(",variable,",-4)")
  str <-  paste0("list(",text0,text1,text2,text3,text4,")")
  
  #expr <- parse(text="list(mnav_agg,mnav_agglag1=shift(mnav_agg,-1),mnav_agglag2=shift(mnav_agg,-2),mnav_agglag3=shift(mnav_agg,-3),mnav_agglag4=shift(mnav_agg,-4))")
  expr <- parse(text=str)
  
  data_lags <- as.data.frame(data.table(data)[,eval(expr),by="wficn"], stringsAsFactors=FALSE)
  
  return(data.frame(data[,-match(c(variable),names(data))],data_lags[,-match(c("wficn"),names(data_lags))],stringsAsFactors=FALSE))
  
}

calculate_similarity_by_group <- function(merged_data,group_var,group_var_value,text_type){
  
  
  #i <- 1
  #i <- 4
  #j <- 1
  
  #merged_data <- temp_stacked_full
  #group_var <- "Broad_Cat_Group"
  #group_var_value <- text_group_vars[i]
  #text_type <- text_variables[j]
  
  temp_stacked <- merged_data[toupper(merged_data[,group_var])==group_var_value,]
  temp_stacked <- temp_stacked[order(temp_stacked[,"yr"],temp_stacked[,"wficn"]),] 
  
  for (k in 1:length(text_percentages))
  {
    #k <- 1
    temp_years <- get(paste("years",text_type,text_percentages[k],sep="_"))
    
    temp_avg <- data.frame(yr=integer(),group=character(),wficn=integer(),avg_similarity=numeric(),stringsAsFactors=FALSE) 
    
    for (l in 1:length(temp_years))
    {
      #l <- 1
      #l <- 10
      
      temp_input_df_name <- paste(temp_input_data_name_short,text_percentages[k],text_type,temp_years[l],sep="_")
      temp_sql_str <- paste("SELECT * FROM",temp_input_df_name,sep=" ")
      temp_data <- runsql(temp_sql_str,similarity_db)
      
      for(m in which(sapply(temp_data,class)=="character"))
      {
        temp_data[[m]] = trim(temp_data[[m]])
      }
      for (m in 1:ncol(temp_data))
      {
        temp_data[,m] <- unknownToNA(temp_data[,m], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
        temp_data[,m] <- ifelse(is.na(temp_data[,m]),NA, temp_data[,m])
      } 
      
      temp_data <- temp_data[order(temp_data[,"yr"],temp_data[,"wficn"]),] 
      
      temp_col_mat_df <- as.matrix(temp_data[,3:ncol(temp_data)])
      
      #Make diagonal NA
      diag(temp_col_mat_df) <- NA
      
      temp_id_cat <- unique(temp_stacked[,c("wficn",group_var)],comparables=FALSE)
      temp_id_cat <- temp_id_cat[!(is.na(temp_id_cat[,group_var])),]
      
      temp_data_obj <- merge(temp_id_cat, temp_data[,c("yr","wficn")], 
                             by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      #temp_data_obj <- merge(temp_id_cat, temp_data[,c("yr","wficn")], 
      #                       by.x=c("wficn"), by.y=c("wficn"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
      temp_data_obj <- temp_data_obj[order(temp_data_obj[,"yr"],temp_data_obj[,"wficn"]),] 
      
      temp_data_full <- data.frame(yr=temp_data_obj[,"yr"],
                                   group=temp_data_obj[,group_var],
                                   wficn=temp_data_obj[,"wficn"],
                                   temp_col_mat_df,stringsAsFactors=FALSE)
      
      temp_data_full <- temp_data_full[order(temp_data_full[,"yr"],temp_data_full[,"wficn"]),] 
      
      #Seperate by group
      temp_data_full_no_na <- temp_data_full[!(is.na(temp_data_full[,"group"])),]
      
      if(nrow(temp_data_full_no_na)>0)
      {
        temp_data_full_no_na_ids <- paste("X",temp_data_full_no_na[,"wficn"],sep="")
        temp_data_full_no_na_trim <- temp_data_full_no_na[,c("yr","group","wficn",temp_data_full_no_na_ids)]
        temp_data_full_no_na_trim[,"wficn"] <- as.integer(temp_data_full_no_na_trim[,"wficn"])
        
        temp_data_full_cat_temp <- temp_data_full_no_na_trim[toupper(temp_data_full_no_na_trim[,"group"])==group_var_value,]
        
        
        if(nrow(temp_data_full_cat_temp)==1)
        {
          temp_data_full_cat_temp_ids <- paste("X",temp_data_full_cat_temp[,"wficn"],sep="")
          temp_data_full_cat_temp_trim <- temp_data_full_cat_temp[,c("yr","group","wficn",temp_data_full_cat_temp_ids)]
          
          temp_data_full_cat_temp_avg <- data.frame(yr=temp_data_full_cat_temp_trim[,"yr"],
                                                    group=temp_data_full_cat_temp_trim[,"group"],
                                                    wficn=temp_data_full_cat_temp_trim[,"wficn"],
                                                    avg_similarity=temp_data_full_cat_temp_trim[,4],
                                                    stringsAsFactors=FALSE)
          
          temp_avg <- rbind(temp_avg,temp_data_full_cat_temp_avg)
          
          rm(temp_data_full_cat_temp,temp_data_full_cat_temp_ids,temp_data_full_cat_temp_trim,temp_data_full_cat_temp_avg)
          
        } else if (nrow(temp_data_full_cat_temp)>1)
        {
          temp_data_full_cat_temp_ids <- paste("X",temp_data_full_cat_temp[,"wficn"],sep="")
          temp_data_full_cat_temp_trim <- temp_data_full_cat_temp[,c("yr","group","wficn",temp_data_full_cat_temp_ids)]
          
          temp_data_full_cat_temp_avg <- data.frame(yr=temp_data_full_cat_temp_trim[,"yr"],
                                                    group=temp_data_full_cat_temp_trim[,"group"],
                                                    wficn=temp_data_full_cat_temp_trim[,"wficn"],
                                                    avg_similarity=rowMeans(temp_data_full_cat_temp_trim[,4:ncol(temp_data_full_cat_temp_trim)], na.rm = TRUE),
                                                    stringsAsFactors=FALSE)
          
          temp_avg <- rbind(temp_avg,temp_data_full_cat_temp_avg)
          
          rm(temp_data_full_cat_temp,temp_data_full_cat_temp_ids,temp_data_full_cat_temp_trim,temp_data_full_cat_temp_avg)
          
        } else
        {
          rm(temp_data_full_cat_temp)
          
        }
        
        rm(temp_input_df_name,temp_sql_str,temp_data,temp_col_mat_df,temp_id_cat,temp_data_obj,temp_data_full)
        rm(temp_data_full_no_na,temp_data_full_no_na_ids,temp_data_full_no_na_trim)
        
      } else
      {
        rm(temp_input_df_name,temp_sql_str,temp_data,temp_col_mat_df,temp_id_cat,temp_data_obj,temp_data_full)
        rm(temp_data_full_no_na)
        
      }
      
    }
    
    colnames(temp_avg)[4] <- paste("similarity",text_percentages[k],text_type,sep="_")
    
    temp_stacked <- merge(temp_stacked, subset(temp_avg,select=-c(group)), by.x=c("wficn","yr"), by.y=c("wficn","yr"), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
    
    #rm2(temp_years,temp_avg)
    
  }
  
  temp_stacked <- temp_stacked[!(rowSums(is.na(temp_stacked[,4:ncol(temp_stacked)]))==(ncol(temp_stacked)-3)),]
  temp_stacked <- temp_stacked[order(temp_stacked[,"wficn"],temp_stacked[,"yr"]),] 
  
  return(temp_stacked)
  
}

empty.df<- function(header){
  
  df<-data.frame(matrix(matrix(rep(1,length(header)),1),1))
  colnames(df)<-header
  return(df[NULL,])
  
}


###############################################################################
cat("SECTION: LIBRARIES", "\n")
###############################################################################

update.packages(ask=FALSE, checkBuilt=TRUE)

#Load External Packages
#external_packages <- c("arules","bigalgebra","biganalytics","bigmemory","bigtabulate","clv","ff",
#                       "foreach","pracma","rJava","snow","Snowball","snowfall","synchronicity","XML")
external_packages <- c("compare","cwhmisc","data.table","fastmatch","foreign","formatR","gdata",
                       "gtools","Hmisc","installr","knitr","koRpus","lmtest","lubridate","markdown","memisc","mitools",
                       "pander","pbapply","plm","plyr","psych","quantreg","R.oo","R2wd","reporttools","rms","RSQLite",
                       "sandwich","sqldf","stargazer","stringr","SWordInstaller","texreg","UsingR","xtable","zoo")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)


###############################################################################
cat("SECTION: SQLITE DATABASES", "\n")
###############################################################################

crsp_db <- paste(output_directory,"CRSPMF_Formatted.s3db",sep="")
mflinks_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")
msd_db <- paste(output_directory,"MDMF_Formatted.s3db",sep="")
similarity_db <- paste(output_directory,"Similarity_Analysis.s3db",sep="")
descriptive_stats_db <- paste(output_directory,"Descriptive_stats.s3db",sep="")


###############################################################################
cat("IMPORT READABILITY TEXT DATA", "\n")
###############################################################################

#Import .CSV files

read_stats_iois_f <- as.data.frame(fread(paste(output_directory,"read_stats_ios_f.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
for(i in which(sapply(read_stats_iois_f,class)=="character"))
{
  read_stats_iois_f[[i]] = trim(read_stats_iois_f[[i]])
}
for (i in 1:ncol(read_stats_iois_f))
{
  read_stats_iois_f[,i] <- unknownToNA(read_stats_iois_f[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  read_stats_iois_f[,i] <- ifelse(is.na(read_stats_iois_f[,i]),NA, read_stats_iois_f[,i])
}

read_stats_pr_f <- as.data.frame(fread(paste(output_directory,"read_stats_pr_f.csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
for(i in which(sapply(read_stats_pr_f,class)=="character"))
{
  read_stats_pr_f[[i]] = trim(read_stats_pr_f[[i]])
}
for (i in 1:ncol(read_stats_pr_f))
{
  read_stats_pr_f[,i] <- unknownToNA(read_stats_pr_f[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  read_stats_pr_f[,i] <- ifelse(is.na(read_stats_pr_f[,i]),NA, read_stats_pr_f[,i])
}

sample_data_all <- as.data.frame(read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)
for(i in which(sapply(sample_data_all,class)=="character"))
{
  sample_data_all[[i]] = trim(sample_data_all[[i]])
}
for (i in 1:ncol(sample_data_all))
{
  sample_data_all[,i] <- unknownToNA(sample_data_all[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  sample_data_all[,i] <- ifelse(is.na(sample_data_all[,i]),NA, sample_data_all[,i])
} 

#read_stats_iois_f <- subset(read_stats_iois_f,select=-c(FOG_hard_words_iois,foreign_iois))
#read_stats_pr_f <- subset(read_stats_pr_f,select=-c(FOG_hard_words_pr,foreign_pr))
read_stats_iois_f <- subset(read_stats_iois_f,select=-c(foreign_iois))
read_stats_pr_f <- subset(read_stats_pr_f,select=-c(foreign_pr))
sample_data_all <- subset(sample_data_all,select=-c(investment_objective_strategy_f,principal_risks_f))


###############################################################################
cat("IMPORT SIMILARITY TEXT DATA", "\n")
###############################################################################

text_variables <- c("iois","pr")
text_percentages <- c("050pct","100pct","250pct","500pct","750pct","900pct")

temp_input_data_name_short <- "year_id_unique_sim_cosine_normalized"

sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]

for (j in 1:length(text_variables))
{
  #j <- 1

  assign(paste("year_sim",text_variables[j],"all_stacked",sep="_"), sample_data_all_temp, envir=.GlobalEnv)
  
}
rm2(sample_data_all_temp)

for (i in 1:length(text_variables))
{
  #i <- 1
  
  for (j in 1:length(text_percentages))
  {
    
    #j <- 1

    temp_input_data_name_full <- paste(temp_input_data_name_short,text_percentages[j],text_variables[i],"avg",sep="_")
    
    year_sim <- as.data.frame(fread(paste(output_directory,temp_input_data_name_full,".csv",sep=""),na.strings="NA",stringsAsFactors=FALSE),stringsAsFactors=FALSE)

    #assign(paste("year_sim",text_variables[j],"all_stacked",sep="_"), sample_data_all_temp, envir=.GlobalEnv)
    
    for(k in which(sapply(year_sim,class)=="character"))
    {
      year_sim[[k]] = trim(year_sim[[k]])
    }
    for (k in 1:ncol(year_sim))
    {
      year_sim[,k] <- unknownToNA(year_sim[,k], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
      year_sim[,k] <- ifelse(is.na(year_sim[,k]),NA, year_sim[,k])
    }
    
    year_sim_years <- colnames(year_sim)
    year_sim_years <- year_sim_years[!(year_sim_years=="wficn")] 
    
    assign(paste("years",text_variables[i],text_percentages[j],sep="_"), year_sim_years, envir=.GlobalEnv)
    
    year_sim_stacked0 <- lapply(year_sim_years, function(x,data){ data.frame(wficn=data[,"wficn"], 
                                                                             yr=as.integer(x), 
                                                                             similarity_all_iois=data[,x], stringsAsFactors=FALSE) }, data = year_sim)
    
    year_sim_stacked <- do.call(rbind, year_sim_stacked0) 
    year_sim_stacked <- year_sim_stacked[order(year_sim_stacked[,"wficn"],year_sim_stacked[,"yr"]),] 
    colnames(year_sim_stacked) <- c("wficn","yr",paste("similarity",text_percentages[j],text_variables[i],sep="_"))
    
    if (text_variables[i]=="iois")
    {
      year_sim_iois_all_stacked <- merge(year_sim_iois_all_stacked, year_sim_stacked, by.x=c("wficn","yr"), by.y=c("wficn","yr"), 
                                         all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
    } else if (text_variables[i]=="pr")
    {
      year_sim_pr_all_stacked <- merge(year_sim_pr_all_stacked, year_sim_stacked, by.x=c("wficn","yr"), by.y=c("wficn","yr"), 
                                       all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_variables), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_percentages))
    
    rm2(year_sim,year_sim_years,year_sim_stacked0,year_sim_stacked)
    
  }
  
} 

year_sim_iois_all_stacked <- year_sim_iois_all_stacked[!(rowSums(is.na(year_sim_iois_all_stacked[,3:ncol(year_sim_iois_all_stacked)]))==(ncol(year_sim_iois_all_stacked)-2)),]
year_sim_iois_all_stacked <- year_sim_iois_all_stacked[order(year_sim_iois_all_stacked[,"wficn"], 
                                                             year_sim_iois_all_stacked[,"yr"]),]
colnames(year_sim_iois_all_stacked) <- paste("all",colnames(year_sim_iois_all_stacked),sep="_")
colnames(year_sim_iois_all_stacked)[1:2] <- c("wficn","yr")
  
year_sim_pr_all_stacked <- year_sim_pr_all_stacked[!(rowSums(is.na(year_sim_pr_all_stacked[,3:ncol(year_sim_pr_all_stacked)]))==(ncol(year_sim_pr_all_stacked)-2)),]
year_sim_pr_all_stacked <- year_sim_pr_all_stacked[order(year_sim_pr_all_stacked[,"wficn"], 
                                                         year_sim_pr_all_stacked[,"yr"]),]
colnames(year_sim_pr_all_stacked) <- paste("all",colnames(year_sim_pr_all_stacked),sep="_")
colnames(year_sim_pr_all_stacked)[1:2] <- c("wficn","yr")


###############################################################################
cat("IMPORT MFLINKS DATA", "\n")
###############################################################################

mflinks_tables <- ListTables(mflinks_db)
mflinks_fields <- ListFields(mflinks_db)

mflink1_fields <- mflinks_fields[mflinks_fields[,1]=="mflink1",]
mflink1 <- runsql("SELECT * FROM mflink1",mflinks_db)

for(i in which(sapply(mflink1,class)=="character"))
{
  mflink1[[i]] = trim(mflink1[[i]])
}

for (i in 1:ncol(mflink1))
{
  mflink1[,i] <- unknownToNA(mflink1[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  mflink1[,i] <- ifelse(is.na(mflink1[,i]),NA, mflink1[,i])
} 

mflink1 <- mflink1[!(is.na(mflink1[,"crsp_fundno"])),]


###############################################################################
cat("IMPORT MDMF DATA", "\n")
###############################################################################

mdmf_tables <- ListTables(msd_db)
mdmf_fields <- ListFields(msd_db)

mdmf_fields <- mdmf_fields[mdmf_fields[,1]=="Mdmf_data_raw",]
Mdmf_data_raw <- runsql("SELECT * FROM Mdmf_data_raw",msd_db)

for(i in which(sapply(Mdmf_data_raw,class)=="character"))
{
  Mdmf_data_raw[[i]] = trim(Mdmf_data_raw[[i]])
}

for (i in 1:ncol(Mdmf_data_raw))
{
  Mdmf_data_raw[,i] <- unknownToNA(Mdmf_data_raw[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  Mdmf_data_raw[,i] <- ifelse(is.na(Mdmf_data_raw[,i]),NA, Mdmf_data_raw[,i])
} 

Mdmf_data_trim <- subset(Mdmf_data_raw,select=c(CUSIP, Broad_Cat_Group, Global_Cat, MS_Cat, MS_Inst_Cat, 
                                                MS_Rating_Overall, US_Broad_Asset_Class, Equity_Style_Box_Long, MS_Anal_Rating, 
                                                Firm_Name, Branding_Name, Prospectus_Objective))
Mdmf_data_trim <- Mdmf_data_trim[order(Mdmf_data_trim[,"CUSIP"]),]



###############################################################################
cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN SMAPLE_DATA_ALL", "\n")
###############################################################################

sample_data_all <- merge(sample_data_all, mflink1, by.x=c("wficn"), by.y=c("wficn"), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

sample_data_all <- sample_data_all[c("wficn","crsp_fundno","yr")]
sample_data_all <- sample_data_all[order(sample_data_all[,"wficn"], sample_data_all[,"crsp_fundno"], sample_data_all[,"yr"]),]


###############################################################################
cat("IMPORT CRSP DATA", "\n")
###############################################################################

crsp_fundno_unique <- data.frame(crsp_fundno=unique(sample_data_all[,c("crsp_fundno")], incomparables=FALSE),stringsAsFactors=FALSE)

ExportTable(crsp_db,"crsp_fundno_unique",crsp_fundno_unique)

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)

Crspa_msi <- runsql("SELECT * FROM Crspa_msi",crsp_db)

for(i in which(sapply(Crspa_msi,class)=="character"))
{
  Crspa_msi[[i]] = trim(Crspa_msi[[i]])
}

for (i in 1:ncol(Crspa_msi))
{
  Crspa_msi[,i] <- unknownToNA(Crspa_msi[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  Crspa_msi[,i] <- ifelse(is.na(Crspa_msi[,i]),NA, Crspa_msi[,i])
} 

#crsp_tables_to_import <- c("fund_hdr", "fund_hdr_hist", "Fund_names", "fund_summary", "fund_summary2","monthly_tna_ret_nav")
crsp_tables_no_year_import <- c("Daily_returns","fund_hdr","fund_hdr_hist","Fund_names","monthly_tna_ret_nav")
for (i in 1:length(crsp_tables_no_year_import))
{
  
  assign(tolower(crsp_tables_no_year_import[i]),import_crsp_file_no_year(crsp_tables_no_year_import[i],"crsp_fundno_unique"), envir=.GlobalEnv)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(crsp_tables_no_year_import), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 

crsp_tables_year_import <- c("fund_fees_month","fund_style_month")
for (i in 1:length(crsp_tables_year_import))
{
  
  assign(tolower(crsp_tables_year_import[i]),import_crsp_file_year(crsp_tables_year_import[i],"crsp_fundno_unique","1998","2012"), envir=.GlobalEnv)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(crsp_tables_year_import), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
} 

DeleteTable(crsp_db,"crsp_fundno_unique")

crsp_tables <- ListTables(crsp_db)
crsp_fields <- ListFields(crsp_db)


###############################################################################
cat("CLEAN FUND_STYLE_MONTH", "\n")
###############################################################################

fund_style_month <- subset(fund_style_month,select=-c(si_obj_cd,wbrger_obj_cd,policy))

fund_style_upcase <- c("crsp_obj_cd","accrual_fund","sales_restrict","lipper_class","lipper_class_name",
                       "lipper_obj_cd","lipper_obj_name","lipper_asset_cd","lipper_tax_cd")
for (i in 1:length(fund_style_upcase))
{
  #i <- 1
  fund_style_month[,fund_style_upcase[i]] <-  toupper(fund_style_month[,fund_style_upcase[i]])

} 


###############################################################################
cat("CLEAN DAILY_RETURNS", "\n")
###############################################################################

Daily_returns_num_to_pad_cols <- c("crsp_fundno")
for (i in 1:length(Daily_returns_num_to_pad_cols))
{
  daily_returns[,Daily_returns_num_to_pad_cols[i]] <- paste("", formatC(as.integer(daily_returns[,Daily_returns_num_to_pad_cols[i]]), width=6, format="d", flag="0"), sep = "")
  daily_returns[,Daily_returns_num_to_pad_cols[i]] <- trim(daily_returns[,Daily_returns_num_to_pad_cols[i]])
} 

Daily_returns_num_to_date_cols <- c("caldt")
for (i in 1:length(Daily_returns_num_to_date_cols))
{
  daily_returns[,Daily_returns_num_to_date_cols[i]] <- as.character(as.Date(as.integer(daily_returns[,Daily_returns_num_to_date_cols[i]]), origin="1960-01-01"))
  daily_returns[,Daily_returns_num_to_date_cols[i]] <- ifelse(daily_returns[,Daily_returns_num_to_date_cols[i]]=="0",NA, daily_returns[,Daily_returns_num_to_date_cols[i]])
  daily_returns[,Daily_returns_num_to_date_cols[i]] <- trim(daily_returns[,Daily_returns_num_to_date_cols[i]])
} 


for(i in which(sapply(daily_returns,class)=="character"))
{
  daily_returns[[i]] = trim(daily_returns[[i]])
}
for (i in 1:ncol(daily_returns))
{
  daily_returns[,i] <- unknownToNA(daily_returns[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  daily_returns[,i] <- ifelse(is.na(daily_returns[,i]),NA, daily_returns[,i])
} 



###############################################################################
cat("CODE MISSING VARIABLES AS NA", "\n")
###############################################################################

fund_fees_month[,"actual_12b1"] <- ifelse((fund_fees_month$actual_12b1==0 & fund_fees_month$yr<1998), NA, fund_fees_month$actual_12b1)
fund_fees_month[,"actual_12b1"] <- ifelse((fund_fees_month$actual_12b1==-99), NA, fund_fees_month$actual_12b1)

fund_fees_month[,"max_12b1"]   <- ifelse((fund_fees_month$max_12b1==-99), NA, fund_fees_month$max_12b1)

fund_fees_month[,"exp_ratio"]  <- ifelse((fund_fees_month$exp_ratio==-99), NA, fund_fees_month$exp_ratio)

fund_fees_month[,"mgmt_fee"]   <- ifelse((fund_fees_month$mgmt_fee==-99), NA, fund_fees_month$mgmt_fee)

fund_fees_month[,"turn_ratio"] <- ifelse((fund_fees_month$turn_ratio==-99), NA, fund_fees_month$turn_ratio)

monthly_tna_ret_nav[,"mtna"] <- ifelse((monthly_tna_ret_nav$mtna==-99), NA, monthly_tna_ret_nav$mtna)

monthly_tna_ret_nav[,"mret"] <- ifelse((monthly_tna_ret_nav$mret==-99), NA, monthly_tna_ret_nav$mret)

monthly_tna_ret_nav[,"mnav"] <- ifelse((monthly_tna_ret_nav$mnav==-99), NA, monthly_tna_ret_nav$mnav)

daily_returns[,"dret"] <- ifelse((daily_returns$dret==-99), NA, daily_returns$dret)

Crspa_msi[,"VWRETD"] <- ifelse((Crspa_msi$VWRETD==-99), NA, Crspa_msi$VWRETD)

Crspa_msi[,"VWRETX"] <- ifelse((Crspa_msi$VWRETX==-99), NA, Crspa_msi$VWRETX)

###############################################################################
cat("CREATE YEAR AND MONTH VARIABLE FOR Crspa_msi", "\n")
###############################################################################

crspa_msi_full <- transform(Crspa_msi,yr=year(as.IDate(CALDT)),month=month(as.IDate(CALDT)))

#crspa_msi_trim <- subset(crspa_msi_full,select=c(yr,month,VWRETD,VWRETX))
crspa_msi_trim <- crspa_msi_full[,(colnames(crspa_msi_full) %in% c("yr","month","vwretd","vwretx"))]

crspa_msi_trim <- data.frame(crspa_msi_trim,
                             vwretd_annualized=crspa_msi_trim[,c("vwretd")],
                             vwretx_annualized=crspa_msi_trim[,c("vwretx")],
                             stringsAsFactors=FALSE)

crspa_msi_trim[,c("vwretd")] <- (((crspa_msi_trim[,c("vwretd")]+1)^(1/12))-1)
crspa_msi_trim[,c("vwretx")] <- (((crspa_msi_trim[,c("vwretx")]+1)^(1/12))-1)

crspa_msi_trim <- crspa_msi_trim[,c("yr","month","vwretd_annualized","vwretd","vwretx_annualized","vwretx")]


###############################################################################
cat("COMPUTE MONTHLY FUND RETURN VOLATILITY", "\n")
###############################################################################

daily_returns_full <- transform(daily_returns,yr=year(as.IDate(caldt)),month=month(as.IDate(caldt)))

daily_returns_trim <- subset(daily_returns_full,select=-c(caldt))

fund_mret_volatility <- ddply(daily_returns_trim, c("crsp_fundno","yr","month"), summarize, sddret = sd(dret, na.rm = TRUE))


###############################################################################
cat("CREATE YEAR AND MONTH VARIABLE FOR monthly_tna_ret_nav", "\n")
###############################################################################

monthly_tna_ret_nav <- monthly_tna_ret_nav[c("crsp_fundno","caldt","mnav","mtna","mret")]
monthly_tna_ret_nav <- monthly_tna_ret_nav[order(monthly_tna_ret_nav[,"crsp_fundno"], monthly_tna_ret_nav[,"caldt"]),] 

monthly_tna_ret_nav2 <- data.frame(monthly_tna_ret_nav[,c("crsp_fundno","caldt")],
                                   yr=monthly_tna_ret_nav[,"caldt"],
                                   month=monthly_tna_ret_nav[,"caldt"], 
                                   monthly_tna_ret_nav[,c("mnav","mtna","mret")],stringsAsFactors=FALSE)

monthly_tna_ret_nav2[,"yr"] <- as.Date(monthly_tna_ret_nav2[,"yr"],format="%Y-%m-%d")
monthly_tna_ret_nav2[,"yr"] <- as.integer(format(monthly_tna_ret_nav2[,"yr"], "%Y"))

monthly_tna_ret_nav2[,"month"] <- as.Date(monthly_tna_ret_nav2[,"month"],format="%Y-%m-%d")
monthly_tna_ret_nav2[,"month"] <- as.integer(format(monthly_tna_ret_nav2[,"month"], "%m"))

monthly_tna_ret_nav2 <- monthly_tna_ret_nav2[order(monthly_tna_ret_nav2[,"crsp_fundno"], 
                                                   monthly_tna_ret_nav2[,"yr"],
                                                   monthly_tna_ret_nav2[,"month"]),] 

rm2(monthly_tna_ret_nav)


###############################################################################
cat("MERGE MONTHLY_TNA_RET_NAV2 AND INDEX RETURNS/FUND VOLATILITY", "\n")
###############################################################################

monthly_tna_ret_nav2 <- merge(monthly_tna_ret_nav2, crspa_msi_trim, 
                              by.x=c("yr","month"), by.y=c("yr","month"), 
                              all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_tna_ret_nav2 <- merge(monthly_tna_ret_nav2, fund_mret_volatility, 
                              by.x=c("crsp_fundno","yr","month"), by.y=c("crsp_fundno","yr","month"), 
                              all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_tna_ret_nav2 <- monthly_tna_ret_nav2[order(monthly_tna_ret_nav2[,"crsp_fundno"], 
                                                   monthly_tna_ret_nav2[,"yr"],
                                                   monthly_tna_ret_nav2[,"month"]),] 

#monthly_tna_ret_nav2 <- transform(monthly_tna_ret_nav2, mktadjret=mret-VWRETX)
monthly_tna_ret_nav2 <- transform(monthly_tna_ret_nav2, mktadjret=mret-VWRETD)


###############################################################################
cat("GET TNA FOR EACH CRSP_FUNDNO", "\n")
###############################################################################

monthly_tna_full <- merge(mflink1, monthly_tna_ret_nav2, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
                                  all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_tna_full <- subset(monthly_tna_full,select=-c(caldt,mnav,mret))

monthly_tna_full <- monthly_tna_full[c("wficn","crsp_fundno","yr","month","mtna")]

monthly_tna_full <- monthly_tna_full[order(monthly_tna_full[,"wficn"], 
                                           monthly_tna_full[,"crsp_fundno"], 
                                           monthly_tna_full[,"yr"],
                                           monthly_tna_full[,"month"]),]

monthly_tna_full <- monthly_tna_full[!(is.na(monthly_tna_full[,"mtna"])),]
monthly_tna_full <- monthly_tna_full[monthly_tna_full[,"mtna"]>0.1,]

#TRIM TNA
TNA_q <- 0.005
TNA_extrema <- quantile(monthly_tna_full[,"mtna"], c(TNA_q, 1-TNA_q)) 
#monthly_tna_full[,"mtna"] <- winsorize_top(monthly_tna_full[,"mtna"],q=0.005)
monthly_tna_full <- monthly_tna_full[monthly_tna_full[,"mtna"]<TNA_extrema[2],]

#rm2(TNA_q,monthly_tna_full)

###############################################################################
cat("AGGREGATE VALUES OF MONTHLY_TNA_RET_NAV VARIABLES FOR EACH WFICN", "\n")
###############################################################################

monthly_tna_ret_nav2_vars <- c("mret","mnav","sddret","mktadjret")
for (i in 1:length(monthly_tna_ret_nav2_vars))
{
  #i <- 1
  #i <- 2
  monthly_tna_ret_nav2[,monthly_tna_ret_nav2_vars[i]] <- winsorize_both(monthly_tna_ret_nav2[,monthly_tna_ret_nav2_vars[i]],q=0.005)
  
} 

for (i in 1:length(monthly_tna_ret_nav2_vars))
{
  #i <- 1
  #i <- 2
  
  
  single_var  <- monthly_tna_ret_nav2[,match(c("crsp_fundno","yr","month",monthly_tna_ret_nav2_vars[i]),names(monthly_tna_ret_nav2))]
  
  single_var <- single_var[order(single_var[,"crsp_fundno"],single_var[,"yr"],single_var[,"month"]),]
  
  single_var <- single_var[!(is.na(single_var[,monthly_tna_ret_nav2_vars[i]])),]
  
  assign(paste("agg_",monthly_tna_ret_nav2_vars[i],sep=""),aggregate_crsp_variable(monthly_tna_full,single_var,monthly_tna_ret_nav2_vars[i]), envir=.GlobalEnv)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(monthly_tna_ret_nav2_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  rm2(single_var)
  
} 


###############################################################################
cat("AGGREGATE VALUES OF FUND_FEES_MONTH VARIABLES FOR EACH WFICN", "\n")
###############################################################################

fund_fees_month_vars <- c("actual_12b1", "max_12b1", "exp_ratio", "mgmt_fee", "turn_ratio")

for (i in 1:length(fund_fees_month_vars))
{
  #i <- 1
  #i <- 2
  fund_fees_month[,fund_fees_month_vars[i]] <- winsorize_both(fund_fees_month[,fund_fees_month_vars[i]],q=0.005)
  
} 

for (i in 1:length(fund_fees_month_vars))
{
  #i <- 1
  #i <- 2
  
  
  single_var  <- fund_fees_month[,match(c("crsp_fundno","yr","month",fund_fees_month_vars[i]),names(fund_fees_month))]
  
  single_var <- single_var[order(single_var[,"crsp_fundno"],single_var[,"yr"],single_var[,"month"]),]
  
  single_var <- single_var[!(is.na(single_var[,fund_fees_month_vars[i]])),]
  
  assign(paste("agg_",fund_fees_month_vars[i],sep=""),aggregate_crsp_variable(monthly_tna_full,single_var,fund_fees_month_vars[i]), envir=.GlobalEnv)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(fund_fees_month_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  rm2(single_var)
  
} 

###############################################################################
cat("AGGREGATE VALUES OF TNA VARIABLES FOR EACH WFICN", "\n")
###############################################################################

monthly_tna_trim <- subset(monthly_tna_full,select=-c(crsp_fundno))

monthly_tna_trim <- monthly_tna_trim[c("wficn","yr","month","mtna")]

monthly_tna_trim <- monthly_tna_trim[order(monthly_tna_trim[,"wficn"], 
                                           monthly_tna_trim[,"yr"], 
                                           monthly_tna_trim[,"month"]),] 

agg_mtna <- as.data.frame(data.table(monthly_tna_trim)[, list(summtna=sum(mtna)),by="wficn,yr,month"],stringsAsFactors=FALSE)
colnames(agg_mtna)[4] <- "mtna_agg"

#rm2(monthly_tna_trim)


###############################################################################
cat("MERGE AGGREGATE DATA", "\n")
###############################################################################

monthly_agg <- merge(agg_mtna, agg_mret, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_mnav, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_mktadjret, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_sddret, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_actual_12b1, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_max_12b1, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_exp_ratio, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_mgmt_fee, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg <- merge(monthly_agg, agg_turn_ratio, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

#rm2(agg_mret,agg_mnav,agg_mktadjret,agg_sddret,agg_actual_12b1,agg_max_12b1,agg_exp_ratio,agg_mgmt_fee,agg_turn_ratio)


###############################################################################
cat("CREATE LAG OF TNA AND RETURNS", "\n")
###############################################################################

monthly_agg_lags <- monthly_agg
monthly_agg_lags <- create_lags(monthly_agg_lags,"mnav_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"mtna_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"mret_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"mktadjret_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"sddret_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"actual_12b1_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"max_12b1_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"exp_ratio_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"mgmt_fee_agg")
monthly_agg_lags <- create_lags(monthly_agg_lags,"turn_ratio_agg")

monthly_agg_lags <- monthly_agg_lags[order(monthly_agg_lags[,"wficn"], 
                                           monthly_agg_lags[,"yr"], 
                                           monthly_agg_lags[,"month"]),] 

#rm2(monthly_agg)


###############################################################################
cat("CREATE FLOW AND LAG FLOW", "\n")
###############################################################################

net_flow <- as.data.frame(data.table(monthly_agg_lags)[,list(net_flow=mtna_agg-(mtna_agglag1*(1+mret_agg))),by="wficn"], stringsAsFactors=FALSE) 
net_flow_lags <- as.data.frame(data.table(net_flow)[,list(net_flow,
                                                          nflowlag1=shift(net_flow,-1),
                                                          nflowlag2=shift(net_flow,-2),
                                                          nflowlag3=shift(net_flow,-3),
                                                          nflowlag4=shift(net_flow,-4)),by="wficn"], stringsAsFactors=FALSE) 

pct_flow <- as.data.frame(data.table(monthly_agg_lags)[,list(pct_flow=((mtna_agg-(mtna_agglag1*(1+mret_agg)))/mtna_agglag1)),by="wficn"], stringsAsFactors=FALSE) 
pct_flow_lags <- as.data.frame(data.table(pct_flow)[,list(pct_flow,
                                                          pflowlag1=shift(pct_flow,-1),
                                                          pflowlag2=shift(pct_flow,-2),
                                                          pflowlag3=shift(pct_flow,-3),
                                                          pflowlag4=shift(pct_flow,-4)),by="wficn"], stringsAsFactors=FALSE) 

monthly_agg_lags_full <- data.frame(monthly_agg_lags,
                                    net_flow_lags[,c("net_flow","nflowlag1","nflowlag2","nflowlag3","nflowlag4")], 
                                    pct_flow_lags[,c("pct_flow","pflowlag1","pflowlag2","pflowlag3","pflowlag4")],stringsAsFactors=FALSE)

monthly_agg_lags_full <- monthly_agg_lags_full[order(monthly_agg_lags_full[,"wficn"], 
                                                     monthly_agg_lags_full[,"yr"],
                                                     monthly_agg_lags_full[,"month"]),]

#rm2(monthly_agg_lags,net_flow,net_flow_lags,pct_flow,pct_flow_lags)


###############################################################################
cat("COMPUTE ANNUAL FUND FLOW VOLATILITY", "\n")
###############################################################################

annual_flow_trim <- subset(monthly_agg_lags_full,select=c(wficn,yr,net_flow,pct_flow))

fund_aflow_volatility <- ddply(annual_flow_trim, c("wficn","yr"), summarize, 
                               sdnet_flow=sd(net_flow, na.rm = TRUE),sdpct_flow=sd(pct_flow, na.rm = TRUE))

fund_aflow_volatility_full <- data.frame(fund_aflow_volatility,
                                         sdnet_flowlag1=shift(fund_aflow_volatility$sdnet_flow,-1),
                                         sdpct_flowlag1=shift(fund_aflow_volatility$sdpct_flow,-1),stringsAsFactors=FALSE)

monthly_agg_lags_full <- merge(monthly_agg_lags_full, fund_aflow_volatility_full, by.x=c("wficn","yr"), by.y=c("wficn","yr"), 
                               all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(annual_flow_trim,fund_aflow_volatility)


###############################################################################
cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN FUND_NAMES", "\n")
###############################################################################

fund_names_full <- merge(fund_names, mflink1, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
                         all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

fund_names_full <- subset(fund_names_full,select=-c(cusip8, crsp_portno,crsp_cl_grp,fund_name, nasdaq, ncusip, first_offer_dt, mgmt_name, mgmt_cd, mgr_name, 
                                                    mgr_dt, adv_name, index_fund_flag, et_flag, end_dt, merge_fundno,delist_cd,header))

fund_names_full <- fund_names_full[c("wficn","crsp_fundno","chgdt","chgenddt","open_to_inv","retail_fund","inst_fund","m_fund","vau_fund",
                                     "dead_flag")]

fund_names_full <- fund_names_full[order(fund_names_full[,"wficn"], fund_names_full[,"crsp_fundno"]),]

#rm2(fund_names)


###############################################################################
cat("CHANGE Y/N TO BINARY IN FUND_NAMES_FULL", "\n")
###############################################################################

fund_names_full_dv <- data.frame(fund_names_full,retail_fund_dv=NA,open_to_inv_dv=NA,inst_fund_dv=NA, 
                                 m_fund_dv=NA,vau_fund_dv=NA,dead_flag_dv=NA,stringsAsFactors=FALSE)

fund_names_full_dv[,"open_to_inv_dv"] <- ifelse(fund_names_full_dv$open_to_inv=="Y", 1, fund_names_full_dv$open_to_inv_dv)
fund_names_full_dv[,"open_to_inv_dv"] <- ifelse(fund_names_full_dv$open_to_inv=="N", 0, fund_names_full_dv$open_to_inv_dv)

fund_names_full_dv[,"retail_fund_dv"] <- ifelse(fund_names_full_dv$retail_fund=="Y", 1, fund_names_full_dv$retail_fund_dv)
fund_names_full_dv[,"retail_fund_dv"] <- ifelse(fund_names_full_dv$retail_fund=="N", 0, fund_names_full_dv$retail_fund_dv)

fund_names_full_dv[,"inst_fund_dv"] <- ifelse(fund_names_full_dv$inst_fund=="Y", 1, fund_names_full_dv$inst_fund_dv)
fund_names_full_dv[,"inst_fund_dv"] <- ifelse(fund_names_full_dv$inst_fund=="N", 0, fund_names_full_dv$inst_fund_dv)

fund_names_full_dv[,"m_fund_dv"] <- ifelse(fund_names_full_dv$m_fund=="Y", 1, fund_names_full_dv$m_fund_dv)
fund_names_full_dv[,"m_fund_dv"] <- ifelse(fund_names_full_dv$m_fund=="N", 0, fund_names_full_dv$m_fund_dv)

fund_names_full_dv[,"vau_fund_dv"] <- ifelse(fund_names_full_dv$vau_fund=="Y", 1, fund_names_full_dv$vau_fund_dv)
fund_names_full_dv[,"vau_fund_dv"] <- ifelse(fund_names_full_dv$vau_fund=="N", 0, fund_names_full_dv$vau_fund_dv)

fund_names_full_dv[,"dead_flag_dv"] <- ifelse(fund_names_full_dv$dead_flag=="Y", 1, fund_names_full_dv$dead_flag_dv)
fund_names_full_dv[,"dead_flag_dv"] <- ifelse(fund_names_full_dv$dead_flag=="N", 0, fund_names_full_dv$dead_flag_dv)

fund_names_full_dv <- subset(fund_names_full_dv,select=-c(retail_fund,open_to_inv,inst_fund,m_fund,vau_fund,dead_flag))

#rm2(fund_names_full)


###############################################################################
cat("EXPAND FUND NAMES", "\n")
###############################################################################

fund_names_full_dv2 <- transform(fund_names_full_dv, chgdt=as.IDate(chgdt), chgenddt=as.IDate(chgenddt))

fund_names_full_dv2 <- fund_names_full_dv2[(fund_names_full_dv2[,"chgdt"]>0 & 
                                              fund_names_full_dv2[,"chgenddt"]>0),]
fund_names_full_dv2 <- fund_names_full_dv2[!(fund_names_full_dv2[,"chgdt"]>=fund_names_full_dv2[,"chgenddt"]),]

fund_names_month_temp <- data.table(fund_names_full_dv2)[,{s=seq(chgdt,chgenddt,"days");list(yr=year(unlist(s)),month=month(unlist(s)))},
                                                         by="crsp_fundno,chgdt,chgenddt"]

#rm2(fund_names_full_dv)

setkeyv(fund_names_month_temp,c("crsp_fundno","yr","month"))
fund_names_month_temp <- fund_names_month_temp[unique(fund_names_month_temp[,key(fund_names_month_temp), with = FALSE]), mult = 'first']

fund_names_month_temp <- as.data.frame(fund_names_month_temp,stringsAsFactors=FALSE) 

fund_names_month <- merge(fund_names_month_temp, fund_names_full_dv2, by.x=c("crsp_fundno","chgdt","chgenddt"), by.y=c("crsp_fundno","chgdt","chgenddt"), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(fund_names_month_temp,fund_names_full_dv2)

fund_names_month <- transform(fund_names_month, chgdt=as.character(chgdt), chgenddt=as.character(chgenddt))

fund_names_month <- fund_names_month[c("wficn","crsp_fundno","chgdt","chgenddt","yr","month",
                                       "retail_fund_dv","open_to_inv_dv","inst_fund_dv","m_fund_dv","vau_fund_dv","dead_flag_dv")]

fund_names_month <- fund_names_month[order(fund_names_month[,"wficn"],fund_names_month[,"crsp_fundno"],
                                           fund_names_month[,"yr"],fund_names_month[,"month"]),] 


###############################################################################
cat("AGGREGATE VALUES OF FUND_NAMES_MONTH VARIABLES FOR EACH WFICN", "\n")
###############################################################################

fund_names_month_vars <- c("retail_fund_dv","open_to_inv_dv","inst_fund_dv","m_fund_dv","vau_fund_dv","dead_flag_dv")
for (i in 1:length(fund_names_month_vars))
{
  #i <- 1
  #i <- 2
  
  single_var  <- fund_names_month[,match(c("crsp_fundno","yr","month",fund_names_month_vars[i]),names(fund_names_month))]
  
  single_var <- single_var[order(single_var[,"crsp_fundno"],single_var[,"yr"],single_var[,"month"]),]
  
  single_var <- single_var[!(is.na(single_var[,fund_names_month_vars[i]])),]
  
  assign(paste("agg_",fund_names_month_vars[i],sep=""),aggregate_crsp_variable(monthly_tna_full,single_var,fund_names_month_vars[i]), envir=.GlobalEnv)
  
  progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(fund_names_month_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  rm2(single_var)

} 


###############################################################################
cat("FIND AGE FOR EVERY WFICN", "\n")
###############################################################################

fund_age <- data.table(fund_names_full)[,list(chgdt=min(chgdt)),by="wficn,crsp_fundno"]
fund_age <- data.frame(fund_age,stringsAsFactors=FALSE)

fund_age_trim <- data.table(fund_age[,-match("crsp_fundno",names(fund_age))])[,list(chgdt=min(chgdt)),by="wficn"]

#rm2(fund_age)

fund_age_trim <- as.data.frame(fund_age_trim,stringsAsFactors=FALSE)


fund_age_trim2 <- transform(fund_age_trim, chgdt=as.IDate(chgdt))

rm2(fund_age_trim)

fund_age_month_temp <- data.table(fund_age_trim2)[,{s=seq(chgdt,today(),"days");list(yr=year(unlist(s)),month=month(unlist(s)))},
                                                  by="wficn,chgdt"]

rm2(fund_age_trim2)

fund_age_month_temp <- as.data.frame(fund_age_month_temp,stringsAsFactors=FALSE)

fund_age_month_temp <- unique(fund_age_month_temp, incomparables=FALSE)

fund_age_month_temp <- transform(fund_age_month_temp, chgdt=as.character(chgdt))

fund_age_month <- data.table(fund_age_month_temp)[,list(yr,month,chgdt,age_m=(seq(1,.N)-1),age_y=((seq(1,.N)-1)/12)),by="wficn"]
fund_age_month <- as.data.frame(fund_age_month,stringsAsFactors=FALSE)

rm2(fund_age_month_temp)


###############################################################################
cat("MERGE AGGREGATE DATA", "\n")
###############################################################################

monthly_agg2 <- merge(agg_mtna, agg_retail_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg2 <- merge(monthly_agg2, agg_open_to_inv_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg2 <- merge(monthly_agg2, agg_inst_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg2 <- merge(monthly_agg2, agg_m_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg2 <- merge(monthly_agg2, agg_vau_fund_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg2 <- merge(monthly_agg2, agg_dead_flag_dv, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                     all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg2 <- merge(monthly_agg2, fund_age_month, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                      all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

rm2(agg_retail_fund_dv,agg_open_to_inv_dv,agg_inst_fund_dv,agg_m_fund_dv,agg_vau_fund_dv,agg_dead_flag_dv)

monthly_agg2 <- subset(monthly_agg2,select=-c(mtna_agg))


###############################################################################
cat("MERGE MONTHLY_AGG_LAGS_FULL AND MONTHLY_AGG2", "\n")
###############################################################################

monthly_agg_full <- merge(monthly_agg_lags_full, monthly_agg2, by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg_full <- monthly_agg_full[order(monthly_agg_full[,"wficn"],
                                           monthly_agg_full[,"yr"],
                                           monthly_agg_full[,"month"]),] 

#rm2(monthly_agg_lags_full,monthly_agg2)


###############################################################################
cat("GET CRSP_FUNDNOS FOR EVERY WFICN IN FUND_STYLE_MONTH", "\n")
###############################################################################

fund_style_full <- merge(mflink1,fund_style_month, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
                         all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

fund_style_full_trim <- subset(fund_style_full,select=-c(crsp_fundno,begdt,enddt,lipper_class_name,accrual_fund,sales_restrict))

fund_style_full_trim <- fund_style_full_trim[c("wficn","yr","month","crsp_obj_cd","lipper_class","lipper_obj_cd","lipper_obj_name","lipper_asset_cd","lipper_tax_cd")]

fund_style_full_trim <- fund_style_full_trim[order(fund_style_full_trim[,"wficn"], fund_style_full_trim[,"yr"], fund_style_full_trim[,"month"]),]

fund_style_full_trim <- unique(fund_style_full_trim, incomparables=FALSE)

fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"crsp_obj_cd"])),]
fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_class"])),]
fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_obj_cd"])),]
fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_obj_name"])),]
fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_asset_cd"])),]
fund_style_full_trim <- fund_style_full_trim[!(is.na(fund_style_full_trim[,"lipper_tax_cd"])),]

query_remove_styles <- "select distinct   wficn,yr, month, Count(wficn) Count
                        from              fund_style_full_trim 
                        group by          wficn, yr, month
                        order by          wficn, yr, month"
remove_styles <- sqldf(query_remove_styles)

remove_styles2 <- remove_styles[!(remove_styles[,"Count"]==1),]

colnames(remove_styles2)[match("Count",names(remove_styles2))] <- "Remove"
remove_styles2[,"Remove"] <- 1

fund_style_full_trim2 <- merge(fund_style_full_trim,remove_styles2, 
                        by.x=c("wficn", "yr", "month"), by.y=c("wficn", "yr", "month"), 
                        all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

fund_style_full_trim3 <- fund_style_full_trim2[is.na(fund_style_full_trim2[,"Remove"]),]
fund_style_full_trim3 <- subset(fund_style_full_trim3,select=-c(Remove))

for(i in which(sapply(fund_style_full_trim3,class)=="character"))
{
  fund_style_full_trim3[[i]] = trim(fund_style_full_trim3[[i]])
}
for (i in 1:ncol(fund_style_full_trim3))
{
  #i <- 1
  fund_style_full_trim3[,i] <- unknownToNA(fund_style_full_trim3[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  fund_style_full_trim3[,i] <- ifelse(is.na(fund_style_full_trim3[,i]),NA, fund_style_full_trim3[,i])
} 

#rm2(fund_style_full,fund_style_full_trim,query_remove_styles,remove_styles,remove_styles2,fund_style_full_trim2)


###############################################################################
cat("MERGE FUND_STYLE_MONTH INTO MONTHLY_AGG_LAGS_FULL", "\n")
###############################################################################

monthly_agg_full2 <- merge(monthly_agg_full, fund_style_full_trim3, 
                          by.x=c("wficn","yr","month"), by.y=c("wficn","yr","month"), 
                          all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_agg_full2 <- monthly_agg_full2[order(monthly_agg_full2[,"wficn"],
                                             monthly_agg_full2[,"yr"],
                                             monthly_agg_full2[,"month"]),] 

#rm2(monthly_agg_full,fund_style_full_trim3)


###############################################################################
cat("TRIM YEARS IN MONTHLY_AGG_FULL", "\n")
###############################################################################

monthly_agg_full2_trim <- monthly_agg_full2[(monthly_agg_full2[,"yr"]>=1999 & monthly_agg_full2[,"yr"]<=2012),]


###############################################################################
cat("CREATE LOG ASSETS, SQUARE RETURNS, AND STLYE DUMMIES", "\n")
###############################################################################

monthly_data_all2 <- data.frame(monthly_agg_full2_trim,log_mtna_agg=NA,
                                log_mtna_agglag1=NA,log_mtna_agglag2=NA,log_mtna_agglag3=NA,log_mtna_agglag4=NA,
                                mktadjret_agg_sq=NA,
                                mktadjret_agglag1_sq=NA,mktadjret_agglag2_sq=NA,mktadjret_agglag3_sq=NA,mktadjret_agglag4_sq=NA,
                                avg_mkt_exp=NA,
                                avg_mkt_explag1=NA,avg_mkt_explag2=NA,avg_mkt_explag3=NA,avg_mkt_explag4=NA,
                                lipper_asset_cd_eq_dv=NA, lipper_asset_cd_tx_dv=NA, lipper_asset_cd_mb_dv=NA, 
                                lipper_tax_cd_t_dv = NA, lipper_tax_cd_te_dv = NA)

monthly_data_all2[,"log_mtna_agg"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agg"]))
monthly_data_all2[,"log_mtna_agglag1"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag1"]))
monthly_data_all2[,"log_mtna_agglag2"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag2"]))
monthly_data_all2[,"log_mtna_agglag3"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag3"]))
monthly_data_all2[,"log_mtna_agglag4"] <- suppressWarnings(log(monthly_data_all2[,"mtna_agglag4"]))

monthly_data_all2[,"mktadjret_agg_sq"] <- (monthly_data_all2[,"mktadjret_agg"])^2
monthly_data_all2[,"mktadjret_agglag1_sq"] <- (monthly_data_all2[,"mktadjret_agglag1"])^2
monthly_data_all2[,"mktadjret_agglag2_sq"] <- (monthly_data_all2[,"mktadjret_agglag2"])^2
monthly_data_all2[,"mktadjret_agglag3_sq"] <- (monthly_data_all2[,"mktadjret_agglag3"])^2
monthly_data_all2[,"mktadjret_agglag4_sq"] <- (monthly_data_all2[,"mktadjret_agglag4"])^2

monthly_data_all2[,"avg_mkt_exp"] <- (monthly_data_all2[,"actual_12b1_agg"]/monthly_data_all2[,"exp_ratio_agg"])
monthly_data_all2[,"avg_mkt_explag1"] <- (monthly_data_all2[,"actual_12b1_agglag1"]/monthly_data_all2[,"exp_ratio_agglag1"])
monthly_data_all2[,"avg_mkt_explag2"] <- (monthly_data_all2[,"actual_12b1_agglag2"]/monthly_data_all2[,"exp_ratio_agglag2"])
monthly_data_all2[,"avg_mkt_explag3"] <- (monthly_data_all2[,"actual_12b1_agglag3"]/monthly_data_all2[,"exp_ratio_agglag3"])
monthly_data_all2[,"avg_mkt_explag4"] <- (monthly_data_all2[,"actual_12b1_agglag4"]/monthly_data_all2[,"exp_ratio_agglag4"])

#lipper_class_u <- unique(monthly_data_all2[,"lipper_class"],incomparables=FALSE)
#lipper_obj_cd_u <- unique(monthly_data_all2[,"lipper_obj_cd"],incomparables=FALSE)
#lipper_asset_cd_u <- unique(monthly_data_all2[,"lipper_asset_cd"],incomparables=FALSE)
#lipper_tax_cd_u <- unique(monthly_data_all2[,"lipper_tax_cd"],incomparables=FALSE)

monthly_data_all2[,"lipper_asset_cd_eq_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="EQ", 1, monthly_data_all2$lipper_asset_cd_eq_dv)
monthly_data_all2[,"lipper_asset_cd_eq_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="TX" | 
                                                         toupper(monthly_data_all2$lipper_asset_cd)=="MB"), 0, monthly_data_all2$lipper_asset_cd_eq_dv)

monthly_data_all2[,"lipper_asset_cd_tx_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="TX", 1, monthly_data_all2$lipper_asset_cd_tx_dv)
monthly_data_all2[,"lipper_asset_cd_tx_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="MB" | 
                                                         toupper(monthly_data_all2$lipper_asset_cd)=="EQ"), 0, monthly_data_all2$lipper_asset_cd_tx_dv)

monthly_data_all2[,"lipper_asset_cd_mb_dv"] <- ifelse(toupper(monthly_data_all2$lipper_asset_cd)=="MB", 1, monthly_data_all2$lipper_asset_cd_mb_dv)
monthly_data_all2[,"lipper_asset_cd_mb_dv"] <- ifelse((toupper(monthly_data_all2$lipper_asset_cd)=="EQ" | 
                                                         toupper(monthly_data_all2$lipper_asset_cd)=="TX"), 0, monthly_data_all2$lipper_asset_cd_mb_dv)

monthly_data_all2[,"lipper_tax_cd_t_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAXABLE", 1, monthly_data_all2$lipper_tax_cd_t_dv)
monthly_data_all2[,"lipper_tax_cd_t_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAX-EXEMPT", 0, monthly_data_all2$lipper_tax_cd_t_dv)

monthly_data_all2[,"lipper_tax_cd_te_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAX-EXEMPT", 1, monthly_data_all2$lipper_tax_cd_te_dv)
monthly_data_all2[,"lipper_tax_cd_te_dv"] <- ifelse(toupper(monthly_data_all2$lipper_tax_cd)=="TAXABLE", 0, monthly_data_all2$lipper_tax_cd_te_dv)


###############################################################################
cat("MERGE FUND_NAMES AND MDM_DATA_TRIM", "\n")
###############################################################################

fund_names_msd <- merge(fund_names, Mdmf_data_trim, 
                        by.x=c("ncusip"), by.y=c("CUSIP"), all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

fund_names_msd <- fund_names_msd[order(fund_names_msd[,"crsp_fundno"], 
                                       fund_names_msd[,"chgdt"]),] 


fund_names_msd2 <- merge(fund_names_msd, mflink1, by.x=c("crsp_fundno"), by.y=c("crsp_fundno"), 
                         all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

fund_names_msd2 <- fund_names_msd2[c("wficn","crsp_fundno","Broad_Cat_Group","Global_Cat","MS_Cat","MS_Inst_Cat",
                                     "MS_Rating_Overall", "US_Broad_Asset_Class","Equity_Style_Box_Long","MS_Anal_Rating",
                                     "Firm_Name","Branding_Name","Prospectus_Objective")]

fund_names_msd2 <- fund_names_msd2[order(fund_names_msd2[,"wficn"], fund_names_msd2[,"crsp_fundno"]),]
fund_names_msd2 <- unique(fund_names_msd2,comparables=FALSE)

fund_names_msd3 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Firm_Name,Branding_Name,Global_Cat,
                                                    MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))

#fund_names_msd3[fund_names_msd3[,"wficn"]==103259,"MS_Anal_Rating"] <- "Silver" 
#fund_names_msd3[fund_names_msd3[,"wficn"]==103380,"MS_Anal_Rating"] <- "Bronze" 
#fund_names_msd3[fund_names_msd3[,"wficn"]==400001,"MS_Anal_Rating"] <- "Bronze" 

fund_names_msd3 <- unique(fund_names_msd3,comparables=FALSE)

fund_names_msd3_row_count <- as.data.frame(data.table(fund_names_msd3)[, list(count=.N),by="wficn"],stringsAsFactors=FALSE)
fund_names_msd3_bad <- fund_names_msd3_row_count[fund_names_msd3_row_count[,"count"]>1,]

fund_names_msd4 <- fund_names_msd3[!(fund_names_msd3[,"wficn"] %in% fund_names_msd3_bad[,"wficn"]),]

#rm2(fund_names_msd,fund_names_msd2,fund_names_msd3,fund_names_msd3_row_count,fund_names_msd3_bad)

fund_names_msd5 <- subset(fund_names_msd2,select=-c(crsp_fundno,MS_Rating_Overall,Broad_Cat_Group,Equity_Style_Box_Long,Global_Cat,
                                                    MS_Cat,MS_Inst_Cat,US_Broad_Asset_Class,MS_Anal_Rating,Prospectus_Objective))
fund_names_msd5 <- subset(fund_names_msd5,select=-c(Firm_Name))
fund_names_msd5 <- unique(fund_names_msd5,comparables=FALSE)

fund_names_msd5_row_count <- as.data.frame(data.table(fund_names_msd5)[, list(count=.N),by="wficn"],stringsAsFactors=FALSE)
fund_names_msd5_bad <- fund_names_msd5_row_count[fund_names_msd5_row_count[,"count"]>1,]

fund_names_msd6 <- fund_names_msd5[!(fund_names_msd5[,"wficn"] %in% fund_names_msd5_bad[,"wficn"]),]

#rm2(fund_names_msd5,fund_names_msd5_row_count,fund_names_msd5_bad)


#test <- as.data.frame(unique(fund_names_msd6[,"Branding_Name"],comparables=FALSE),stringsAsFactors=FALSE)
#colnames(test) <- "Branding_Name"
#test <- test[order(test[,"Branding_Name"]),]


###############################################################################
cat("ADD DVS FOR STYLE AND RATING", "\n")
###############################################################################

#aa_Broad_Cat_Group_u <- as.data.frame(unique(fund_names_msd4[,"Broad_Cat_Group"],comparables=FALSE),stringsAsFactors=FALSE)
#aa_Equity_Style_Box_Long_u <- as.data.frame(unique(fund_names_msd4[,"Equity_Style_Box_Long"],comparables=FALSE),stringsAsFactors=FALSE)
#aa_MS_Anal_Rating_u <- as.data.frame(unique(fund_names_msd4[,"MS_Anal_Rating"],comparables=FALSE),stringsAsFactors=FALSE)
#aa_Prospectus_Objective_u <- as.data.frame(unique(fund_names_msd4[,"Prospectus_Objective"],comparables=FALSE),stringsAsFactors=FALSE)

fund_names_msd_dv <- data.frame(fund_names_msd4,
                                Broad_Cat_Group_eq_dv=NA,Broad_Cat_Group_al_dv=NA,Broad_Cat_Group_fi_dv=NA,Broad_Cat_Group_alt_dv=NA,Broad_Cat_Group_tp_dv=NA,
                                Equity_Style_Box_Long_lg_dv=NA,Equity_Style_Box_Long_lv_dv=NA,Equity_Style_Box_Long_lb_dv=NA,
                                Equity_Style_Box_Long_mg_dv=NA,Equity_Style_Box_Long_mv_dv=NA,Equity_Style_Box_Long_mb_dv=NA,
                                Equity_Style_Box_Long_sg_dv=NA,Equity_Style_Box_Long_sv_dv=NA,Equity_Style_Box_Long_sb_dv=NA)
                                #MS_Anal_Rating_gold_dv=NA,MS_Anal_Rating_silver_dv=NA,MS_Anal_Rating_bronze_dv=NA,
                                #MS_Anal_Rating_neutral_dv=NA,MS_Anal_Rating_negative_dv=NA)

fund_names_msd_dv[,"Broad_Cat_Group_eq_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="EQUITY", 1, 0)
fund_names_msd_dv[,"Broad_Cat_Group_eq_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_eq_dv)

fund_names_msd_dv[,"Broad_Cat_Group_al_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="ALLOCATION", 1, 0)
fund_names_msd_dv[,"Broad_Cat_Group_al_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_al_dv)

fund_names_msd_dv[,"Broad_Cat_Group_fi_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="FIXED INCOME", 1, 0)
fund_names_msd_dv[,"Broad_Cat_Group_fi_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_fi_dv)

fund_names_msd_dv[,"Broad_Cat_Group_alt_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="ALTERNATIVE", 1, 0)
fund_names_msd_dv[,"Broad_Cat_Group_alt_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_alt_dv)

fund_names_msd_dv[,"Broad_Cat_Group_tp_dv"] <- ifelse(toupper(fund_names_msd4$Broad_Cat_Group)=="TAX PREFERRED", 1, 0)
fund_names_msd_dv[,"Broad_Cat_Group_tp_dv"] <- ifelse(is.na(fund_names_msd4$Broad_Cat_Group), NA, fund_names_msd_dv$Broad_Cat_Group_tp_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_lg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE GROWTH", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_lg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lg_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_lv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE VALUE", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_lv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lv_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_lb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="LARGE BLEND", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_lb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_lb_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_mg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID GROWTH", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_mg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mg_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_mv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID VALUE", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_mv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mv_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_mb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="MID BLEND", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_mb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_mb_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_sg_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL GROWTH", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_sg_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sg_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_sv_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL VALUE", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_sv_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sv_dv)

fund_names_msd_dv[,"Equity_Style_Box_Long_sb_dv"] <- ifelse(toupper(fund_names_msd4$Equity_Style_Box_Long)=="SMALL BLEND", 1, 0)
fund_names_msd_dv[,"Equity_Style_Box_Long_sb_dv"] <- ifelse(is.na(fund_names_msd4$Equity_Style_Box_Long), NA, fund_names_msd_dv$Equity_Style_Box_Long_sb_dv)

####CANNOT USE THESE BECAUSE THERE AREN"T ACROSS TIME
# fund_names_msd_dv[,"MS_Anal_Rating_gold_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="GOLD", 1, 0)
# fund_names_msd_dv[,"MS_Anal_Rating_gold_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_gold_dv)
# 
# fund_names_msd_dv[,"MS_Anal_Rating_silver_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="SILVER", 1, 0)
# fund_names_msd_dv[,"MS_Anal_Rating_silver_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_silver_dv)
# 
# fund_names_msd_dv[,"MS_Anal_Rating_bronze_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="BRONZE", 1, 0)
# fund_names_msd_dv[,"MS_Anal_Rating_bronze_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_bronze_dv)
# 
# fund_names_msd_dv[,"MS_Anal_Rating_neutral_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="NEUTRAL", 1, 0)
# fund_names_msd_dv[,"MS_Anal_Rating_neutral_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_neutral_dv)
# 
# fund_names_msd_dv[,"MS_Anal_Rating_negative_dv"] <- ifelse(toupper(fund_names_msd4$MS_Anal_Rating)=="NEGATIVE", 1, 0)
# fund_names_msd_dv[,"MS_Anal_Rating_negative_dv"] <- ifelse(is.na(fund_names_msd4$MS_Anal_Rating), NA, fund_names_msd_dv$MS_Anal_Rating_negative_dv)
# 

monthly_data_all3 <- merge(monthly_data_all2, fund_names_msd_dv, 
                        by.x=c("wficn"), by.y=c("wficn"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

monthly_data_all4 <- data.frame(monthly_data_all3,FundRetMktNeg=NA,stringsAsFactors=FALSE)

monthly_data_all4[,"FundRetMktNeg"] <- ifelse(monthly_data_all4$mktadjret_agg<0, monthly_data_all4$mktadjret_agg, 0)
monthly_data_all4[,"FundRetMktNeg"] <- ifelse(is.na(monthly_data_all4$mktadjret_agg), NA, monthly_data_all4$FundRetMktNeg)

monthly_data_all4 <- monthly_data_all4[order(monthly_data_all4[,"wficn"],
                                             monthly_data_all4[,"yr"],
                                             monthly_data_all4[,"month"]),]

#rm2(fund_names_msd_dv)


###############################################################################
cat("COMPUTE AVERAGE GRADE-LEVEL", "\n")
###############################################################################

read_stats_iois_f  <- read_stats_iois_f[((!is.na(read_stats_iois_f[,"ARI_iois"])) & 
                                           (!is.na(read_stats_iois_f[,"Coleman_Liau_iois"])) & 
                                           (!is.na(read_stats_iois_f[,"Flesch_Kincaid_iois"])) & 
                                           (!is.na(read_stats_iois_f[,"FOG_iois"])) &   
                                           (!is.na(read_stats_iois_f[,"SMOG_iois"]))),]

avg_grade_level_iois <- as.data.frame(rowMeans(read_stats_iois_f[,c("ARI_iois","Coleman_Liau_iois","Flesch_Kincaid_iois","FOG_iois","SMOG_iois")], na.rm=TRUE),stringsAsFactors=FALSE)
colnames(avg_grade_level_iois) <- "avg_grade_level_iois"

read_stats_iois_f <- cbind(read_stats_iois_f,avg_grade_level_iois)


read_stats_pr_f   <- read_stats_pr_f[((!is.na(read_stats_pr_f[,"ARI_pr"])) & 
                                        (!is.na(read_stats_pr_f[,"Coleman_Liau_pr"])) & 
                                        (!is.na(read_stats_pr_f[,"Flesch_Kincaid_pr"])) & 
                                        (!is.na(read_stats_pr_f[,"FOG_pr"])) & 
                                        (!is.na(read_stats_pr_f[,"SMOG_pr"]))),]

avg_grade_level_pr <- as.data.frame(rowMeans(read_stats_pr_f[,c("ARI_pr","Coleman_Liau_pr","Flesch_Kincaid_pr","FOG_pr","SMOG_pr")], na.rm=TRUE),stringsAsFactors=FALSE)
colnames(avg_grade_level_pr) <- "avg_grade_level_pr"

read_stats_pr_f <- cbind(read_stats_pr_f,avg_grade_level_pr)

rm2(avg_grade_level_iois,avg_grade_level_pr)


###############################################################################
cat("COMPUTE AVERAGE SIMILARITY BY BROAD_CAT_GROUP AND YEAR", "\n")
###############################################################################

#similarity_db_tables <- ListTables(similarity_db)
#similarity_db_fields <- ListFields(similarity_db)

group_column <- "Broad_Cat_Group"

sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]

temp_stacked_full <- merge(fund_names_msd4[,c("wficn",group_column)], sample_data_all_temp[,c("yr","wficn")], 
                           by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
text_group_vars <- toupper(text_group_vars)
text_group_vars <- trim(text_group_vars)
text_group_vars <- text_group_vars[!is.na(text_group_vars)]
text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]

#Create empty data.frame for merging
for (j in 1:length(text_variables))
{
  #j <- 1
  
  text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
  text_percentages_temp_col <- empty.df(text_percentages_temp)
  for (k in 1:ncol(text_percentages_temp_col))
  {
    text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
  }
  header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
  assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
  
  rm2(text_percentages_temp,text_percentages_temp_col,header_col)
}


for (i in 1:length(text_group_vars))
{
  #i <- 1

  for (j in 1:length(text_variables))
  {
    #j <- 1
    
    temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j])
    
    if (text_variables[j]=="iois")
    {
      year_sim_iois_broad_cat_group_stacked <- rbind(year_sim_iois_broad_cat_group_stacked,temp_sim_stacked)
      
    } else if (text_variables[j]=="pr")
    {
      year_sim_pr_broad_cat_group_stacked <- rbind(year_sim_pr_broad_cat_group_stacked,temp_sim_stacked)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    rm2(temp_sim_stacked)
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
    

  }
  
}

colnames(year_sim_iois_broad_cat_group_stacked) <- paste(group_column,colnames(year_sim_iois_broad_cat_group_stacked),sep="_")
colnames(year_sim_iois_broad_cat_group_stacked)[1:3] <- c("wficn","yr",group_column)

year_sim_iois_broad_cat_group_stacked <- year_sim_iois_broad_cat_group_stacked[order(year_sim_iois_broad_cat_group_stacked[,"wficn"], 
                                                                                     year_sim_iois_broad_cat_group_stacked[,"yr"]),]

colnames(year_sim_pr_broad_cat_group_stacked) <- paste(tolower(group_column),colnames(year_sim_pr_broad_cat_group_stacked),sep="_")
colnames(year_sim_pr_broad_cat_group_stacked)[1:3] <- c("wficn","yr",group_column)

year_sim_pr_broad_cat_group_stacked <- year_sim_pr_broad_cat_group_stacked[order(year_sim_pr_broad_cat_group_stacked[,"wficn"], 
                                                                                 year_sim_pr_broad_cat_group_stacked[,"yr"]),]

rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)


###############################################################################
cat("COMPUTE AVERAGE SIMILARITY BY EQUITY STYLE LONG BOX AND YEAR", "\n")
###############################################################################

group_column <- "Equity_Style_Box_Long"

sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]

temp_stacked_full <- merge(fund_names_msd4[,c("wficn",group_column)], sample_data_all_temp[,c("yr","wficn")], 
                           by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
text_group_vars <- toupper(text_group_vars)
text_group_vars <- trim(text_group_vars)
text_group_vars <- text_group_vars[!is.na(text_group_vars)]

#Create empty data.frame for merging
for (j in 1:length(text_variables))
{
  #j <- 1
  
  text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
  text_percentages_temp_col <- empty.df(text_percentages_temp)
  for (k in 1:ncol(text_percentages_temp_col))
  {
    text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
  }
  header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
  assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
  
  rm2(text_percentages_temp,text_percentages_temp_col,header_col)
}

for (i in 1:length(text_group_vars))
{
  #i <- 1
  
  for (j in 1:length(text_variables))
  {
    #j <- 1
    
    temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j])
    
    if (text_variables[j]=="iois")
    {
      year_sim_iois_equity_style_box_long_stacked <- rbind(year_sim_iois_equity_style_box_long_stacked,temp_sim_stacked)
      
    } else if (text_variables[j]=="pr")
    {
      year_sim_pr_equity_style_box_long_stacked <- rbind(year_sim_pr_equity_style_box_long_stacked,temp_sim_stacked)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    rm2(temp_sim_stacked)
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
    
    
  }
  
}

colnames(year_sim_iois_equity_style_box_long_stacked) <- paste(group_column,colnames(year_sim_iois_equity_style_box_long_stacked),sep="_")
colnames(year_sim_iois_equity_style_box_long_stacked)[1:3] <- c("wficn","yr",group_column)

year_sim_iois_equity_style_box_long_stacked <- year_sim_iois_equity_style_box_long_stacked[order(year_sim_iois_equity_style_box_long_stacked[,"wficn"], 
                                                                                                 year_sim_iois_equity_style_box_long_stacked[,"yr"]),]

colnames(year_sim_pr_equity_style_box_long_stacked) <- paste(tolower(group_column),colnames(year_sim_pr_equity_style_box_long_stacked),sep="_")
colnames(year_sim_pr_equity_style_box_long_stacked)[1:3] <- c("wficn","yr",group_column)

year_sim_pr_equity_style_box_long_stacked <- year_sim_pr_equity_style_box_long_stacked[order(year_sim_pr_equity_style_box_long_stacked[,"wficn"], 
                                                                                             year_sim_pr_equity_style_box_long_stacked[,"yr"]),]

rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)


###############################################################################
cat("COMPUTE AVERAGE SIMILARITY BY FAMILY AND YEAR", "\n")
###############################################################################

group_column <- "Branding_Name"

sample_data_all_temp <- sample_data_all[,c("wficn","yr")]
sample_data_all_temp <- unique(sample_data_all_temp,comparables=FALSE)
sample_data_all_temp <- sample_data_all_temp[order(sample_data_all_temp[,"wficn"], sample_data_all_temp[,"yr"]),]

temp_stacked_full <- merge(fund_names_msd6[,c("wficn",group_column)], sample_data_all_temp[,c("yr","wficn")], 
                           by.x=c("wficn"), by.y=c("wficn"), all.x=FALSE, all.y=TRUE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

text_group_vars <- unique(temp_stacked_full[,group_column],comparables=FALSE)
text_group_vars <- toupper(text_group_vars)
text_group_vars <- trim(text_group_vars)
text_group_vars <- text_group_vars[!is.na(text_group_vars)]
#text_group_vars <- text_group_vars[!(text_group_vars %in% c("ALTERNATIVE", "TAX PREFERRED"))]

#Create empty data.frame for merging
for (j in 1:length(text_variables))
{
  #j <- 1
  
  text_percentages_temp <- paste("similarity",text_percentages,text_variables[j],sep="_")
  text_percentages_temp_col <- empty.df(text_percentages_temp)
  for (k in 1:ncol(text_percentages_temp_col))
  {
    text_percentages_temp_col[,k] <- as.numeric(text_percentages_temp_col[,k])
  }
  header_col <- data.frame(wficn=integer(),yr=integer(),group=character(),stringsAsFactors=FALSE) 
  assign(paste("year_sim",text_variables[j],tolower(group_column),"stacked",sep="_"), cbind(header_col,text_percentages_temp_col), envir=.GlobalEnv)
  
  rm2(text_percentages_temp,text_percentages_temp_col,header_col)
}


for (i in 1:length(text_group_vars))
{
  #i <- 1
  
  for (j in 1:length(text_variables))
  {
    #j <- 1
    
    temp_sim_stacked <- calculate_similarity_by_group(temp_stacked_full,group_column,text_group_vars[i],text_variables[j])
    
    if (text_variables[j]=="iois")
    {
      year_sim_iois_branding_name_stacked <- rbind(year_sim_iois_branding_name_stacked,temp_sim_stacked)
      
    } else if (text_variables[j]=="pr")
    {
      year_sim_pr_branding_name_stacked <- rbind(year_sim_pr_branding_name_stacked,temp_sim_stacked)
      
    } else
    {
      cat("ERROR!", "\n")
    }
    
    rm2(temp_sim_stacked)
    
    progress_function(outer_loop_count=i, outer_loop_start_val=1, outer_loop_end_val=length(text_group_vars), 
                      inner_loop_count=j, inner_loop_start_val=1, inner_loop_end_val=length(text_variables))
    
    
  }
  
}

colnames(year_sim_iois_branding_name_stacked) <- paste(group_column,colnames(year_sim_iois_branding_name_stacked),sep="_")
colnames(year_sim_iois_branding_name_stacked)[1:3] <- c("wficn","yr",group_column)

year_sim_iois_branding_name_stacked <- year_sim_iois_branding_name_stacked[order(year_sim_iois_branding_name_stacked[,"wficn"], 
                                                                                 year_sim_iois_branding_name_stacked[,"yr"]),]

colnames(year_sim_pr_branding_name_stacked) <- paste(tolower(group_column),colnames(year_sim_pr_branding_name_stacked),sep="_")
colnames(year_sim_pr_branding_name_stacked)[1:3] <- c("wficn","yr",group_column)

year_sim_pr_branding_name_stacked <- year_sim_pr_branding_name_stacked[order(year_sim_pr_branding_name_stacked[,"wficn"], 
                                                                             year_sim_pr_branding_name_stacked[,"yr"]),]

rm2(group_column,sample_data_all_temp,temp_stacked_full,text_group_vars)


###############################################################################
cat("BACKUP SIMIALRITY DATA", "\n")
###############################################################################

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

ExportTable(descriptive_stats_db,"year_sim_iois_all_stacked",year_sim_iois_all_stacked)
ExportTable(descriptive_stats_db,"year_sim_pr_all_stacked",year_sim_pr_all_stacked)
ExportTable(descriptive_stats_db,"year_sim_iois_broad_cat_group_stacked",year_sim_iois_broad_cat_group_stacked)
ExportTable(descriptive_stats_db,"year_sim_pr_broad_cat_group_stacked",year_sim_pr_broad_cat_group_stacked)
ExportTable(descriptive_stats_db,"year_sim_iois_equity_style_box_long_stacked",year_sim_iois_equity_style_box_long_stacked)
ExportTable(descriptive_stats_db,"year_sim_pr_equity_style_box_long_stacked",year_sim_pr_equity_style_box_long_stacked)
ExportTable(descriptive_stats_db,"year_sim_iois_branding_name_stacked",year_sim_iois_branding_name_stacked)
ExportTable(descriptive_stats_db,"year_sim_pr_branding_name_stacked",year_sim_pr_branding_name_stacked)

#year_sim_iois_all_stacked                   <- runsql("SELECT * FROM year_sim_iois_all_stacked",descriptive_stats_db)
#year_sim_pr_all_stacked                     <- runsql("SELECT * FROM year_sim_pr_all_stacked",descriptive_stats_db)
#year_sim_iois_broad_cat_group_stacked       <- runsql("SELECT * FROM year_sim_iois_broad_cat_group_stacked",descriptive_stats_db)
#year_sim_pr_broad_cat_group_stacked         <- runsql("SELECT * FROM year_sim_pr_broad_cat_group_stacked",descriptive_stats_db)
#year_sim_iois_equity_style_box_long_stacked <- runsql("SELECT * FROM year_sim_iois_equity_style_box_long_stacked",descriptive_stats_db)
#year_sim_pr_equity_style_box_long_stacked   <- runsql("SELECT * FROM year_sim_pr_equity_style_box_long_stacked",descriptive_stats_db)
#year_sim_iois_branding_name_stacked         <- runsql("SELECT * FROM year_sim_iois_branding_name_stacked",descriptive_stats_db)
#year_sim_pr_branding_name_stacked           <- runsql("SELECT * FROM year_sim_pr_branding_name_stacked",descriptive_stats_db)


###############################################################################
cat("MERGE IOIS TEXT DATA", "\n")
###############################################################################

read_stats_iois <- merge(monthly_data_all4[,c("wficn","yr","month")], read_stats_iois_f, 
                         by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
read_stats_iois <- read_stats_iois[order(read_stats_iois[,"wficn"],read_stats_iois[,"yr"],read_stats_iois[,"month"]),]

sim_stats_iois <- merge(monthly_data_all4[,c("wficn","yr","month")], year_sim_iois_all_stacked,
                        by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_iois <- merge(sim_stats_iois, year_sim_iois_broad_cat_group_stacked, 
                        by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_iois <- merge(sim_stats_iois, year_sim_iois_equity_style_box_long_stacked, 
                        by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_iois <- merge(sim_stats_iois, year_sim_iois_branding_name_stacked, 
                        by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_iois <- sim_stats_iois[order(sim_stats_iois[,"wficn"],sim_stats_iois[,"yr"],sim_stats_iois[,"month"]),]

text_stats_iois <- merge(read_stats_iois, sim_stats_iois, by.x=c("wficn","yr","month"), 
                         by.y=c("wficn","yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

#rm2(read_stats_iois_f)
#rm2(year_sim_iois_all_stacked,year_sim_iois_broad_cat_group_stacked)
#rm2(year_sim_iois_equity_style_box_long_stacked,year_sim_iois_branding_name_stacked)
rm2(read_stats_iois,sim_stats_iois)


###############################################################################
cat("MERGE PR TEXT DATA", "\n")
###############################################################################

read_stats_pr <- merge(monthly_data_all4[,c("wficn","yr","month")], read_stats_pr_f, 
                       by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
read_stats_pr <- read_stats_pr[order(read_stats_pr[,"wficn"],read_stats_pr[,"yr"],read_stats_pr[,"month"]),]

sim_stats_pr <- merge(monthly_data_all4[,c("wficn","yr","month")], year_sim_pr_all_stacked,
                      by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_pr <- merge(sim_stats_pr, year_sim_pr_broad_cat_group_stacked, 
                      by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_pr <- merge(sim_stats_pr, year_sim_pr_equity_style_box_long_stacked, 
                      by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_pr <- merge(sim_stats_pr, year_sim_pr_branding_name_stacked, 
                      by.x=c("wficn","yr"), by.y=c("wficn","yr"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
sim_stats_pr <- sim_stats_pr[order(sim_stats_pr[,"wficn"],sim_stats_pr[,"yr"],sim_stats_pr[,"month"]),]

text_stats_pr <- merge(read_stats_pr, sim_stats_pr, by.x=c("wficn","yr","month"), 
                       by.y=c("wficn","yr","month"), all.x=TRUE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

#rm2(read_stats_pr_f)
#rm2(year_sim_pr_all_stacked,year_sim_pr_broad_cat_group_stacked)
#rm2(year_sim_pr_equity_style_box_long_stacked,year_sim_pr_branding_name_stacked)
rm2(read_stats_pr,sim_stats_pr)


###############################################################################
cat("BACKUP DATA", "\n")
###############################################################################

descriptive_stats_tables <- ListTables(descriptive_stats_db)
descriptive_stats_fields <- ListFields(descriptive_stats_db)

ExportTable(descriptive_stats_db,"monthly_data_all4",monthly_data_all4)
ExportTable(descriptive_stats_db,"text_stats_iois",text_stats_iois)
ExportTable(descriptive_stats_db,"text_stats_pr",text_stats_pr)

