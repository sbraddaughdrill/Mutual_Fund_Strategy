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

rm(list=ls(all=TRUE))                                #Clear workspace

Sys.setenv(R_HISTSIZE=99999)                         #Limit History so that it never contains more than 50 lines

repo <- c("http://cran.us.r-project.org")
options(repos=structure(repo))
options(install.packages.check.source = FALSE)
options(StringsAsFactors=FALSE)                      #String as factors is False -- used for read.csv
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
  input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)      #Create data input directory
  output_directory <- normalizePath("C:/Research_temp/",winslash = "\\", mustWork = NA)                                       #Create data output directory
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/",winslash = "\\", mustWork = NA)                     #Create function directory
  #package_directory <- normalizePath("C:/Users/Brad/Documents/R/win-library/2.15/",winslash = "\\", mustWork = NA)            #Create package directory
  treetag_directory <- normalizePath("C:/TreeTagger",winslash = "\\", mustWork = NA)                                          #Create treetag directory
  
} else if (Location==2)
{
  #WORK
  
  setwd("C:/Research_temp/")                                                                                                  #Set working directory
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)  #Create data input directory
  output_directory <- normalizePath("C:/Research_temp/",winslash = "\\", mustWork = NA)                                       #Create data output directory
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/R/",winslash = "\\", mustWork = NA)                 #Create function directory
  #package_directory <- normalizePath("C:/Users/bdaughdr/Documents/R/win-library/2.15/",winslash = "\\", mustWork = NA)        #Create package directory
  treetag_directory <- normalizePath("C:/TreeTagger",winslash = "\\", mustWork = NA)                                          #Create treetag directory
  
} else
{
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)

#==============================================================================;
#FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
#==============================================================================;

#External Functions
source(file=paste(function_directory,"functions.R",sep=""),echo=FALSE)

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

data_expand_function <- function(id_col,multiplier_col){
  
  return(rep(id_col,multiplier_col))
}

# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=30) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

load_external_packages <- function(x,repo_str)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE,repos=repo_str)
    #install.packages(x,dep=TRUE)
    
    if(!require(x,character.only = TRUE)) 
    {
      cat(paste("ERROR LOADING PACKAGE ",x,sep=""), "\n")
      
      return(0)
      
    } else
    {
      return(1)
      
    }
    
  } else
  {
    return(1)
    
  }
  
}

list_installed_packages <- function(external_package_char_vec)
{
  #Get list of installed packages
  installed_packages_temp0 <- as.data.frame(installed.packages(),stringsAsFactors=FALSE)
  installed_packages_temp0 <- unique(installed_packages_temp0)
  Packages_titles <- unlist(sapply(installed_packages_temp0[,1],function(x) return(packageDescription(x)$Title),  simplify = FALSE, USE.NAMES = FALSE))
  installed_packages_temp <- cbind(installed_packages_temp0,Packages_titles,rep(NA,nrow(installed_packages_temp0)),rep(NA,nrow(installed_packages_temp0)))
  colnames(installed_packages_temp) <- append(names(installed_packages_temp0),c("Packages_titles","User_Listed_External","Loaded"))                                                               
  installed_packages_temp <- subset(installed_packages_temp,select=c("Package","Packages_titles","Version","User_Listed_External","Loaded"))
  
  
  #Get list of loaded packages
  loaded_packages_temp <- search()
  loaded_packages_temp <- loaded_packages_temp[!(loaded_packages_temp==".GlobalEnv") & !(loaded_packages_temp=="Autoloads")]
  loaded_packages_temp <- gsub(pattern="package:", replacement="", x=loaded_packages_temp)
  loaded_packages_temp <- unique(loaded_packages_temp)
  loaded_packages_temp <- sort(loaded_packages_temp, decreasing = FALSE)
  
  #Populate user listed external packages in vector external_packages
  installed_packages_temp[!(installed_packages_temp$Package %in% external_package_char_vec), "User_Listed_External"] <- 0
  installed_packages_temp[ (installed_packages_temp$Package %in% external_package_char_vec), "User_Listed_External"] <- 1
  
  #Populate loaded packages
  installed_packages_temp[!(installed_packages_temp$Package %in% loaded_packages_temp), "Loaded"] <- 0
  installed_packages_temp[ (installed_packages_temp$Package %in% loaded_packages_temp), "Loaded"] <- 1
  
  installed_packages_temp <- unique(installed_packages_temp)
  installed_packages_temp <- installed_packages_temp[order(-installed_packages_temp$User_Listed_External,installed_packages_temp$Package),]
  
  return(installed_packages_temp)
}

unfactorize <- function(df){
  for(i in which(sapply(df, class) == "factor"))
  {
    df[[i]] = as.character(df[[i]])
  }
  return(df)
}


compute_readability_stats <- function(sample_cell_temp,tagged_text_desc_measures,hyph_text_en_desc_measures,readability_measures,readability_desc_measures,token_measures){
  
  #TEST!!
  #sample_cell_temp <- sample_data_all[1,3]
  #tagged_text_desc_measures <- tagged_text_desc_stats
  #hyph_text_en_desc_measures <- hyph_text_en_desc_stats
  #readability_measures <- readability_stats
  #readability_desc_measures <- readability_desc_stats 
  #token_measures <- token_stats
  
  #rm(sample_cell_temp,tagged_text_desc_measures,hyph_text_en_desc_measures,readability_measures,readability_desc_measures)
  
  if(!(is.na(sample_cell_temp)))
  {
    
    temp_text <- unlist(strsplit(sample_cell_temp, "\n"))
    
    tagged_text <- treetag(temp_text, treetagger="manual",lang="en", TT.options=c(path="C:/TreeTagger", preset="en"),debug=FALSE,format="obj")
    tagged_text_desc_measures_temp <- unlist(sapply(tagged_text_desc_measures,function(x) return(as.numeric(unlist(tagged_text@desc)[x])),  simplify = FALSE, USE.NAMES = FALSE))
    
    hyph_text_en <- hyphen(tagged_text,quiet=TRUE,cache=TRUE)
    hyph_text_en_desc_measures_temp <- unlist(sapply(hyph_text_en_desc_measures,function(x) return(as.numeric(unlist(hyph_text_en@desc)[x])),  simplify = FALSE, USE.NAMES = FALSE))
    
    readbl_text <- suppressWarnings(readability(tagged_text, hyphen=hyph_text_en, index=readability_measures,quiet=TRUE))
    readability_measures_temp <- unlist(sapply(readability_measures,function(x) return(as.numeric(unlist(slot(readbl_text, x))["grade"])),  simplify = FALSE, USE.NAMES = FALSE))
    readability_desc_measures_temp <- unlist(sapply(readability_desc_measures,function(x) return(as.numeric(unlist(readbl_text@desc)[x])),  simplify = FALSE, USE.NAMES = FALSE))
    
    readstats_df <- as.data.frame(do.call(cbind, as.list(c(tagged_text_desc_measures_temp,hyph_text_en_desc_measures_temp,readability_measures_temp,readability_desc_measures_temp))),stringsAsFactors=FALSE)
    colnames(readstats_df) <- paste("V",1:ncol(readstats_df),sep="")
    
    tokens <- sapply(token_measures,function(x) return(readbl_text@TT.res[x]), simplify = FALSE, USE.NAMES = FALSE)
    tokens_df <- as.data.frame(tokens,stringsAsFactors=FALSE)
    colnames(tokens_df) <- paste("V",1:ncol(tokens_df),sep="")
    
    return(list(readstats = readstats_df,tokens = tokens_df))
    
  } else
  {
    readstats_df <- as.data.frame(do.call(cbind, as.list(rep(NA, length(c(tagged_text_desc_measures,hyph_text_en_desc_measures,readability_measures,readability_desc_measures))))),stringsAsFactors=FALSE)
    colnames(readstats_df) <- paste("V",1:ncol(readstats_df),sep="")
    
    tokens_df <- as.data.frame(do.call(cbind, as.list(rep(NA, length(token_measures)))),stringsAsFactors=FALSE)
    colnames(tokens_df) <- paste("V",1:ncol(tokens_df),sep="")
    
    return(list(readstats = readstats_df,tokens = tokens_df))
    
  }
  
}

#==============================================================================;
#LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
#==============================================================================;

#Load External Packages
external_packages <- c("arules","bigalgebra","biganalytics","bigmemory","bigtabulate","clv","compare","cwhmisc","data.table","fastmatch","ff","foreach","foreign","formatR",
                       "gdata","gtools","Hmisc","koRpus","mitools","pbapply","plyr","R.oo","snow","snowfall","sqldf","stringr","synchronicity","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify = FALSE, USE.NAMES = FALSE)))
installed_packages <- list_installed_packages(external_packages)

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
#files[,1] <- c("1999.csv","2000.csv","2001.csv","2002.csv","2003.csv","2004.csv","2005.csv","2006.csv","2007.csv","2008.csv")
file_list <- c("Target_good_final.csv")
files_cols_count <- 2
files_cols <- temp_data_cols[1:files_cols_count,]
files_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filename",stringsAsFactors = FALSE)
files_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="filepath",stringsAsFactors = FALSE)
files <- as.data.frame(matrix(NA, ncol = files_cols_count, nrow = length(file_list)))
colnames(files) <- files_cols[,6]
files <- format_function(files,files_cols)

#Percentiles table
percentiles_cols_count <- 8
percentiles_cols <- temp_data_cols[1:percentiles_cols_count,]
percentiles_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Level",stringsAsFactors = FALSE)
percentiles_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Confidence_Pct",stringsAsFactors = FALSE)
percentiles_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Confidence_lbl",stringsAsFactors = FALSE)
percentiles_cols[4,] <- data.frame(order=4,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Level",stringsAsFactors = FALSE)
percentiles_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Significance_Pct",stringsAsFactors = FALSE)
percentiles_cols[6,] <- data.frame(order=6,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Significance_lbl",stringsAsFactors = FALSE)
percentiles_cols[7,] <- data.frame(order=7,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_lbl",stringsAsFactors = FALSE)
percentiles_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Column_DV",stringsAsFactors = FALSE)
percentiles <- as.data.frame(matrix(NA, ncol = percentiles_cols_count, nrow = 3))
colnames(percentiles) <- percentiles_cols[,6]

#Readability columns table
readbl_vars_cols_count <- 4
readbl_vars_cols <- temp_data_cols[1:readbl_vars_cols_count,]
readbl_vars_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="column",stringsAsFactors = FALSE)
readbl_vars_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="suffix",stringsAsFactors = FALSE)
readbl_vars_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="readabilitystats_table",stringsAsFactors = FALSE)
readbl_vars_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token_table",stringsAsFactors = FALSE)
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
readbl_all_df <- format_function(readbl_all_df,readbl_all_df_cols)

#Sample data input columns
sample_data_input_cols_count <- 7
sample_data_input_cols <- temp_data_cols[1:sample_data_input_cols_count,]
sample_data_input_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[2,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="crsp_fundno",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[3,] <- data.frame(order=38,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[4,] <- data.frame(order=39,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[5,] <- data.frame(order=40,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_strategy_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[6,] <- data.frame(order=41,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks_f",desc="input",stringsAsFactors = FALSE)
sample_data_input_cols[7,] <- data.frame(order=44,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="File",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols_count <- 44
# sample_data_input_cols <- temp_data_cols[1:sample_data_input_cols_count,]
# sample_data_input_cols[1,] <- data.frame(order=1,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[2,] <- data.frame(order=2,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="cusip8",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="crsp_fundno",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="chgdt",desc="input",stringsAsFactors = FALSE)             #DATE
# sample_data_input_cols[5,] <- data.frame(order=5,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="chgenddt",desc="input",stringsAsFactors = FALSE)          #DATE
# sample_data_input_cols[6,] <- data.frame(order=6,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="crsp_portno",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[7,] <- data.frame(order=7,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="crsp_cl_grp",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[8,] <- data.frame(order=8,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="fund_name",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[9,] <- data.frame(order=9,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="nasdaq",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[10,] <- data.frame(order=10,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ncusip",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[11,] <- data.frame(order=11,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="first_offer_dt",desc="input",stringsAsFactors = FALSE)  #DATE
# sample_data_input_cols[12,] <- data.frame(order=12,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgmt_name",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[13,] <- data.frame(order=13,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgmt_cd",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[14,] <- data.frame(order=14,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgr_name",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[15,] <- data.frame(order=15,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="mgr_dt",desc="input",stringsAsFactors = FALSE)          #DATE
# sample_data_input_cols[16,] <- data.frame(order=16,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="adv_name",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[17,] <- data.frame(order=17,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="open_to_inv",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[18,] <- data.frame(order=18,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="retail_fund",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[19,] <- data.frame(order=19,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="inst_fund",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[20,] <- data.frame(order=20,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="m_fund",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[21,] <- data.frame(order=21,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="index_fund_flag",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[22,] <- data.frame(order=22,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="vau_fund",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[23,] <- data.frame(order=23,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="et_flag",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[24,] <- data.frame(order=24,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="fyear",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[25,] <- data.frame(order=25,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="accession_num",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[26,] <- data.frame(order=26,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="GVKEY",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[27,] <- data.frame(order=27,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CIK",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[28,] <- data.frame(order=28,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="FDATE",desc="input",stringsAsFactors = FALSE)           #DATE
# sample_data_input_cols[29,] <- data.frame(order=29,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="FINDEXDATE",desc="input",stringsAsFactors = FALSE)      #DATE
# sample_data_input_cols[30,] <- data.frame(order=30,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="LINDEXDATE",desc="input",stringsAsFactors = FALSE)      #DATE
# sample_data_input_cols[31,] <- data.frame(order=31,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Form",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[32,] <- data.frame(order=32,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="CoName",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[33,] <- data.frame(order=33,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="Fname",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[34,] <- data.frame(order=34,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="PortName",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[35,] <- data.frame(order=35,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[36,] <- data.frame(order=36,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[37,] <- data.frame(order=37,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[38,] <- data.frame(order=38,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_f",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[39,] <- data.frame(order=39,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_strategy_f",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[40,] <- data.frame(order=40,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="investment_objective_strategy_f",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[41,] <- data.frame(order=41,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="principal_risks_f",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[42,] <- data.frame(order=42,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Process_IS",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[43,] <- data.frame(order=43,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Process_R",desc="input",stringsAsFactors = FALSE)
# sample_data_input_cols[44,] <- data.frame(order=44,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="File",desc="input",stringsAsFactors = FALSE)
sample_data_all <- as.data.frame(matrix(NA, ncol = sample_data_input_cols_count, nrow = 300000))
colnames(sample_data_all) <- sample_data_input_cols[,6]
sample_data_all <- format_function(sample_data_all,sample_data_input_cols)

#Tokens table
tokens_all_cols_count <- 5
tokens_all_cols <- temp_data_cols[1:tokens_all_cols_count,]
tokens_all_cols[1,] <- data.frame(order=1,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="ID",stringsAsFactors = FALSE)
tokens_all_cols[2,] <- data.frame(order=2,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="yr",stringsAsFactors = FALSE)
tokens_all_cols[3,] <- data.frame(order=3,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="token",stringsAsFactors = FALSE)
tokens_all_cols[4,] <- data.frame(order=4,isnum=0,ischar=1,isdate=0,isfactor=0,colnames="desc",stringsAsFactors = FALSE)
tokens_all_cols[5,] <- data.frame(order=5,isnum=1,ischar=0,isdate=0,isfactor=0,colnames="Remove",stringsAsFactors = FALSE)

#==============================================================================;
#POPULATE SELECTED DATA;
cat("SECTION: POPULATE DATA", "\n")
#==============================================================================;

#Populate Percentiles table
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
percentiles <- format_function(percentiles,percentiles_cols)
Word_Cutoff_vector <- grep(pattern = 'Word_Cutoff_*', percentiles[,names(percentiles)=="Column_lbl"], ignore.case = FALSE, perl = FALSE, value = TRUE)
Word_DV_vector <- grep(pattern = 'Word_DV_*', percentiles[,names(percentiles)=="Column_DV"], ignore.case = FALSE, perl = FALSE, value = TRUE)

#List variables to compute readability statistics and the suffixes used for each in the sample_data_statistics_cols table
readbl_vars[,1] <- c("investment_objective_strategy_f","principal_risks_f")
readbl_vars[,2] <- c("_iois","_pr")
readbl_vars[,3] <- c("read_stats_ios_f","read_stats_pr_f")
readbl_vars[,4] <- c("tokens_all_ios_f","tokens_all_pr_f")


#==============================================================================;
#IMPORT DATA;
cat("SECTION: IMPORT DATA", "\n")
#==============================================================================;

files[,1] <-  file_list
files[,2] <-  unlist(mapply(merge_cols_function,col_one=input_directory,col_two=files[,1],separator="", SIMPLIFY = FALSE,USE.NAMES = FALSE))

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
      csv_k_col_num  <- as.numeric(match(sample_data_input_cols[k,6],csv_j_headers))
      
      if(!(is.na(csv_k_col_num)))
      {
        #cat(sample_data_input_cols[k,6]," is in ",files[j,2], "\n")
        
        sample_data_all[,k] <- append(as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:csv_j_row_count,csv_k_col_num]),
                                      as.character(sample_data_all[(csv_j_row_count+1):nrow(sample_data_all),k]))
        
      } else
      {
        #cat(sample_data_input_cols[k,6]," not in ",files[j,2], "\n")
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
      csv_k_col_num  <- as.numeric(match(sample_data_input_cols[k,6],csv_j_headers))
      
      if(!(is.na(csv_k_col_num)))
      {
        #cat(sample_data_input_cols[k,6]," is in ",files[j,2], "\n")
        
        sample_data_all[,k] <- append(append(as.character(sample_data_all[1:(sample_data_NA_row_first-1),k]),
                                             as.character(read.csv(file=files[j,2],header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[1:(csv_j_row_count),csv_k_col_num])),
                                      as.character(sample_data_all[(sample_data_NA_row_first+csv_j_row_count):nrow(sample_data_all),k]))
        
      } else
      {
        #cat(sample_data_input_cols[k,6]," not in ",files[j,2], "\n")
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
  sample_data_all <- format_function(sample_data_all,sample_data_input_cols)
  
  #==============================================================================;
  #OUTPUT DATA;
  #==============================================================================;
  
  #Create temp data.frame of Sample data where not NA after each iteration
  temp_output_name1 <- paste("sample_data", formatC(j, width=6, format="d", flag="0"), sep = "")
  #assign(temp_output_name1, sample_data_all[!(rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))==ncol(sample_data_all)),], envir = .GlobalEnv)
  write.csv(sample_data_all[!(rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))==ncol(sample_data_all)),], file = paste(output_directory,temp_output_name1,".csv",sep=""))
  
  
  #Create temp data.frame of for each CSV file
  temp_output_name2 <- paste("input_csv", formatC(j, width=6, format="d", flag="0"), sep = "")
  #assign(temp_output_name2, sample_data_all[sample_data_all[,sample_data_file_col_num]==files[j,1] & !is.na(sample_data_all[,sample_data_file_col_num]),], envir = .GlobalEnv)
  write.csv(sample_data_all[sample_data_all[,sample_data_file_col_num]==files[j,1] & !is.na(sample_data_all[,sample_data_file_col_num]),], file = paste(output_directory,temp_output_name2,".csv",sep=""))
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  #Initiate garbage collection
  capture.output(gc(),file='NUL')
  
  progress_function(outer_loop_count=j, outer_loop_start_val=1, outer_loop_end_val=nrow(files), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
}

sample_data_all <- format_function(sample_data_all,sample_data_input_cols)
sample_data_all <- sample_data_all[rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))<ncol(sample_data_all),]
sample_data_all <- sample_data_all[!(is.na(sample_data_all[c("investment_objective_f")])) | !(is.na(sample_data_all[c("investment_strategy_f")])) | !(is.na(sample_data_all[c("principal_risks_f")])),]
write.csv(sample_data_all, file = paste(output_directory,"sample_data_all.csv",sep=""))
#xxx2 <- sample_data_all[!(is.na(sample_data_all[c("investment_objective_f","investment_strategy_f","principal_risks_f")])),]
#write.csv(xxx2, file = paste(output_directory,"xxx2.csv",sep=""))

rm(sample_data_all,sample_data_NA_row_index,temp_output_name1,temp_output_name2,csv_k_col_num)
capture.output(gc(),file='NUL')

#==============================================================================;
#CLEAN DATA;
#==============================================================================;

sample_data_all <- read.csv(file=paste(output_directory,"sample_data_all.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[-c(1)]
sample_data_all <- format_function(sample_data_all,sample_data_input_cols)

#sample_data_all <- sample_data000010

#Find most recent sample_dataXXXXXX
#sample_data_vector <- ls(pattern = 'sample_data[0-9]_*')
#sample_data_last <- max(sample_data_vector)

#test <- sample_data_all[245:260,]
#test_iois <- test[,names(sample_data_all)=="investment_objective_strategy_f"]
#test_pr <- test[,names(sample_data_all)=="principal_risks_f"]

#Get columns numbers for investment_objective_strategy_f and principal_risks_f
sample_data_io_f_col_num <- as.numeric(match("investment_objective_f",names(sample_data_all)))
sample_data_is_f_col_num <- as.numeric(match("investment_strategy_f",names(sample_data_all)))
sample_data_ios_f_col_num <- as.numeric(match("investment_objective_strategy_f",names(sample_data_all)))
sample_data_pr_f_col_num <- as.numeric(match("principal_risks_f",names(sample_data_all)))

#Merge investment_objective_f and investment_strategy_f and place in investment_objective_strategy_f
sample_data_all[,sample_data_ios_f_col_num] <- unlist(mapply(merge_cols_function,col_one=sample_data_all[,sample_data_io_f_col_num],col_two=sample_data_all[,sample_data_is_f_col_num],separator="\n\n", SIMPLIFY = FALSE,USE.NAMES = FALSE))

#Replace unformatted text with N/A's
#sample_data_all[,names(sample_data_all)=="investment_objective"] <- rep(NA, nrow(sample_data_all))
#sample_data_all[,names(sample_data_all)=="investment_strategy"] <- rep(NA, nrow(sample_data_all))
#sample_data_all[,names(sample_data_all)=="principal_risks"] <- rep(NA, nrow(sample_data_all))
#sample_data_all[,sample_data_io_f_col_num] <- rep(NA, nrow(sample_data_all))
#sample_data_all[,sample_data_is_f_col_num] <- rep(NA, nrow(sample_data_all))

#Remove all NAs rows left
sample_data_all <- sample_data_all[!(rowSums(is.na(sample_data_all[,1:ncol(sample_data_all)]))==ncol(sample_data_all)),]

#==============================================================================;
#COMPUTE READABILITY STATISTICS;
cat("SECTION: COMPUTE READABILITY STATISTICS", "\n")
#==============================================================================;

tagged_text_desc_stats <- c("lines","chars.no.space","letters.only","digits","normalized.space")
hyph_text_en_desc_stats <- c("num.syll")
readability_stats <- c("ARI","Coleman.Liau","Flesch.Kincaid","SMOG")
readability_desc_stats <- c("sentences","words","all.chars","punct","conjunctions","prepositions","pronouns","foreign","FOG.hard.words",
                            "TTR","sntc.per.word","avg.sentc.length","avg.word.length","avg.syll.word","sntc.per100","syll.per100","lett.per100")
token_stats <- c("token","desc")

for (l in 1:nrow(readbl_vars))
{
  #l <- 1
  #l <- 2
  
  sample_data_l_col_num  <- as.numeric(match(readbl_vars[l,1],sample_data_input_cols[,6]))
  
  sample_results <- pbsapply(sample_data_all[,sample_data_l_col_num],compute_readability_stats,
                             tagged_text_desc_measures=tagged_text_desc_stats,
                             hyph_text_en_desc_measures=hyph_text_en_desc_stats,
                             readability_measures=readability_stats,
                             readability_desc_measures=readability_desc_stats,
                             token_measures=token_stats,
                             simplify = FALSE, USE.NAMES = FALSE)
  
  sample_read_stats <- pblapply(sample_results, "[[","readstats")
  sample_read_stats <- pblapply(seq_along(sample_read_stats), function(x) data.frame(ID=x,sample_read_stats[x],stringsAsFactors=FALSE))
  sample_read_stats_df <- as.data.frame(do.call(rbind, sample_read_stats),stringsAsFactors=FALSE)
  sample_read_stats_df[,1] <- paste("", formatC(sample_read_stats_df[,1], width=6, format="d", flag="0"), sep = "")
  colnames(sample_read_stats_df) <- c("ID",tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats)
  write.csv(sample_read_stats_df, file = paste(output_directory,readbl_vars[l,3],".csv",sep=""))
  
  rm(sample_read_stats,sample_read_stats_df)
  capture.output(gc(),file='NUL')
  
  sample_tokens <- pblapply(sample_results, "[[","tokens") 
  sample_tokens <- pblapply(seq_along(sample_tokens), function(x) data.frame(ID=x,sample_tokens[x],stringsAsFactors=FALSE))
  sample_tokens_df <- as.data.frame(do.call(rbind, sample_tokens),stringsAsFactors=FALSE)
  sample_tokens_df[,1] <- paste("", formatC(sample_tokens_df[,1], width=6, format="d", flag="0"), sep = "")
  colnames(sample_tokens_df) <- c("ID",token_stats)
  write.csv(sample_tokens_df, file = paste(output_directory,readbl_vars[l,4],".csv",sep=""))
  
  rm(sample_tokens,sample_tokens_df)
  capture.output(gc(),file='NUL')
  
  rm(sample_data_l_col_num,sample_results)
  capture.output(gc(),file='NUL')
  
}


#==============================================================================;
###COMPUTE SIMILARITY STATISTICS;
cat("SECTION: COMPUTE READABILITY STATISTICS", "\n")
#==============================================================================;

#ADD ID IN READABILITY FUNCTION
#NOTE MERGE IN YR
#NOTE ADD REMOVE COLUMNS (NAs)
#FORMAT TOKENS

# tokens_all_ios_f <- read.csv(file=paste(input_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
# tokens_all_ios_f[,names(tokens_all_ios_f)=="ID"] <- as.character(tokens_all_ios_f[,names(tokens_all_ios_f)=="ID"])
# tokens_all_ios_f[,names(tokens_all_ios_f)=="yr"] <- as.numeric(tokens_all_ios_f[,names(tokens_all_ios_f)=="yr"])
# tokens_all_ios_f[,names(tokens_all_ios_f)=="token"] <- as.character(tokens_all_ios_f[,names(tokens_all_ios_f)=="token"])
# tokens_all_ios_f[,names(tokens_all_ios_f)=="desc"] <- as.character(tokens_all_ios_f[,names(tokens_all_ios_f)=="desc"])
# tokens_all_ios_f[,names(tokens_all_ios_f)=="Remove"] <- as.numeric(tokens_all_ios_f[,names(tokens_all_ios_f)=="Remove"])
# tokens_all_pr_f <- read.csv(file=paste(input_directory,"tokens_all_pr_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)
# tokens_all_pr_f[,names(tokens_all_pr_f)=="ID"] <- as.character(tokens_all_pr_f[,names(tokens_all_pr_f)=="ID"])
# tokens_all_pr_f[,names(tokens_all_pr_f)=="yr"] <- as.numeric(tokens_all_pr_f[,names(tokens_all_pr_f)=="yr"])
# tokens_all_pr_f[,names(tokens_all_pr_f)=="token"] <- as.character(tokens_all_pr_f[,names(tokens_all_pr_f)=="token"])
# tokens_all_pr_f[,names(tokens_all_pr_f)=="desc"] <- as.character(tokens_all_pr_f[,names(tokens_all_pr_f)=="desc"])
# tokens_all_pr_f[,names(tokens_all_pr_f)=="Remove"] <- as.numeric(tokens_all_pr_f[,names(tokens_all_pr_f)=="Remove"])

for (m in 1:nrow(readbl_vars))
{
  
  #m <- 1
  #m <- 2
  
  ###Clear tokens_all_temp table
  tokens_all_temp[,names(tokens_all_temp)=="ID"] <- rep(NA, nrow(tokens_all_temp))
  tokens_all_temp[,names(tokens_all_temp)=="yr"] <- rep(NA, nrow(tokens_all_temp))
  tokens_all_temp[,names(tokens_all_temp)=="token"] <- rep(NA, nrow(tokens_all_temp))
  tokens_all_temp[,names(tokens_all_temp)=="desc"] <- rep(NA, nrow(tokens_all_temp))
  tokens_all_temp[,names(tokens_all_temp)=="Remove"] <- rep(NA, nrow(tokens_all_temp))
  
  cat("Token table: ",readbl_vars[m,3], "\n")
  if (m==1)
  {
    #tokens_all_temp[1:nrow(tokens_all_ios_f),] <- tokens_all_ios_f
    #tokens_all_ios_f <- tokens_all_temp
    #tokens_all_temp <- get(readbl_vars[m,3])
    
    tokens_all_ios_f <- read.csv(file=paste(output_directory,"tokens_all_ios_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[-c(1)]
    
    tokens_all_temp[1:nrow(tokens_all_ios_f),] <- tokens_all_ios_f[1:nrow(tokens_all_ios_f),]
    
    rm(tokens_all_ios_f)
    
  } else if (m==2)
  {
    #tokens_all_temp[1:nrow(tokens_all_pr_f),] <- tokens_all_pr_f
    #tokens_all_pr_f <- tokens_all_temp
    #tokens_all_temp <- get(readbl_vars[m,3])
    
    tokens_all_pr_f <- read.csv(file=paste(output_directory,"tokens_all_pr_f.csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[-c(1)]
    
    tokens_all_temp[1:nrow(tokens_all_pr_f),] <- tokens_all_pr_f[1:nrow(tokens_all_pr_f),]
    
    rm(tokens_all_pr_f)
    
  } else
  {
    cat("ERROR WHEN FINDING CORRECT READABILITY COLUMN", "\n")
    
  }
  
  capture.output(gc(),file='NUL')
  
  #Format token temp
  tokens_all_temp <- format_function(tokens_all_temp,tokens_all_cols)
  
  #Remove all NAs rows left
  tokens_all_temp <- tokens_all_temp[!(rowSums(is.na(tokens_all_temp[,1:ncol(tokens_all_temp)]))==ncol(tokens_all_temp)),]
  
  #Convert Ticker columns to character type
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- as.character(tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Remove multiple spaces
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- gsub(pattern=" {2,}", replacement=" ", x=tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Remove punctuation
  #tokens_all_temp[,names(tokens_all_temp)=="token"]  <- gsub(pattern="[[:punct:]]", replacement=" ", x=tokens_all_temp[,names(tokens_all_temp)=="token"])
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- gsub(pattern="[^[:alnum:]]", replacement=" ", x=tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Remove numbers
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- gsub(pattern="\\d", replacement="", x=tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Trim strings
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- trim(tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Upcase strings
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- toupper(tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Add back in removed hyphens
  #tokens_all_temp[,5] <- with(tokens_all_temp, nchar(as.character(token))) 
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- gsub(pattern=" ", replacement="-", x=tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  #Add back in removed parentheses
  parentheses_list <- sapply(tokens_all_temp[,names(tokens_all_temp)=="token"], str_sub,start=-2)
  parentheses_list <- as.data.frame(parentheses_list)
  tokens_all_temp[,names(tokens_all_temp)=="Remove"] <- parentheses_list[,1]
  tokens_all_temp[,names(tokens_all_temp)=="token"]  <- gsub(pattern=" ", replacement="-", x=tokens_all_temp[,names(tokens_all_temp)=="token"])
  
  rm(parentheses_list)
  capture.output(gc(),file='NUL')
  
  token_temp_col_num <- match("token",names(tokens_all_temp))
  remove_temp_col_num <- match("Remove",names(tokens_all_temp))
  
  parentheses_criteria <- which(((tokens_all_temp[,remove_temp_col_num])=="-S") | ((tokens_all_temp[,remove_temp_col_num])=="-D") | ((tokens_all_temp[,remove_temp_col_num])=="-T"))
  for(n in parentheses_criteria)
  {
    
    tokens_all_temp[n,token_temp_col_num] <- paste(substr(tokens_all_temp[n,token_temp_col_num] , 1, (token_temp_char_count-2)), 
                                                   substr(tokens_all_temp[n,token_temp_col_num], token_temp_char_count, token_temp_char_count), sep="'")
    
    progress_function(outer_loop_count=n, outer_loop_start_val=1, outer_loop_end_val=nrow(tokens_all_temp), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  
  rm(parentheses_criteria)
  capture.output(gc(),file='NUL')
  
  #   for (n in 1:nrow(tokens_all_temp))
  #   {
  #     #n <- 1
  #     #n <- 62
  #     
  #     if (((tokens_all_temp[n,remove_temp_col_num])=="-S") | ((tokens_all_temp[n,remove_temp_col_num])=="-D") | ((tokens_all_temp[n,remove_temp_col_num])=="-T"))
  #     {
  #       token_temp_char_count <- as.numeric(nchar(tokens_all_temp[n,token_temp_col_num] ))
  #       tokens_all_temp[n,token_temp_col_num] <- paste(substr(tokens_all_temp[n,token_temp_col_num] , 1, (token_temp_char_count-2)), 
  #                                                      substr(tokens_all_temp[n,token_temp_col_num], token_temp_char_count, token_temp_char_count), sep="'")
  #       
  #     }
  #     
  #     progress_function(outer_loop_count=n, outer_loop_start_val=1, outer_loop_end_val=nrow(tokens_all_temp), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  #     
  #   }
  
  
  
  #Default Remove back to 0
  tokens_all_temp[,names(tokens_all_temp)=="Remove"] <- as.numeric(rep(NA, nrow(tokens_all_temp)))
  
  #Find which rows to remove
  remove_descriptions <- c("Cardinal number","Comma","Sentence ending punctuation","Symbol",
                           "Opening bracket","Closing bracket","Quote","End quote")
  remove_tokens <- c("%","&","\t","-","--","---","'",""," ","1-800-XXX-XXXX","XXX-XXX-XXXX","XXX-XXXX")
  tokens_all_temp[!(tokens_all_temp$desc %in% remove_descriptions) & !(tokens_all_temp$token %in% remove_tokens), "Remove"] <- 0
  tokens_all_temp[ (tokens_all_temp$desc %in% remove_descriptions) |  (tokens_all_temp$token %in% remove_tokens), "Remove"] <- 1
  
  #Sort tokens_all_temp
  tokens_all_temp  <- tokens_all_temp[order(tokens_all_temp[,names(tokens_all_temp)=="ID"],tokens_all_temp[,names(tokens_all_temp)=="yr"],tokens_all_temp[,names(tokens_all_temp)=="token"]),]
  
  #Only keep observations where Remove=0
  tokens_all_temp1 <- tokens_all_temp[tokens_all_temp[,names(tokens_all_temp)=="Remove"]==0,]
  
  #Group words and get counts
  query_tokens_all_temp2 <- "select distinct  ID, yr, Upper(token) token, Count(token) Count, Remove
                             from             tokens_all_temp1 
                             where            Remove=0
                             group by         ID, yr, Upper(token)"
  tokens_all_temp2 <- sqldf(query_tokens_all_temp2)
  
  #Create uTotal, gTotal, and percentage
  tokens_all_temp2 <- ddply(tokens_all_temp2, "ID", function(x) data.frame(x, uTotal=nrow(x),gTotal=sum(x$Count),Total_Percentage=(x$Count)/sum(x$Count)) )
  
  #==============================================================================;
  #GLOBAL DICTIONARY (AGGREGATE);
  cat("SECTION: GLOBAL DICTIONARY (AGGREGATE)", "\n")
  #
  # global_agg_word_grand_temp: total words across ids 
  # global_agg_word_unique_temp: total unique words across ids 
  #
  #==============================================================================;
  
  #==============================================================================;
  #Create global aggregate grand word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_agg_word_grand_temp
  global_agg_word_grand_temp <- tokens_all_temp[tokens_all_temp[,names(tokens_all_temp)=="Remove"]==0,c("token","ID")]
  
  #Remove all NAs rows left
  global_agg_word_grand_temp <- global_agg_word_grand_temp[!(rowSums(is.na(global_agg_word_grand_temp[,1:ncol(global_agg_word_grand_temp)]))==ncol(global_agg_word_grand_temp)),]
  
  #Sort global_agg_word_grand_temp
  global_agg_word_grand_temp  <- global_agg_word_grand_temp[order(global_agg_word_grand_temp[,names(global_agg_word_grand_temp)=="ID"],global_agg_word_grand_temp[,names(global_agg_word_grand_temp)=="token"]),] 
  
  query_global_agg_word_grand_temp2 <- "select distinct  Upper(token) token, Count(token) Count_word_grand
                                        from             global_agg_word_grand_temp 
                                        group by         Upper(token)
                                        order by         Count_word_grand desc, token"
  global_agg_word_grand_temp2 <- sqldf(query_global_agg_word_grand_temp2)
  
  #Create global_word_grand tables
  global_agg_word_grand_temp3 <- as.data.frame(matrix(NA, ncol = (ncol(global_agg_word_grand_temp2)+3+(nrow(percentiles)*2)), nrow = nrow(global_agg_word_grand_temp2)))
  global_agg_word_grand_temp3_cols <- append(names(global_agg_word_grand_temp2),c("uTotal_word_grand","gTotal_word_grand","Total_Percentage_word_grand"))
  global_agg_word_grand_temp3_cols <- append(global_agg_word_grand_temp3_cols,paste(percentiles[,names(percentiles)=="Column_lbl"],"_word_grand",sep=""))
  global_agg_word_grand_temp3_cols <- append(global_agg_word_grand_temp3_cols,paste(percentiles[,names(percentiles)=="Column_DV"],"_word_grand",sep=""))
  colnames(global_agg_word_grand_temp3) <- global_agg_word_grand_temp3_cols
  
  #Populate global_agg_word_grand_temp3 with columns from global_agg_temp2
  for (o in 1:ncol(global_agg_word_grand_temp2))
  {
    #o <- 1
    global_agg_word_grand_temp3[1:nrow(global_agg_word_grand_temp2),names(global_agg_word_grand_temp3)==colnames(global_agg_word_grand_temp2)[o]] <- global_agg_word_grand_temp2[1:nrow(global_agg_word_grand_temp2),o]
  }
  
  #Create uTotal_word_unique, gTotal_word_grand, and Total_Percentage_word_grand
  global_agg_word_grand_temp3[,names(global_agg_word_grand_temp3)=="uTotal_word_grand"] <- nrow(global_agg_word_grand_temp2)
  global_agg_word_grand_temp3[,names(global_agg_word_grand_temp3)=="gTotal_word_grand"] <- sum(global_agg_word_grand_temp2[,names(global_agg_word_grand_temp2)=="Count_word_grand"])
  global_agg_word_grand_temp3[,names(global_agg_word_grand_temp3)=="Total_Percentage_word_grand"] <- (global_agg_word_grand_temp3[,names(global_agg_word_grand_temp3)=="Count_word_grand"])/(global_agg_word_grand_temp3[,names(global_agg_word_grand_temp3)=="gTotal_word_grand"])
  
  #Create Word_Cutoff_XXXpct and Word_DV_XXXpct
  for (p in 1:nrow(percentiles))
  {
    #p <- 1
    Word_cutoff_name_temp <- paste(percentiles[p,names(percentiles)=="Column_lbl"],"_word_grand",sep="")
    Word_DV_name_temp <- paste(percentiles[p,names(percentiles)=="Column_DV"],"_word_grand",sep="")
    #Word_cutoff_value_temp <- ceil(percentiles[p,names(percentiles)=="Significance_Level"]*global_agg_word_grand_temp3[1,names(global_agg_word_grand_temp3)=="uTotal_word_grand"])
    Word_cutoff_value_temp <- ceil(percentiles[p,names(percentiles)=="Confidence_Level"]*global_agg_word_grand_temp3[1,names(global_agg_word_grand_temp3)=="uTotal_word_grand"])
    
    Cutoff_row_count_value_temp <- as.numeric(global_agg_word_grand_temp2[Word_cutoff_value_temp,names(global_agg_word_grand_temp2)=="Count_word_grand"])
    Cutoff_row_count_value_vector_temp <- which(global_agg_word_grand_temp2[,names(global_agg_word_grand_temp2)=="Count_word_grand"]==Cutoff_row_count_value_temp)
    Cutoff_row_count_value_vector_last_temp <- as.numeric(max(Cutoff_row_count_value_vector_temp))
    
    global_agg_word_grand_temp3[,names(global_agg_word_grand_temp3)==Word_cutoff_name_temp] <- Cutoff_row_count_value_vector_last_temp
    
    if (Cutoff_row_count_value_vector_last_temp == nrow(global_agg_word_grand_temp3))
    {
      global_agg_word_grand_temp3[1:nrow(global_agg_word_grand_temp3),names(global_agg_word_grand_temp3)==Word_DV_name_temp] <- 0
      
    } else
    {
      global_agg_word_grand_temp3[1:Cutoff_row_count_value_vector_last_temp,names(global_agg_word_grand_temp3)==Word_DV_name_temp] <- 0
      global_agg_word_grand_temp3[(Cutoff_row_count_value_vector_last_temp+1):nrow(global_agg_word_grand_temp3),names(global_agg_word_grand_temp3)==Word_DV_name_temp] <- 1
      
    }
    
  }
  
  rm(global_agg_word_grand_temp,global_agg_word_grand_temp2,global_agg_word_grand_temp3_cols)
  
  
  #==============================================================================;
  #Create global aggregate unique word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_agg_word_unique_temp
  global_agg_word_unique_temp <- tokens_all_temp2[tokens_all_temp2[,names(tokens_all_temp2)=="Remove"]==0,c("token","ID")]
  
  #Remove all NAs rows left
  global_agg_word_unique_temp <- global_agg_word_unique_temp[!(rowSums(is.na(global_agg_word_unique_temp[,1:ncol(global_agg_word_unique_temp)]))==ncol(global_agg_word_unique_temp)),]
  
  #Sort global_agg_word_unique_temp
  global_agg_word_unique_temp <- global_agg_word_unique_temp[order(global_agg_word_unique_temp[,names(global_agg_word_unique_temp)=="ID"],global_agg_word_unique_temp[,names(global_agg_word_unique_temp)=="token"]),]
  
  query_global_agg_word_unique_temp2 <- "select distinct  Upper(token) token, Count(token) Count_word_unique
                                         from             global_agg_word_unique_temp 
                                         group by         Upper(token)
                                         order by         Count_word_unique desc, token"
  global_agg_word_unique_temp2 <- sqldf(query_global_agg_word_unique_temp2)
  
  #Create global_word_unique tables
  global_agg_word_unique_temp3 <- as.data.frame(matrix(NA, ncol = (ncol(global_agg_word_unique_temp2)+3+(nrow(percentiles)*2)), nrow = nrow(global_agg_word_unique_temp2)))
  global_agg_word_unique_temp3_cols <- append(names(global_agg_word_unique_temp2),c("uTotal_word_unique","gTotal_word_unique","Total_Percentage_word_unique"))
  global_agg_word_unique_temp3_cols <- append(global_agg_word_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_lbl"],"_word_unique",sep=""))
  global_agg_word_unique_temp3_cols <- append(global_agg_word_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_DV"],"_word_unique",sep=""))
  colnames(global_agg_word_unique_temp3) <- global_agg_word_unique_temp3_cols
  
  #Populate global_agg_word_unique_temp3 with columns from global_agg_temp2
  for (o in 1:ncol(global_agg_word_unique_temp2))
  {
    #o <- 1
    global_agg_word_unique_temp3[1:nrow(global_agg_word_unique_temp2),names(global_agg_word_unique_temp3)==colnames(global_agg_word_unique_temp2)[o]] <- global_agg_word_unique_temp2[1:nrow(global_agg_word_unique_temp2),o]
  }
  
  #Create uTotal_word_unique, gTotal_word_unique, and Total_Percentage_word_unique
  global_agg_word_unique_temp3[,names(global_agg_word_unique_temp3)=="uTotal_word_unique"] <- nrow(global_agg_word_unique_temp2)
  global_agg_word_unique_temp3[,names(global_agg_word_unique_temp3)=="gTotal_word_unique"] <- sum(global_agg_word_unique_temp2[,names(global_agg_word_unique_temp2)=="Count_word_unique"])
  global_agg_word_unique_temp3[,names(global_agg_word_unique_temp3)=="Total_Percentage_word_unique"] <- (global_agg_word_unique_temp3[,names(global_agg_word_unique_temp3)=="Count_word_unique"])/(global_agg_word_unique_temp3[,names(global_agg_word_unique_temp3)=="gTotal_word_unique"])
  
  #Create Word_Cutoff_XXXpct and Word_DV_XXXpct
  for (p in 1:nrow(percentiles))
  {
    #p <- 1
    Word_cutoff_name_temp <- paste(percentiles[p,names(percentiles)=="Column_lbl"],"_word_unique",sep="")
    Word_DV_name_temp <- paste(percentiles[p,names(percentiles)=="Column_DV"],"_word_unique",sep="")
    #Word_cutoff_value_temp <- ceil(percentiles[p,names(percentiles)=="Significance_Level"]*global_agg_word_unique_temp3[1,names(global_agg_word_unique_temp3)=="uTotal_word_unique"])
    Word_cutoff_value_temp <- ceil(percentiles[p,names(percentiles)=="Confidence_Level"]*global_agg_word_unique_temp3[1,names(global_agg_word_unique_temp3)=="uTotal_word_unique"])  
    
    Cutoff_row_count_value_temp <- as.numeric(global_agg_word_unique_temp2[Word_cutoff_value_temp,names(global_agg_word_unique_temp2)=="Count_word_unique"])
    Cutoff_row_count_value_vector_temp <- which(global_agg_word_unique_temp2[,names(global_agg_word_unique_temp2)=="Count_word_unique"]==Cutoff_row_count_value_temp)
    Cutoff_row_count_value_vector_last_temp <- as.numeric(max(Cutoff_row_count_value_vector_temp))
    
    global_agg_word_unique_temp3[,names(global_agg_word_unique_temp3)==Word_cutoff_name_temp] <- Cutoff_row_count_value_vector_last_temp
    
    if (Cutoff_row_count_value_vector_last_temp == nrow(global_agg_word_unique_temp3))
    {
      global_agg_word_unique_temp3[1:nrow(global_agg_word_unique_temp3),names(global_agg_word_unique_temp3)==Word_DV_name_temp] <- 0
      
    } else
    {
      global_agg_word_unique_temp3[1:Cutoff_row_count_value_vector_last_temp,names(global_agg_word_unique_temp3)==Word_DV_name_temp] <- 0
      global_agg_word_unique_temp3[(Cutoff_row_count_value_vector_last_temp+1):nrow(global_agg_word_unique_temp3),names(global_agg_word_unique_temp3)==Word_DV_name_temp] <- 1
      
    }
    
  }

  rm(global_agg_word_unique_temp,global_agg_word_unique_temp2,global_agg_word_unique_temp3_cols)
  
  
  #==============================================================================;
  #Create global aggregate unique id table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_agg_id_unique_temp
  global_agg_id_unique_temp <- tokens_all_temp2[tokens_all_temp2[,names(tokens_all_temp2)=="Remove"]==0,c("token","ID")]
  
  #Remove all NAs rows left
  global_agg_id_unique_temp <- global_agg_id_unique_temp[!(rowSums(is.na(global_agg_id_unique_temp[,1:ncol(global_agg_id_unique_temp)]))==ncol(global_agg_id_unique_temp)),]
  
  #Sort global_agg_id_unique_temp
  global_agg_id_unique_temp <- global_agg_id_unique_temp[order(global_agg_id_unique_temp[,names(global_agg_id_unique_temp)=="ID"],global_agg_id_unique_temp[,names(global_agg_id_unique_temp)=="token"]),]
  
  query_global_agg_id_unique_temp2 <- "select distinct  Upper(token) token, Count(token) Count_id_unique
                                       from             global_agg_id_unique_temp 
                                       group by         Upper(token)
                                       order by         Count_id_unique desc, token"
  global_agg_id_unique_temp2 <- sqldf(query_global_agg_id_unique_temp2)
  
  #Create global_id_unique tables
  global_agg_id_unique_temp3 <- as.data.frame(matrix(NA, ncol = (ncol(global_agg_id_unique_temp2)+2+(nrow(percentiles)*2)), nrow = nrow(global_agg_id_unique_temp2)))
  global_agg_id_unique_temp3_cols <- append(names(global_agg_id_unique_temp2),c("gTotal_id_unique","Total_Percentage_id_unique"))
  global_agg_id_unique_temp3_cols <- append(global_agg_id_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_lbl"],"_id_unique",sep=""))
  global_agg_id_unique_temp3_cols <- append(global_agg_id_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_DV"],"_id_unique",sep=""))
  colnames(global_agg_id_unique_temp3) <- global_agg_id_unique_temp3_cols
  
  #Populate global_agg_id_unique_temp3 with columns from global_agg_temp2
  for (o in 1:ncol(global_agg_id_unique_temp2))
  {
    #o <- 1
    global_agg_id_unique_temp3[1:nrow(global_agg_id_unique_temp2),names(global_agg_id_unique_temp3)==colnames(global_agg_id_unique_temp2)[o]] <- global_agg_id_unique_temp2[1:nrow(global_agg_id_unique_temp2),o]
  } 
  
  #Get list of all unique ids
  unique_ids <- as.data.frame(unique(tokens_all_temp1[,c("ID")], incomparables = FALSE))
  colnames(unique_ids) <- "ID"
  
  #Create uTotal_id_unique, gTotal_id_unique, and Total_Percentage_id_unique
  global_agg_id_unique_temp3[,names(global_agg_id_unique_temp3)=="gTotal_id_unique"] <- nrow(unique_ids)
  global_agg_id_unique_temp3[,names(global_agg_id_unique_temp3)=="Total_Percentage_id_unique"] <- (global_agg_id_unique_temp3[,names(global_agg_id_unique_temp3)=="Count_id_unique"])/(global_agg_id_unique_temp3[,names(global_agg_id_unique_temp3)=="gTotal_id_unique"])
  
  #Create id_Cutoff_XXXpct and id_DV_XXXpct
  for (p in 1:nrow(percentiles))
  {
    #p <- 1
    id_cutoff_name_temp <- paste(percentiles[p,names(percentiles)=="Column_lbl"],"_id_unique",sep="")
    id_DV_name_temp <- paste(percentiles[p,names(percentiles)=="Column_DV"],"_id_unique",sep="")
    #id_cutoff_value_temp <- ceil(percentiles[p,names(percentiles)=="Significance_Level"]*global_agg_id_unique_temp3[1,names(global_agg_id_unique_temp3)=="gTotal_id_unique"])
    id_cutoff_value_temp <- ceil(percentiles[p,names(percentiles)=="Confidence_Level"]*global_agg_id_unique_temp3[1,names(global_agg_id_unique_temp3)=="gTotal_id_unique"])
    
    Cutoff_row_count_value_temp <- as.numeric(global_agg_id_unique_temp2[id_cutoff_value_temp,names(global_agg_id_unique_temp2)=="Count_id_unique"])
    Cutoff_row_count_value_vector_temp <- which(global_agg_id_unique_temp2[,names(global_agg_id_unique_temp2)=="Count_id_unique"]==Cutoff_row_count_value_temp)
    Cutoff_row_count_value_vector_last_temp <- as.numeric(max(Cutoff_row_count_value_vector_temp))
    
    global_agg_id_unique_temp3[,names(global_agg_id_unique_temp3)==id_cutoff_name_temp] <- Cutoff_row_count_value_vector_last_temp
    
    
    if (Cutoff_row_count_value_vector_last_temp == nrow(global_agg_id_unique_temp3))
    {
      global_agg_id_unique_temp3[1:nrow(global_agg_id_unique_temp3),names(global_agg_id_unique_temp3)==id_DV_name_temp] <- 0
      
    } else
    {
      global_agg_id_unique_temp3[1:Cutoff_row_count_value_vector_last_temp,names(global_agg_id_unique_temp3)==id_DV_name_temp] <- 0
      global_agg_id_unique_temp3[(Cutoff_row_count_value_vector_last_temp+1):nrow(global_agg_id_unique_temp3),names(global_agg_id_unique_temp3)==id_DV_name_temp] <- 1
      
    }
  }

  rm(global_agg_id_unique_temp,global_agg_id_unique_temp2,global_agg_id_unique_temp3_cols)
  
  
  #==============================================================================;
  #Merge the ID DVs with the Word DVs (aggregate);
  #==============================================================================;
  
  global_agg_comb1 <- merge(global_agg_word_grand_temp3, global_agg_word_unique_temp3, by.x = "token" , by.y = "token" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
  global_agg_comb2 <- merge(global_agg_comb1, global_agg_id_unique_temp3, by.x = "token" , by.y = "token" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
  
  global_agg_count_vector <- grep(pattern = '*Count_*', colnames(global_agg_comb2), ignore.case = FALSE, perl = FALSE, value = TRUE)
  global_agg_dv_vector <- grep(pattern = '*_DV_*', colnames(global_agg_comb2), ignore.case = FALSE, perl = FALSE, value = TRUE)
  global_agg_col_vector <- append(global_agg_count_vector,global_agg_dv_vector)
  global_agg_col_vector <- append("token",global_agg_col_vector)
  global_agg_comb  <- global_agg_comb2[,global_agg_col_vector]
  
  rm(global_agg_comb1,global_agg_comb2,global_agg_word_grand_temp3,global_agg_word_unique_temp3,global_agg_id_unique_temp3)
  capture.output(gc(),file='NUL')
  
  #==============================================================================;
  #Create global aggregate tables based on the percentiles;
  #==============================================================================;
  
  global_agg_dv_vector_used <- as.data.frame(cbind(global_agg_dv_vector,rep(NA,length(global_agg_dv_vector))),stringsAsFactors=FALSE)
  colnames(global_agg_dv_vector_used)[2] <- "Column_not_all_0"
  
  #Create id_Cutoff_XXXpct and id_DV_XXXpct
  for (q in 1:nrow(percentiles))
  {
    #q <- 1
    
    #==============================================================================;
    #Word Grand;
    #==============================================================================;
    
    temp_word_grand_column <- paste(percentiles[q,names(percentiles)=="Column_DV"],"_word_grand",sep="")
    
    if (sum(global_agg_comb[,names(global_agg_comb)==temp_word_grand_column])==0)
    {
      global_agg_dv_vector_used[global_agg_dv_vector_used[,1]==temp_word_grand_column,2] <- 0
            
      cat("Column", temp_word_grand_column,"is empty -- no global dictionary for this column", "\n")

      
    } else
    {
      global_agg_dv_vector_used[global_agg_dv_vector_used[,1]==temp_word_grand_column,2] <- 1
      
      query_global_agg_word_grand_percentiles <- paste("select distinct  Upper(token) token, Count_word_grand
                                                        from             global_agg_comb 
                                                        where ",         temp_word_grand_column,"=1
                                                        group by         Upper(token)
                                                        order by         Count_word_grand desc, token", sep = "") 

      temp_word_grand_df <- create_stats_function(sqldf(query_global_agg_word_grand_percentiles),"Count_word_grand")
      temp_word_grand_df_name <- paste("global_agg_word_grand", percentiles[q,names(percentiles)=="Confidence_lbl"], sep = "")
      
      write.csv(temp_word_grand_df, file = paste(output_directory,temp_word_grand_df_name,".csv",sep=""))
      assign(temp_word_grand_df_name, temp_word_grand_df, envir = .GlobalEnv)
      
      rm(temp_word_grand_df,temp_word_grand_df_name)
      
    }
    
    rm(temp_word_grand_column)
    capture.output(gc(),file='NUL')

    #==============================================================================;
    #Word Unique;
    #==============================================================================;
    
    temp_word_unique_column <- paste(percentiles[q,names(percentiles)=="Column_DV"],"_word_unique",sep="")
    
    if (sum(global_agg_comb[,names(global_agg_comb)==temp_word_unique_column])==0)
    {
      global_agg_dv_vector_used[global_agg_dv_vector_used[,1]==temp_word_unique_column,2] <- 0
      
      cat("Column", temp_word_unique_column,"is empty -- no global dictionary for this column", "\n")
      
    } else
    {
      global_agg_dv_vector_used[global_agg_dv_vector_used[,1]==temp_word_unique_column,2] <- 1
      
      query_global_agg_word_unique_percentiles <- paste("select distinct  Upper(token) token, Count_word_unique
                                                         from             global_agg_comb 
                                                         where ",         temp_word_unique_column,"=1
                                                         group by         Upper(token)
                                                         order by         Count_word_unique desc, token", sep = "") 

      temp_word_unique_df <- create_stats_function(sqldf(query_global_agg_word_unique_percentiles),"Count_word_unique")
      temp_word_unique_df_name <- paste("global_agg_word_unique", percentiles[q,names(percentiles)=="Confidence_lbl"], sep = "")
      
      write.csv(temp_word_unique_df, file = paste(output_directory,temp_word_unique_df_name,".csv",sep=""))
      assign(temp_word_unique_df_name, temp_word_unique_df, envir = .GlobalEnv)
      
      rm(temp_word_unique_df,temp_word_unique_df_name)
    
    }
    
    rm(temp_word_unique_column)
    capture.output(gc(),file='NUL')
    
    
    #==============================================================================;
    #ID Unique;
    #==============================================================================;
    
    temp_id_unique_column <- paste(percentiles[q,names(percentiles)=="Column_DV"],"_id_unique",sep="")
    
    if (sum(global_agg_comb[,names(global_agg_comb)==temp_id_unique_column])==0)
    {
      global_agg_dv_vector_used[global_agg_dv_vector_used[,1]==temp_id_unique_column,2] <- 0
      
      cat("Column", temp_id_unique_column,"is empty -- no global dictionary for this column", "\n")
      
    } else
    {
      global_agg_dv_vector_used[global_agg_dv_vector_used[,1]==temp_id_unique_column,2] <- 1
    
      query_global_agg_id_unique_percentiles <- paste("select distinct  Upper(token) token, Count_id_unique
                                                       from             global_agg_comb 
                                                       where ",         temp_id_unique_column,"=1
                                                       group by         Upper(token)
                                                       order by         Count_id_unique desc, token", sep = "") 
      temp_id_unique_df <- create_stats_function(sqldf(query_global_agg_id_unique_percentiles),"Count_id_unique")
      temp_id_unique_df_name <- paste("global_agg_id_unique", percentiles[q,names(percentiles)=="Confidence_lbl"], sep = "")
      
      write.csv(temp_id_unique_df, file = paste(output_directory,temp_id_unique_df_name,".csv",sep=""))
      assign(temp_id_unique_df_name, temp_id_unique_df, envir = .GlobalEnv)
      
      rm(temp_id_unique_df,temp_id_unique_df_name)
    }
    
    rm(temp_id_unique_column)
    capture.output(gc(),file='NUL')
  }
  
  rm(Cutoff_row_count_value_vector_temp,Cutoff_row_count_value_vector_last_temp)
  rm(Word_cutoff_name_temp,Word_DV_name_temp,id_cutoff_name_temp,id_DV_name_temp)
  rm(global_agg_count_vector,global_agg_dv_vector,global_agg_col_vector)
  capture.output(gc(),file='NUL')
  
  #==============================================================================;
  #GLOBAL DICTIONARY (YEARLY);
  cat("SECTION: GLOBAL DICTIONARY (YEARLY)", "\n")
  #==============================================================================;
  
  #==============================================================================;
  #Create global yearly grand word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_year_word_grand_temp
  global_year_word_grand_temp <- tokens_all_temp[tokens_all_temp[,names(tokens_all_temp)=="Remove"]==0,c("token","yr","ID")]
  
  #Remove all NAs rows left
  global_year_word_grand_temp <- global_year_word_grand_temp[!(rowSums(is.na(global_year_word_grand_temp[,1:ncol(global_year_word_grand_temp)]))==ncol(global_year_word_grand_temp)),]
  
  #Sort global_year_word_grand_temp
  global_year_word_grand_temp  <- global_year_word_grand_temp[order(global_year_word_grand_temp[,names(global_year_word_grand_temp)=="yr"], global_year_word_grand_temp[,names(global_year_word_grand_temp)=="ID"],global_year_word_grand_temp[,names(global_year_word_grand_temp)=="token"]),] 
  
  query_global_year_word_grand_temp2 <- "select distinct   yr, Upper(token) token, Count(token) Count_word_grand
                                         from              global_year_word_grand_temp 
                                         group by          yr, Upper(token)
                                         order by          yr, Count_word_grand desc, token"
  global_year_word_grand_temp2 <- sqldf(query_global_year_word_grand_temp2)
  
  #Create global_word_grand tables
  global_year_word_grand_temp3 <- as.data.frame(matrix(NA, ncol = (ncol(global_year_word_grand_temp2)+3+(nrow(percentiles)*2)), nrow = nrow(global_year_word_grand_temp2)))
  global_year_word_grand_temp3_cols <- append(names(global_year_word_grand_temp2),c("uTotal_word_grand","gTotal_word_grand","Total_Percentage_word_grand"))
  global_year_word_grand_temp3_cols <- append(global_year_word_grand_temp3_cols,paste(percentiles[,names(percentiles)=="Column_lbl"],"_word_grand",sep=""))
  global_year_word_grand_temp3_cols <- append(global_year_word_grand_temp3_cols,paste(percentiles[,names(percentiles)=="Column_DV"],"_word_grand",sep=""))
  colnames(global_year_word_grand_temp3) <- global_year_word_grand_temp3_cols
  
  #Populate global_year_word_grand_temp3 with columns from global_year_temp2
  for (r in 1:ncol(global_year_word_grand_temp2))
  {
    #r <- 1
    global_year_word_grand_temp3[1:nrow(global_year_word_grand_temp2),names(global_year_word_grand_temp3)==colnames(global_year_word_grand_temp2)[r]] <- global_year_word_grand_temp2[1:nrow(global_year_word_grand_temp2),r]
  }
  
  #Create uTotal_word_unique, gTotal_word_grand, and Total_Percentage_word_grand
  global_year_word_grand_temp3[,names(global_year_word_grand_temp3)=="uTotal_word_grand"] <- as.numeric(ddply(global_year_word_grand_temp2, "yr", function(x) data.frame(x,nrow=nrow(x)))$nrow)
  global_year_word_grand_temp3[,names(global_year_word_grand_temp3)=="gTotal_word_grand"] <- as.numeric(ddply(global_year_word_grand_temp2, "yr", function(x) data.frame(x,sum=sum(x$Count_word_grand)))$sum)
  global_year_word_grand_temp3[,names(global_year_word_grand_temp3)=="Total_Percentage_word_grand"] <- as.numeric(ddply(global_year_word_grand_temp2, "yr", function(x) data.frame(x,tp=((x$Count_word_grand)/sum(x$Count_word_grand))))$tp)
  
  #Create Word_Cutoff_XXXpct and Word_DV_XXXpct
  for (s in 1:nrow(percentiles))
  {
    #s <- 1
    Word_cutoff_name_temp <- paste(percentiles[s,names(percentiles)=="Column_lbl"],"_word_grand",sep="")
    Word_DV_name_temp <- paste(percentiles[s,names(percentiles)=="Column_DV"],"_word_grand",sep="")
    #Word_cutoff_value_temp1 <- percentiles[s,names(percentiles)=="Significance_Level"]
    Word_cutoff_value_temp1 <- percentiles[s,names(percentiles)=="Confidence_Level"]
    Word_cutoff_value_temp2 <- ddply(global_year_word_grand_temp3, "yr", function(x) data.frame(cutoff=ceil(Word_cutoff_value_temp1*nrow(x))))
    Word_cutoff_value_temp3 <- unique(Word_cutoff_value_temp2[,c("yr","cutoff")], incomparables = FALSE)
    
    global_year_word_grand_temp2a <- merge(global_year_word_grand_temp2, Word_cutoff_value_temp3, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    Cutoff_row_count_value_temp1 <- ddply(global_year_word_grand_temp2a, "yr", function(x) data.frame(x[x$cutoff,]))
    Cutoff_row_count_value_temp2 <- unique(Cutoff_row_count_value_temp1[,c("yr","Count_word_grand")], incomparables = FALSE)        
    names(Cutoff_row_count_value_temp2)[2] <- "Count_word_grand_cutoff"
    
    global_year_word_grand_temp2b <- merge(global_year_word_grand_temp2a, Cutoff_row_count_value_temp2, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    Cutoff_row_count_value_vector_temp <- global_year_word_grand_temp2b[global_year_word_grand_temp2b[,names(global_year_word_grand_temp2b)=="Count_word_grand"]==global_year_word_grand_temp2b[,names(global_year_word_grand_temp2b)=="Count_word_grand_cutoff"],]
    Cutoff_row_count_value_vector_temp <- cbind(as.numeric(row.names(Cutoff_row_count_value_vector_temp)),Cutoff_row_count_value_vector_temp)
    names(Cutoff_row_count_value_vector_temp)[1] <- "row_name"
    
    Cutoff_row_count_value_vector_last_temp <- ddply(Cutoff_row_count_value_vector_temp, "yr", function(x) data.frame(last_row_by_year=as.numeric(max(x$row_name))))
    
    global_year_word_grand_temp2c <- merge(global_year_word_grand_temp2b, Cutoff_row_count_value_vector_last_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    global_year_word_grand_temp3[,names(global_year_word_grand_temp3)==Word_cutoff_name_temp] <- global_year_word_grand_temp2c[,names(global_year_word_grand_temp2c)=="last_row_by_year"]
    
    #     if (Cutoff_row_count_value_vector_last_temp == nrow(global_agg_id_unique_temp3))
    #     {
    #       global_agg_id_unique_temp3[1:nrow(global_agg_id_unique_temp3),names(global_agg_id_unique_temp3)==id_DV_name_temp] <- 0
    #       
    #     } else
    #     {
    #       global_agg_id_unique_temp3[1:Cutoff_row_count_value_vector_last_temp,names(global_agg_id_unique_temp3)==id_DV_name_temp] <- 0
    #       global_agg_id_unique_temp3[(Cutoff_row_count_value_vector_last_temp+1):nrow(global_agg_id_unique_temp3),names(global_agg_id_unique_temp3)==id_DV_name_temp] <- 1
    #       
    #     }
    
    global_year_word_grand_temp3[as.numeric(row.names(global_year_word_grand_temp3))<=as.numeric(global_year_word_grand_temp3[,names(global_year_word_grand_temp3)==Word_cutoff_name_temp]),names(global_year_word_grand_temp3)==Word_DV_name_temp] <- 0
    global_year_word_grand_temp3[as.numeric(row.names(global_year_word_grand_temp3))>as.numeric(global_year_word_grand_temp3[,names(global_year_word_grand_temp3)==Word_cutoff_name_temp]),names(global_year_word_grand_temp3)==Word_DV_name_temp] <- 1
  }
  
  rm(global_year_word_grand_temp,global_year_word_grand_temp2)
  rm(global_year_word_grand_temp2a,global_year_word_grand_temp2b,global_year_word_grand_temp2c)
  
  #==============================================================================;
  #Create global year unique word table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_year_word_unique_temp
  global_year_word_unique_temp <- tokens_all_temp2[tokens_all_temp2[,names(tokens_all_temp2)=="Remove"]==0,c("token","yr","ID")]
  
  #Remove all NAs rows left
  global_year_word_unique_temp <- global_year_word_unique_temp[!(rowSums(is.na(global_year_word_unique_temp[,1:ncol(global_year_word_unique_temp)]))==ncol(global_year_word_unique_temp)),]
  
  #Sort global_year_word_unique_temp
  global_year_word_unique_temp  <- global_year_word_unique_temp[order(global_year_word_unique_temp[,names(global_year_word_unique_temp)=="yr"], global_year_word_unique_temp[,names(global_year_word_unique_temp)=="ID"],global_year_word_unique_temp[,names(global_year_word_unique_temp)=="token"]),] 
  
  query_global_year_word_unique_temp2 <- "select distinct   yr, Upper(token) token, Count(token) Count_word_unique
                                          from              global_year_word_unique_temp 
                                          group by          yr, Upper(token)
                                          order by          yr, Count_word_unique desc, token"
  global_year_word_unique_temp2 <- sqldf(query_global_year_word_unique_temp2)
  
  #Create global_word_unique tables
  global_year_word_unique_temp3 <- as.data.frame(matrix(NA, ncol = (ncol(global_year_word_unique_temp2)+3+(nrow(percentiles)*2)), nrow = nrow(global_year_word_unique_temp2)))
  global_year_word_unique_temp3_cols <- append(names(global_year_word_unique_temp2),c("uTotal_word_unique","gTotal_word_unique","Total_Percentage_word_unique"))
  global_year_word_unique_temp3_cols <- append(global_year_word_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_lbl"],"_word_unique",sep=""))
  global_year_word_unique_temp3_cols <- append(global_year_word_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_DV"],"_word_unique",sep=""))
  colnames(global_year_word_unique_temp3) <- global_year_word_unique_temp3_cols
  
  #Populate global_year_word_unique_temp3 with columns from global_year_temp2
  for (r in 1:ncol(global_year_word_unique_temp2))
  {
    #r <- 1
    global_year_word_unique_temp3[1:nrow(global_year_word_unique_temp2),names(global_year_word_unique_temp3)==colnames(global_year_word_unique_temp2)[r]] <- global_year_word_unique_temp2[1:nrow(global_year_word_unique_temp2),r]
  }
  
  #Create uTotal_word_unique, gTotal_word_unique, and Total_Percentage_word_unique
  global_year_word_unique_temp3[,names(global_year_word_unique_temp3)=="uTotal_word_unique"] <- as.numeric(ddply(global_year_word_unique_temp2, "yr", function(x) data.frame(x,nrow=nrow(x)))$nrow)
  global_year_word_unique_temp3[,names(global_year_word_unique_temp3)=="gTotal_word_unique"] <- as.numeric(ddply(global_year_word_unique_temp2, "yr", function(x) data.frame(x,sum=sum(x$Count_word_unique)))$sum)
  global_year_word_unique_temp3[,names(global_year_word_unique_temp3)=="Total_Percentage_word_unique"] <- as.numeric(ddply(global_year_word_unique_temp2, "yr", function(x) data.frame(x,tp=((x$Count_word_unique)/sum(x$Count_word_unique))))$tp)
  
  #Create Word_Cutoff_XXXpct and Word_DV_XXXpct
  for (s in 1:nrow(percentiles))
  {
    #s <- 1
    Word_cutoff_name_temp <- paste(percentiles[s,names(percentiles)=="Column_lbl"],"_word_unique",sep="")
    Word_DV_name_temp <- paste(percentiles[s,names(percentiles)=="Column_DV"],"_word_unique",sep="")
    #Word_cutoff_value_temp1 <- percentiles[s,names(percentiles)=="Significance_Level"]
    Word_cutoff_value_temp1 <- percentiles[s,names(percentiles)=="Confidence_Level"]
    Word_cutoff_value_temp2 <- ddply(global_year_word_unique_temp3, "yr", function(x) data.frame(cutoff=ceil(Word_cutoff_value_temp1*nrow(x))))
    Word_cutoff_value_temp3 <- unique(Word_cutoff_value_temp2[,c("yr","cutoff")], incomparables = FALSE)
    
    global_year_word_unique_temp2a <- merge(global_year_word_unique_temp2, Word_cutoff_value_temp3, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    Cutoff_row_count_value_temp1 <- ddply(global_year_word_unique_temp2a, "yr", function(x) data.frame(x[x$cutoff,]))
    Cutoff_row_count_value_temp2 <- unique(Cutoff_row_count_value_temp1[,c("yr","Count_word_unique")], incomparables = FALSE)        
    names(Cutoff_row_count_value_temp2)[2] <- "Count_word_unique_cutoff"
    
    global_year_word_unique_temp2b <- merge(global_year_word_unique_temp2a, Cutoff_row_count_value_temp2, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    Cutoff_row_count_value_vector_temp <- global_year_word_unique_temp2b[global_year_word_unique_temp2b[,names(global_year_word_unique_temp2b)=="Count_word_unique"]==global_year_word_unique_temp2b[,names(global_year_word_unique_temp2b)=="Count_word_unique_cutoff"],]
    Cutoff_row_count_value_vector_temp <- cbind(as.numeric(row.names(Cutoff_row_count_value_vector_temp)),Cutoff_row_count_value_vector_temp)
    names(Cutoff_row_count_value_vector_temp)[1] <- "row_name"
    
    Cutoff_row_count_value_vector_last_temp <- ddply(Cutoff_row_count_value_vector_temp, "yr", function(x) data.frame(last_row_by_year=as.numeric(max(x$row_name))))
    
    global_year_word_unique_temp2c <- merge(global_year_word_unique_temp2b, Cutoff_row_count_value_vector_last_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    global_year_word_unique_temp3[,names(global_year_word_unique_temp3)==Word_cutoff_name_temp] <- global_year_word_unique_temp2c[,names(global_year_word_unique_temp2c)=="last_row_by_year"]
    
    global_year_word_unique_temp3[as.numeric(row.names(global_year_word_unique_temp3))<=as.numeric(global_year_word_unique_temp3[,names(global_year_word_unique_temp3)==Word_cutoff_name_temp]),names(global_year_word_unique_temp3)==Word_DV_name_temp] <- 0
    global_year_word_unique_temp3[as.numeric(row.names(global_year_word_unique_temp3))>as.numeric(global_year_word_unique_temp3[,names(global_year_word_unique_temp3)==Word_cutoff_name_temp]),names(global_year_word_unique_temp3)==Word_DV_name_temp] <- 1
  }
  
  rm(global_year_word_unique_temp,global_year_word_unique_temp2)
  rm(global_year_word_unique_temp2a,global_year_word_unique_temp2b,global_year_word_unique_temp2c)
  
  #==============================================================================;
  #Create global year unique id table;
  #==============================================================================;
  
  #Copy tokens_all_temp to global_year_id_unique_temp
  global_year_id_unique_temp <- tokens_all_temp2[tokens_all_temp2[,names(tokens_all_temp2)=="Remove"]==0,c("token","yr","ID")]
  
  #Remove all NAs rows left
  global_year_id_unique_temp <- global_year_id_unique_temp[!(rowSums(is.na(global_year_id_unique_temp[,1:ncol(global_year_id_unique_temp)]))==ncol(global_year_id_unique_temp)),]
  
  #Sort global_year_id_unique_temp
  global_year_id_unique_temp  <- global_year_id_unique_temp[order(global_year_id_unique_temp[,names(global_year_id_unique_temp)=="yr"], global_year_id_unique_temp[,names(global_year_id_unique_temp)=="ID"],global_year_id_unique_temp[,names(global_year_id_unique_temp)=="token"]),] 
  
  query_global_year_id_unique_temp2 <- "select distinct  yr, Upper(token) token, Count(token) Count_id_unique
                                       from              global_year_id_unique_temp 
                                       group by          yr, Upper(token)
                                       order by          yr, Count_id_unique desc, token"
  global_year_id_unique_temp2 <- sqldf(query_global_year_id_unique_temp2)
  
  #Create global_id_unique tables
  global_year_id_unique_temp3 <- as.data.frame(matrix(NA, ncol = (ncol(global_year_id_unique_temp2)+2+(nrow(percentiles)*2)), nrow = nrow(global_year_id_unique_temp2)))
  global_year_id_unique_temp3_cols <- append(names(global_year_id_unique_temp2),c("gTotal_id_unique","Total_Percentage_id_unique"))
  global_year_id_unique_temp3_cols <- append(global_year_id_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_lbl"],"_id_unique",sep=""))
  global_year_id_unique_temp3_cols <- append(global_year_id_unique_temp3_cols,paste(percentiles[,names(percentiles)=="Column_DV"],"_id_unique",sep=""))
  colnames(global_year_id_unique_temp3) <- global_year_id_unique_temp3_cols
  
  #Populate global_year_id_unique_temp3 with columns from global_year_temp2
  for (r in 1:ncol(global_year_id_unique_temp2))
  {
    #r <- 1
    global_year_id_unique_temp3[1:nrow(global_year_id_unique_temp2),names(global_year_id_unique_temp3)==colnames(global_year_id_unique_temp2)[r]] <- global_year_id_unique_temp2[1:nrow(global_year_id_unique_temp2),r]
  }
  
  #Get list of all unique words
  unique_ids_year <- ddply(tokens_all_temp1, "yr", function(x) as.data.frame(unique(x[,c("yr","ID")], incomparables = FALSE)))
  unique_ids_year <- ddply(unique_ids_year, "yr", function(x) data.frame(x,nrow=as.numeric(nrow(x))))
  unique_ids_year2 <- subset(unique_ids_year, select = c("yr","nrow"))
  unique_ids_year2 <- unique(unique_ids_year2, incomparables = FALSE)   
  
  global_year_id_unique_temp3a <- subset(global_year_id_unique_temp3, select = c("yr"))
  query_global_year_id_unique_temp3a <- "select            a.yr, b.nrow
                                         from              global_year_id_unique_temp3a a
                                         left join         unique_ids_year2 b
                                         on                a.yr=b.yr"
  global_year_id_unique_temp3b <- sqldf(query_global_year_id_unique_temp3a)
  
  #Create uTotal_id_unique, gTotal_id_unique, and Total_Percentage_id_unique
  global_year_id_unique_temp3[,names(global_year_id_unique_temp3)=="gTotal_id_unique"] <- global_year_id_unique_temp3b[,names(global_year_id_unique_temp3b)=="nrow"]
  global_year_id_unique_temp3[,names(global_year_id_unique_temp3)=="Total_Percentage_id_unique"] <- (global_year_id_unique_temp3[,names(global_year_id_unique_temp3)=="Count_id_unique"]/global_year_id_unique_temp3[,names(global_year_id_unique_temp3)=="gTotal_id_unique"])
  
  #Create id_Cutoff_XXXpct and id_DV_XXXpct
  for (s in 1:nrow(percentiles))
  {
    #s <- 1
    #s <- 2
    id_cutoff_name_temp <- paste(percentiles[s,names(percentiles)=="Column_lbl"],"_id_unique",sep="")
    id_DV_name_temp <- paste(percentiles[s,names(percentiles)=="Column_DV"],"_id_unique",sep="")
    #id_cutoff_value_temp1 <- percentiles[s,names(percentiles)=="Significance_Level"]
    id_cutoff_value_temp1 <- percentiles[s,names(percentiles)=="Confidence_Level"]   
    id_cutoff_value_temp2 <- ddply(global_year_id_unique_temp3, "yr", function(x) data.frame(cutoff=ceil(id_cutoff_value_temp1*x$gTotal_id_unique)))
    id_cutoff_value_temp3 <- unique(id_cutoff_value_temp2[,c("yr","cutoff")], incomparables = FALSE)
    
    global_year_id_unique_temp2a <- merge(global_year_id_unique_temp2, id_cutoff_value_temp3, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    Cutoff_row_count_value_temp1 <- ddply(global_year_id_unique_temp2a, "yr", function(x) data.frame(x[x$cutoff,]))
    Cutoff_row_count_value_temp2 <- unique(Cutoff_row_count_value_temp1[,c("yr","Count_id_unique")], incomparables = FALSE)        
    names(Cutoff_row_count_value_temp2)[2] <- "Count_id_unique_cutoff"
    
    global_year_id_unique_temp2b <- merge(global_year_id_unique_temp2a, Cutoff_row_count_value_temp2, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    Cutoff_row_count_value_vector_temp <- global_year_id_unique_temp2b[global_year_id_unique_temp2b[,names(global_year_id_unique_temp2b)=="Count_id_unique"]==global_year_id_unique_temp2b[,names(global_year_id_unique_temp2b)=="Count_id_unique_cutoff"],]
    Cutoff_row_count_value_vector_temp <- cbind(as.numeric(row.names(Cutoff_row_count_value_vector_temp)),Cutoff_row_count_value_vector_temp)
    names(Cutoff_row_count_value_vector_temp)[1] <- "row_name"
    
    Cutoff_row_count_value_vector_last_temp <- ddply(Cutoff_row_count_value_vector_temp, "yr", function(x) data.frame(last_row_by_year=as.numeric(max(x$row_name))))
    
    global_year_id_unique_temp2c <- merge(global_year_id_unique_temp2b, Cutoff_row_count_value_vector_last_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    global_year_id_unique_temp3[,names(global_year_id_unique_temp3)==id_cutoff_name_temp] <- global_year_id_unique_temp2c[,names(global_year_id_unique_temp2c)=="last_row_by_year"]
    
    global_year_id_unique_temp3[as.numeric(row.names(global_year_id_unique_temp3))<=as.numeric(global_year_id_unique_temp3[,names(global_year_id_unique_temp3)==id_cutoff_name_temp]),names(global_year_id_unique_temp3)==id_DV_name_temp] <- 0
    global_year_id_unique_temp3[as.numeric(row.names(global_year_id_unique_temp3))>as.numeric(global_year_id_unique_temp3[,names(global_year_id_unique_temp3)==id_cutoff_name_temp]),names(global_year_id_unique_temp3)==id_DV_name_temp] <- 1
  }
  
  rm(global_year_id_unique_temp,global_year_id_unique_temp2)
  rm(global_year_id_unique_temp2a,global_year_id_unique_temp2b,global_year_id_unique_temp2c)
  rm(global_year_id_unique_temp3a,global_year_id_unique_temp3b)
  
  #==============================================================================;
  #Merge the ID DVs with the Word DVs (year);
  #==============================================================================;
  
  global_year_comb1 <- merge(global_year_word_grand_temp3, global_year_word_unique_temp3, by.x = c("yr","token") , by.y = c("yr","token"), all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
  global_year_comb2 <- merge(global_year_comb1, global_year_id_unique_temp3, by.x = c("yr","token") , by.y = c("yr","token") , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
  
  global_year_count_vector <- grep(pattern = '*Count_*', colnames(global_year_comb2), ignore.case = FALSE, perl = FALSE, value = TRUE)
  global_year_dv_vector <- grep(pattern = '*_DV_*', colnames(global_year_comb2), ignore.case = FALSE, perl = FALSE, value = TRUE)
  global_year_col_vector <- append(global_year_count_vector,global_year_dv_vector)
  global_year_col_vector <- append("token",global_year_col_vector)
  global_year_col_vector <- append("yr",global_year_col_vector)
  global_year_comb  <- global_year_comb2[,global_year_col_vector]
  
  #global_year_comb_diff <- global_year_comb[global_year_comb[,11]!=global_year_comb[,14],]
  
  rm(global_year_comb1,global_year_comb2,global_year_word_grand_temp3,global_year_word_unique_temp3,global_year_id_unique_temp3)
  capture.output(gc(),file='NUL')
  
  #==============================================================================;
  #Create global year tables based on the percentiles;
  #==============================================================================;
  
  global_year_dv_vector_used <- as.data.frame(cbind(global_year_dv_vector,rep(NA,length(global_year_dv_vector))),stringsAsFactors=FALSE)
  colnames(global_year_dv_vector_used)[2] <- "Column_not_all_0"
  
  #Create id_Cutoff_XXXpct and id_DV_XXXpct
  for (q in 1:nrow(percentiles))
  {
    #q <- 1
    
    #==============================================================================;
    #Word Grand;
    #==============================================================================;
    
    temp_word_grand_column <- paste(percentiles[q,names(percentiles)=="Column_DV"],"_word_grand",sep="")
    
    if (sum(global_year_comb[,names(global_year_comb)==temp_word_grand_column])==0)
    {
      global_year_dv_vector_used[global_year_dv_vector_used[,1]==temp_word_grand_column,2] <- 0
      
      cat("Column", temp_word_grand_column,"is empty -- no global dictionary for this column", "\n")
      
      
    } else
    {
      global_year_dv_vector_used[global_year_dv_vector_used[,1]==temp_word_grand_column,2] <- 1
      
      query_global_year_word_grand_percentiles <- paste("select distinct  yr,Upper(token) token, Count_word_grand
                                                         from             global_year_comb 
                                                         where ",         temp_word_grand_column,"=1
                                                         group by         yr,Upper(token)
                                                         order by         yr,Count_word_grand desc, token", sep = "") 
     
      temp_word_grand_df <- create_stats_yr_function(sqldf(query_global_year_word_grand_percentiles),"Count_word_grand","yr")
      temp_word_grand_df_name <- paste("global_year_word_grand", percentiles[q,names(percentiles)=="Confidence_lbl"], sep = "")
      
      write.csv(temp_word_grand_df, file = paste(output_directory,temp_word_grand_df_name,".csv",sep=""))
      assign(temp_word_grand_df_name, temp_word_grand_df, envir = .GlobalEnv)
      
      rm(temp_word_grand_df,temp_word_grand_df_name)
      
    }
    
    rm(temp_word_grand_column)
    capture.output(gc(),file='NUL')
    
    #==============================================================================;
    #Word Unique;
    #==============================================================================;
    
    temp_word_unique_column <- paste(percentiles[q,names(percentiles)=="Column_DV"],"_word_unique",sep="")
    
    if (sum(global_year_comb[,names(global_year_comb)==temp_word_unique_column])==0)
    {
      global_year_dv_vector_used[global_year_dv_vector_used[,1]==temp_word_unique_column,2] <- 0
      
      cat("Column", temp_word_unique_column,"is empty -- no global dictionary for this column", "\n")
      
      
    } else
    {
      global_year_dv_vector_used[global_year_dv_vector_used[,1]==temp_word_unique_column,2] <- 1
      
      
      query_global_year_word_unique_percentiles <- paste("select distinct  yr,Upper(token) token, Count_word_unique
                                                          from             global_year_comb 
                                                          where ",         temp_word_unique_column,"=1
                                                          group by         yr,Upper(token)
                                                          order by         yr,Count_word_unique desc, token", sep = "") 
  
      temp_word_unique_df <- create_stats_yr_function(sqldf(query_global_year_word_unique_percentiles),"Count_word_unique","yr")
      temp_word_unique_df_name <- paste("global_year_word_unique", percentiles[q,names(percentiles)=="Confidence_lbl"], sep = "")
      
      write.csv(temp_word_unique_df, file = paste(output_directory,temp_word_unique_df_name,".csv",sep=""))
      assign(temp_word_unique_df_name, temp_word_unique_df, envir = .GlobalEnv)
      
      rm(temp_word_unique_df,temp_word_unique_df_name)
      
    }
    
    rm(temp_word_unique_column)
    capture.output(gc(),file='NUL')
      
      
    #==============================================================================;
    #ID Unique;
    #==============================================================================;
    
    temp_id_unique_column <- paste(percentiles[q,names(percentiles)=="Column_DV"],"_id_unique",sep="")
    
    if (sum(global_year_comb[,names(global_year_comb)==temp_id_unique_column])==0)
    {
      global_year_dv_vector_used[global_year_dv_vector_used[,1]==temp_id_unique_column,2] <- 0
      
      cat("Column", temp_id_unique_column,"is empty -- no global dictionary for this column", "\n")
      
      
    } else
    {
      global_year_dv_vector_used[global_year_dv_vector_used[,1]==temp_id_unique_column,2] <- 1
      
      
      query_global_year_id_unique_percentiles <- paste("select distinct  yr,Upper(token) token, Count_id_unique
                                                        from             global_year_comb 
                                                        where ",         temp_id_unique_column,"=1
                                                        group by         yr,Upper(token)
                                                        order by         yr,Count_id_unique desc, token", sep = "") 
      
      temp_id_unique_df <- create_stats_yr_function(sqldf(query_global_year_id_unique_percentiles),"Count_id_unique","yr")
      temp_id_unique_df_name <- paste("global_year_id_unique", percentiles[q,names(percentiles)=="Confidence_lbl"], sep = "")
      
      write.csv(temp_id_unique_df, file = paste(output_directory,temp_id_unique_df_name,".csv",sep=""))
      assign(temp_id_unique_df_name, temp_id_unique_df, envir = .GlobalEnv)
      
      rm(temp_id_unique_df,temp_id_unique_df_name)
      
    }
    
    rm(temp_id_unique_column)
    capture.output(gc(),file='NUL')
      
  }
    
  rm(Cutoff_row_count_value_vector_temp,Cutoff_row_count_value_vector_last_temp)
  rm(global_year_word_grand_temp3_cols,global_year_word_unique_temp3_cols,global_year_id_unique_temp3_cols)
  rm(Word_cutoff_name_temp,Word_DV_name_temp,Word_cutoff_value_temp1,Word_cutoff_value_temp2,Word_cutoff_value_temp3)
  rm(id_cutoff_name_temp,id_DV_name_temp,id_cutoff_value_temp1,id_cutoff_value_temp2,id_cutoff_value_temp3)
  rm(global_year_count_vector,global_year_dv_vector,global_year_col_vector)

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #==============================================================================;
  #INDIVIDUAL DICTIONARY (AGGREGATE);
  cat("SECTION: INDIVIDUAL DICTIONARY (AGGREGATE)", "\n")
  #==============================================================================;
  
  for (t in 1:nrow(percentiles))
  {
    #t <- 1
    
    #==============================================================================;
    #Word Grand;
    #==============================================================================;
    
    temp_word_grand_column <- paste(percentiles[t,names(percentiles)=="Column_DV"],"_word_grand",sep="")
    query_multiple_agg_word_grand_temp <- paste("select distinct  Upper(token) token
                                                 from             global_agg_comb 
                                                 where ",         temp_word_grand_column,"=1
                                                 order by         token", sep = "") 
    multiple_agg_word_grand_temp <- sqldf(query_multiple_agg_word_grand_temp)
    multiple_agg_word_grand_token_count_temp <- as.numeric(nrow(multiple_agg_word_grand_temp))
    multiple_agg_word_grand_id_count_temp <- as.numeric(nrow(unique_ids))
    multiple_agg_word_grand_token_temp <- cbind(multiple_agg_word_grand_temp,multiple_agg_word_grand_id_count_temp)
    colnames(multiple_agg_word_grand_token_temp) <- c("token","multiple")
    multiple_agg_word_grand_id_temp <- cbind(unique_ids,multiple_agg_word_grand_token_count_temp)
    colnames(multiple_agg_word_grand_id_temp) <- c("ID","multiple")
    multiple_agg_word_grand_expand_temp <- as.data.frame(matrix(NA, ncol = 3, nrow = (multiple_agg_word_grand_token_count_temp*multiple_agg_word_grand_id_count_temp)))
    multiple_agg_word_grand_expand_temp[,1] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_agg_word_grand_id_temp[,1],multiple_agg_word_grand_id_temp[,2], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_agg_word_grand_expand_temp[,1] <- as.character(multiple_agg_word_grand_expand_temp[,1])
    multiple_agg_word_grand_expand_temp[,2] <- rep(multiple_agg_word_grand_token_temp[,1],multiple_agg_word_grand_id_count_temp)
    multiple_agg_word_grand_expand_temp[,3] <- rep(NA,nrow(multiple_agg_word_grand_expand_temp))
    colnames(multiple_agg_word_grand_expand_temp) <- c("ID","token","token_dv")
    query_multiple_agg_word_grand_expand_intersect_temp <- "select            a.ID, a.token
                                                            from              multiple_agg_word_grand_expand_temp a
                                                            intersect
                                                            select            b.ID, b.token
                                                            from              tokens_all_temp2 b"
    multiple_agg_word_grand_expand_intersect_temp <- sqldf(query_multiple_agg_word_grand_expand_intersect_temp)
    colnames(multiple_agg_word_grand_expand_intersect_temp) <- c("ID","token")
    multiple_agg_word_grand_expand_intersect_temp$in_both <- 1
    query_multiple_agg_word_grand_expand_temp2 <- "select            in_both
                                                   from              multiple_agg_word_grand_expand_temp a
                                                   left join         multiple_agg_word_grand_expand_intersect_temp b
                                                   on                a.ID=b.ID
                                                   and               a.token=b.token"
    multiple_agg_word_grand_expand_temp[,3] <- as.numeric(sqldf(query_multiple_agg_word_grand_expand_temp2)[,1])
    multiple_agg_word_grand_expand_temp[is.na(multiple_agg_word_grand_expand_temp[,3]),3] <- 0 
    #assign(paste("multiple_agg_word_grand_expand", percentiles[t,names(percentiles)=="Confidence_lbl"], sep = ""), multiple_agg_word_grand_expand_temp, envir = .GlobalEnv)
    
    write.csv(multiple_agg_word_grand_expand_temp, file = paste(output_directory,"multiple_agg_word_grand_expand", percentiles[t,names(percentiles)=="Confidence_lbl"],".csv",sep=""))
    rm(temp_word_grand_column,multiple_agg_word_grand_temp,multiple_agg_word_grand_token_count_temp)
    rm(multiple_agg_word_grand_id_count_temp,multiple_agg_word_grand_token_temp,multiple_agg_word_grand_id_temp)
    rm(multiple_agg_word_grand_expand_intersect_temp,multiple_agg_word_grand_expand_temp)
    capture.output(gc(),file='NUL')
    
    #==============================================================================;
    #Word Unique;
    #==============================================================================;
    
    temp_word_unique_column <- paste(percentiles[t,names(percentiles)=="Column_DV"],"_word_unique",sep="")
    query_multiple_agg_word_unique_temp <- paste("select distinct  Upper(token) token
                                                  from             global_agg_comb 
                                                  where ",         temp_word_unique_column,"=1
                                                  order by         token", sep = "") 
    multiple_agg_word_unique_temp <- sqldf(query_multiple_agg_word_unique_temp)
    multiple_agg_word_unique_token_count_temp <- as.numeric(nrow(multiple_agg_word_unique_temp))
    multiple_agg_word_unique_id_count_temp <- as.numeric(nrow(unique_ids))
    multiple_agg_word_unique_token_temp <- cbind(multiple_agg_word_unique_temp,multiple_agg_word_unique_id_count_temp)
    colnames(multiple_agg_word_unique_token_temp) <- c("token","multiple")
    multiple_agg_word_unique_id_temp <- cbind(unique_ids,multiple_agg_word_unique_token_count_temp)
    colnames(multiple_agg_word_unique_id_temp) <- c("ID","multiple")
    multiple_agg_word_unique_expand_temp <- as.data.frame(matrix(NA, ncol = 3, nrow = (multiple_agg_word_unique_token_count_temp*multiple_agg_word_unique_id_count_temp)))
    multiple_agg_word_unique_expand_temp[,1] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_agg_word_unique_id_temp[,1],multiple_agg_word_unique_id_temp[,2], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_agg_word_unique_expand_temp[,1] <- as.character(multiple_agg_word_unique_expand_temp[,1])
    multiple_agg_word_unique_expand_temp[,2] <- rep(multiple_agg_word_unique_token_temp[,1],multiple_agg_word_unique_id_count_temp)
    multiple_agg_word_unique_expand_temp[,3] <- rep(NA,nrow(multiple_agg_word_unique_expand_temp))
    colnames(multiple_agg_word_unique_expand_temp) <- c("ID","token","token_dv")
    query_multiple_agg_word_unique_expand_intersect_temp <- "select            a.ID, a.token
                                                             from              multiple_agg_word_unique_expand_temp a
                                                             intersect
                                                             select            b.ID, b.token
                                                             from              tokens_all_temp2 b"
    multiple_agg_word_unique_expand_intersect_temp <- sqldf(query_multiple_agg_word_unique_expand_intersect_temp)
    colnames(multiple_agg_word_unique_expand_intersect_temp) <- c("ID","token")
    multiple_agg_word_unique_expand_intersect_temp$in_both <- 1
    query_multiple_agg_word_unique_expand_temp2 <- "select            in_both
                                                    from              multiple_agg_word_unique_expand_temp a
                                                    left join         multiple_agg_word_unique_expand_intersect_temp b
                                                    on                a.ID=b.ID
                                                    and               a.token=b.token"
    multiple_agg_word_unique_expand_temp[,3] <- as.numeric(sqldf(query_multiple_agg_word_unique_expand_temp2)[,1])
    multiple_agg_word_unique_expand_temp[is.na(multiple_agg_word_unique_expand_temp[,3]),3] <- 0 
    #assign(paste("multiple_agg_word_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"], sep = ""), multiple_agg_word_unique_expand_temp, envir = .GlobalEnv)
    
    write.csv(multiple_agg_word_unique_expand_temp, file = paste(output_directory,"multiple_agg_word_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"],".csv",sep=""))
    rm(temp_word_unique_column,multiple_agg_word_unique_temp,multiple_agg_word_unique_token_count_temp)
    rm(multiple_agg_word_unique_id_count_temp,multiple_agg_word_unique_token_temp,multiple_agg_word_unique_id_temp)
    rm(multiple_agg_word_unique_expand_intersect_temp,multiple_agg_word_unique_expand_temp)
    capture.output(gc(),file='NUL')
    
    #==============================================================================;
    #ID Unique;
    #==============================================================================;
    
    temp_id_unique_column <- paste(percentiles[t,names(percentiles)=="Column_DV"],"_id_unique",sep="")
    query_multiple_agg_id_unique_temp <- paste("select distinct  Upper(token) token
                                                from             global_agg_comb 
                                                where ",         temp_id_unique_column,"=1
                                                order by         token", sep = "") 
    multiple_agg_id_unique_temp <- sqldf(query_multiple_agg_id_unique_temp)
    multiple_agg_id_unique_token_count_temp <- as.numeric(nrow(multiple_agg_id_unique_temp))
    multiple_agg_id_unique_id_count_temp <- as.numeric(nrow(unique_ids))
    multiple_agg_id_unique_token_temp <- cbind(multiple_agg_id_unique_temp,multiple_agg_id_unique_id_count_temp)
    colnames(multiple_agg_id_unique_token_temp) <- c("token","multiple")
    multiple_agg_id_unique_id_temp <- cbind(unique_ids,multiple_agg_id_unique_token_count_temp)
    colnames(multiple_agg_id_unique_id_temp) <- c("ID","multiple")
    multiple_agg_id_unique_expand_temp <- as.data.frame(matrix(NA, ncol = 3, nrow = (multiple_agg_id_unique_token_count_temp*multiple_agg_id_unique_id_count_temp)))
    multiple_agg_id_unique_expand_temp[,1] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_agg_id_unique_id_temp[,1],multiple_agg_id_unique_id_temp[,2], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_agg_id_unique_expand_temp[,1] <- as.character(multiple_agg_id_unique_expand_temp[,1])
    multiple_agg_id_unique_expand_temp[,2] <- rep(multiple_agg_id_unique_token_temp[,1],multiple_agg_id_unique_id_count_temp)
    multiple_agg_id_unique_expand_temp[,3] <- rep(NA,nrow(multiple_agg_id_unique_expand_temp))
    colnames(multiple_agg_id_unique_expand_temp) <- c("ID","token","token_dv")
    query_multiple_agg_id_unique_expand_intersect_temp <- "select            a.ID, a.token
                                                           from              multiple_agg_id_unique_expand_temp a
                                                           intersect
                                                           select            b.ID, b.token
                                                           from              tokens_all_temp2 b"
    multiple_agg_id_unique_expand_intersect_temp <- sqldf(query_multiple_agg_id_unique_expand_intersect_temp)
    colnames(multiple_agg_id_unique_expand_intersect_temp) <- c("ID","token")
    multiple_agg_id_unique_expand_intersect_temp$in_both <- 1
    query_multiple_agg_id_unique_expand_temp2 <- "select            in_both
                                                  from              multiple_agg_id_unique_expand_temp a
                                                  left join         multiple_agg_id_unique_expand_intersect_temp b
                                                  on                a.ID=b.ID
                                                  and               a.token=b.token"
    multiple_agg_id_unique_expand_temp[,3] <- as.numeric(sqldf(query_multiple_agg_id_unique_expand_temp2)[,1])
    multiple_agg_id_unique_expand_temp[is.na(multiple_agg_id_unique_expand_temp[,3]),3] <- 0 
    #assign(paste("multiple_agg_id_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"], sep = ""), multiple_agg_id_unique_expand_temp, envir = .GlobalEnv)
    
    write.csv(multiple_agg_id_unique_expand_temp, file = paste(output_directory,"multiple_agg_id_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"],".csv",sep=""))
    rm(temp_id_unique_column,multiple_agg_id_unique_temp,multiple_agg_id_unique_token_count_temp)
    rm(multiple_agg_id_unique_id_count_temp,multiple_agg_id_unique_token_temp,multiple_agg_id_unique_id_temp)
    rm(multiple_agg_id_unique_expand_intersect_temp,multiple_agg_id_unique_expand_temp)
    capture.output(gc(),file='NUL')
    
    progress_function(outer_loop_count=t, outer_loop_start_val=1, outer_loop_end_val=nrow(percentiles), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  
  
  
  #==============================================================================;
  #INDIVIDUAL DICTIONARY (YEARLY);
  cat("SECTION: INDIVIDUAL DICTIONARY (YEARLY)", "\n")
  #==============================================================================;
  
  for (t in 1:nrow(percentiles))
  {
    #t <- 1
    
    #==============================================================================;
    #Word Grand;
    #==============================================================================;
    
    temp_word_grand_column <- paste(percentiles[t,names(percentiles)=="Column_DV"],"_word_grand",sep="")
    query_multiple_year_word_grand_temp <- paste("select distinct  yr,Upper(token) token
                                                  from              global_year_comb 
                                                  where ",          temp_word_grand_column,"=1
                                                  order by          yr, token", sep = "") 
    multiple_year_word_grand_temp <- sqldf(query_multiple_year_word_grand_temp)
    multiple_year_word_grand_token_count_temp <- ddply(multiple_year_word_grand_temp, "yr", function(x) data.frame(nrow=nrow(x)))
    multiple_year_word_grand_id_count_temp <- unique_ids_year2
    multiple_year_word_grand_token_temp <- merge(multiple_year_word_grand_temp, multiple_year_word_grand_id_count_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    colnames(multiple_year_word_grand_token_temp) <- c("yr","token","multiple")
    multiple_year_word_grand_id_temp <- merge(unique_ids_year, multiple_year_word_grand_token_count_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    multiple_year_word_grand_id_temp <- subset(multiple_year_word_grand_id_temp, select = c(1,2,4))
    colnames(multiple_year_word_grand_id_temp) <- c("yr","ID","multiple")
    multiple_year_word_grand_expand_temp <- as.data.frame(matrix(NA, ncol = 4, nrow = (as.numeric(crossprod(multiple_year_word_grand_token_count_temp[,2], multiple_year_word_grand_id_count_temp[,2])))))
    multiple_year_word_grand_expand_temp[,1] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_year_word_grand_id_temp[,1],multiple_year_word_grand_id_temp[,3], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_year_word_grand_expand_temp[,2] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_year_word_grand_id_temp[,2],multiple_year_word_grand_id_temp[,3], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_year_word_grand_expand_temp[,2] <- as.character(multiple_year_word_grand_expand_temp[,2])
    multiple_year_word_grand_expand_temp[,3] <- subset(ddply(multiple_year_word_grand_token_temp, "yr", function(x) data.frame(rep(x[,2],x[1,3]))), select = c(2))
    multiple_year_word_grand_expand_temp[,3] <- as.character(multiple_year_word_grand_expand_temp[,3])
    multiple_year_word_grand_expand_temp[,4] <- rep(NA,nrow(multiple_year_word_grand_expand_temp))
    colnames(multiple_year_word_grand_expand_temp) <- c("yr","ID","token","token_dv")
    query_multiple_year_word_grand_expand_intersect_temp <- "select            a.yr, a.ID, a.token
                                                             from              multiple_year_word_grand_expand_temp a
                                                             intersect
                                                             select            b.yr, b.ID, b.token
                                                             from              tokens_all_temp2 b"
    multiple_year_word_grand_expand_intersect_temp <- sqldf(query_multiple_year_word_grand_expand_intersect_temp)
    colnames(multiple_year_word_grand_expand_intersect_temp) <- c("yr","ID","token")
    multiple_year_word_grand_expand_intersect_temp$in_both <- 1
    query_multiple_year_word_grand_expand_temp2 <- "select            in_both
                                                    from              multiple_year_word_grand_expand_temp a
                                                    left join         multiple_year_word_grand_expand_intersect_temp b
                                                    on                a.yr=b.yr
                                                    and               a.ID=b.ID
                                                    and               a.token=b.token"
    multiple_year_word_grand_expand_temp[,4] <- as.numeric(sqldf(query_multiple_year_word_grand_expand_temp2)[,1])
    multiple_year_word_grand_expand_temp[is.na(multiple_year_word_grand_expand_temp[,4]),4] <- 0 
    #assign(paste("multiple_year_word_grand_expand", percentiles[t,names(percentiles)=="Confidence_lbl"], sep = ""), multiple_year_word_grand_expand_temp, envir = .GlobalEnv)
    
    write.csv(multiple_year_word_grand_expand_temp, file = paste(output_directory,"multiple_year_word_grand_expand", percentiles[t,names(percentiles)=="Confidence_lbl"],".csv",sep=""))
    rm(temp_word_grand_column,multiple_year_word_grand_temp,multiple_year_word_grand_token_count_temp)
    rm(multiple_year_word_grand_id_count_temp,multiple_year_word_grand_token_temp,multiple_year_word_grand_id_temp)
    rm(multiple_year_word_grand_expand_intersect_temp,multiple_year_word_grand_expand_temp)
    capture.output(gc(),file='NUL')
    
    #==============================================================================;
    #Word Unique;
    #==============================================================================;
    
    temp_word_unique_column <- paste(percentiles[t,names(percentiles)=="Column_DV"],"_word_unique",sep="")
    query_multiple_year_word_unique_temp <- paste("select distinct  yr,Upper(token) token
                                                   from              global_year_comb 
                                                   where ",          temp_word_unique_column,"=1
                                                   order by          yr, token", sep = "") 
    multiple_year_word_unique_temp <- sqldf(query_multiple_year_word_unique_temp)
    multiple_year_word_unique_token_count_temp <- ddply(multiple_year_word_unique_temp, "yr", function(x) data.frame(nrow=nrow(x)))
    multiple_year_word_unique_id_count_temp <- unique_ids_year2
    multiple_year_word_unique_token_temp <- merge(multiple_year_word_unique_temp, multiple_year_word_unique_id_count_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    colnames(multiple_year_word_unique_token_temp) <- c("yr","token","multiple")
    multiple_year_word_unique_id_temp <- merge(unique_ids_year, multiple_year_word_unique_token_count_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    multiple_year_word_unique_id_temp <- subset(multiple_year_word_unique_id_temp, select = c(1,2,4))
    colnames(multiple_year_word_unique_id_temp) <- c("yr","ID","multiple")
    multiple_year_word_unique_expand_temp <- as.data.frame(matrix(NA, ncol = 4, nrow = (as.numeric(crossprod(multiple_year_word_unique_token_count_temp[,2], multiple_year_word_unique_id_count_temp[,2])))))
    multiple_year_word_unique_expand_temp[,1] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_year_word_unique_id_temp[,1],multiple_year_word_unique_id_temp[,3], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_year_word_unique_expand_temp[,2] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_year_word_unique_id_temp[,2],multiple_year_word_unique_id_temp[,3], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_year_word_unique_expand_temp[,2] <- as.character(multiple_year_word_unique_expand_temp[,2])
    multiple_year_word_unique_expand_temp[,3] <- subset(ddply(multiple_year_word_unique_token_temp, "yr", function(x) data.frame(rep(x[,2],x[1,3]))), select = c(2))
    multiple_year_word_unique_expand_temp[,3] <- as.character(multiple_year_word_unique_expand_temp[,3])
    multiple_year_word_unique_expand_temp[,4] <- rep(NA,nrow(multiple_year_word_unique_expand_temp))
    colnames(multiple_year_word_unique_expand_temp) <- c("yr","ID","token","token_dv")
    query_multiple_year_word_unique_expand_intersect_temp <- "select            a.yr, a.ID, a.token
                                                              from              multiple_year_word_unique_expand_temp a
                                                              intersect
                                                              select            b.yr, b.ID, b.token
                                                              from              tokens_all_temp2 b"
    multiple_year_word_unique_expand_intersect_temp <- sqldf(query_multiple_year_word_unique_expand_intersect_temp)
    colnames(multiple_year_word_unique_expand_intersect_temp) <- c("yr","ID","token")
    multiple_year_word_unique_expand_intersect_temp$in_both <- 1
    query_multiple_year_word_unique_expand_temp2 <- "select            in_both
                                                     from              multiple_year_word_unique_expand_temp a
                                                     left join         multiple_year_word_unique_expand_intersect_temp b
                                                     on                a.yr=b.yr
                                                     and               a.ID=b.ID
                                                     and               a.token=b.token"
    multiple_year_word_unique_expand_temp[,4] <- as.numeric(sqldf(query_multiple_year_word_unique_expand_temp2)[,1])
    multiple_year_word_unique_expand_temp[is.na(multiple_year_word_unique_expand_temp[,4]),4] <- 0 
    #assign(paste("multiple_year_word_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"], sep = ""), multiple_year_word_unique_expand_temp, envir = .GlobalEnv)
    
    write.csv(multiple_year_word_unique_expand_temp, file = paste(output_directory,"multiple_year_word_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"],".csv",sep=""))
    rm(temp_word_unique_column,multiple_year_word_unique_temp,multiple_year_word_unique_token_count_temp)
    rm(multiple_year_word_unique_id_count_temp,multiple_year_word_unique_token_temp,multiple_year_word_unique_id_temp)
    rm(multiple_year_word_unique_expand_intersect_temp,multiple_year_word_unique_expand_temp)
    capture.output(gc(),file='NUL')
    
    #==============================================================================;
    #ID Unique;
    #==============================================================================;
    
    temp_id_unique_column <- paste(percentiles[t,names(percentiles)=="Column_DV"],"_id_unique",sep="")
    query_multiple_year_id_unique_temp <- paste("select distinct  yr,Upper(token) token
                                                 from              global_year_comb 
                                                 where ",          temp_id_unique_column,"=1
                                                 order by          yr, token", sep = "") 
    multiple_year_id_unique_temp <- sqldf(query_multiple_year_id_unique_temp)
    multiple_year_id_unique_token_count_temp <- ddply(multiple_year_id_unique_temp, "yr", function(x) data.frame(nrow=nrow(x)))
    multiple_year_id_unique_id_count_temp <- unique_ids_year2
    multiple_year_id_unique_token_temp <- merge(multiple_year_id_unique_temp, multiple_year_id_unique_id_count_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    colnames(multiple_year_id_unique_token_temp) <- c("yr","token","multiple")
    multiple_year_id_unique_id_temp <- merge(unique_ids_year, multiple_year_id_unique_token_count_temp, by.x = "yr" , by.y = "yr" , all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
    multiple_year_id_unique_id_temp <- subset(multiple_year_id_unique_id_temp, select = c(1,2,4))
    colnames(multiple_year_id_unique_id_temp) <- c("yr","ID","multiple")
    multiple_year_id_unique_expand_temp <- as.data.frame(matrix(NA, ncol = 4, nrow = (as.numeric(crossprod(multiple_year_id_unique_token_count_temp[,2], multiple_year_id_unique_id_count_temp[,2])))))
    multiple_year_id_unique_expand_temp[,1] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_year_id_unique_id_temp[,1],multiple_year_id_unique_id_temp[,3], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_year_id_unique_expand_temp[,2] <- as.data.frame(unlist(mapply(data_expand_function,id_col=multiple_year_id_unique_id_temp[,2],multiple_year_id_unique_id_temp[,3], SIMPLIFY = FALSE,USE.NAMES = FALSE)))
    multiple_year_id_unique_expand_temp[,2] <- as.character(multiple_year_id_unique_expand_temp[,2])
    multiple_year_id_unique_expand_temp[,3] <- subset(ddply(multiple_year_id_unique_token_temp, "yr", function(x) data.frame(rep(x[,2],x[1,3]))), select = c(2))
    multiple_year_id_unique_expand_temp[,3] <- as.character(multiple_year_id_unique_expand_temp[,3])
    multiple_year_id_unique_expand_temp[,4] <- rep(NA,nrow(multiple_year_id_unique_expand_temp))
    colnames(multiple_year_id_unique_expand_temp) <- c("yr","ID","token","token_dv")
    query_multiple_year_id_unique_expand_intersect_temp <- "select            a.yr, a.ID, a.token
                                                            from              multiple_year_id_unique_expand_temp a
                                                            intersect
                                                            select            b.yr, b.ID, b.token
                                                            from              tokens_all_temp2 b"
    multiple_year_id_unique_expand_intersect_temp <- sqldf(query_multiple_year_id_unique_expand_intersect_temp)
    colnames(multiple_year_id_unique_expand_intersect_temp) <- c("yr","ID","token")
    multiple_year_id_unique_expand_intersect_temp$in_both <- 1
    query_multiple_year_id_unique_expand_temp2 <- "select            in_both
                                                   from              multiple_year_id_unique_expand_temp a
                                                   left join         multiple_year_id_unique_expand_intersect_temp b
                                                   on                a.yr=b.yr
                                                   and               a.ID=b.ID
                                                   and               a.token=b.token"
    multiple_year_id_unique_expand_temp[,4] <- as.numeric(sqldf(query_multiple_year_id_unique_expand_temp2)[,1])
    multiple_year_id_unique_expand_temp[is.na(multiple_year_id_unique_expand_temp[,4]),4] <- 0 
    #assign(paste("multiple_year_id_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"], sep = ""), multiple_year_id_unique_expand_temp, envir = .GlobalEnv)
    
    write.csv(multiple_year_id_unique_expand_temp, file = paste(output_directory,"multiple_year_id_unique_expand", percentiles[t,names(percentiles)=="Confidence_lbl"],".csv",sep=""))
    rm(temp_id_unique_column,multiple_year_id_unique_temp,multiple_year_id_unique_token_count_temp)
    rm(multiple_year_id_unique_id_count_temp,multiple_year_id_unique_token_temp,multiple_year_id_unique_id_temp)
    rm(multiple_year_id_unique_expand_intersect_temp,multiple_year_id_unique_expand_temp)
    capture.output(gc(),file='NUL')
    
    progress_function(outer_loop_count=t, outer_loop_start_val=1, outer_loop_end_val=nrow(percentiles), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
    
  }
  
  
  
  
  
  #   
  #   #Overwrite token file with temp file
  #   cat("Token table: ",readbl_vars[m,3], "\n")
  #   if (m==1)
  #   {
  #     tokens_all_ios_f[1:nrow(tokens_all_temp2),] <- tokens_all_temp2[1:nrow(tokens_all_temp2),]
  #     write.csv(tokens_all_ios_f, file = paste(output_directory,"tokens_all_ios_f.csv",sep=""))
  #     
  #     global_agg_ios_f <- global_agg_temp
  #     global_year_ios_f <- global_year_temp
  #     
  #   } else if (m==2)
  #   {
  #     tokens_all_pr_f[1:nrow(tokens_all_temp2),] <- tokens_all_temp2[1:nrow(tokens_all_temp2),]
  #     write.csv(tokens_all_ios_f, file = paste(output_directory,"tokens_all_pr_f.csv",sep=""))
  #     
  #     global_agg_pr_f <- global_agg_temp
  #     global_year_pr_f <- global_year_temp
  #     
  #   } else
  #   {
  #     cat("ERROR WHEN REASSIGNING COLUMNS", "\n")
  #     
  #   }
  
  #==============================================================================;
  #CREATE PROGRESS OUTPUTS;
  #==============================================================================;
  
  #Initiate garbage collection
  capture.output(gc(),file='NUL')
  
  progress_function(outer_loop_count=m, outer_loop_start_val=1, outer_loop_end_val=nrow(readbl_vars), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
  #END OF M FOR LOOP
  
}



#==============================================================================;
#DONE;
cat("DONE", "\n")
#==============================================================================;

proc.time() - ptm