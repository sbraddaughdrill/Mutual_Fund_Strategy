
###############################################################################
cat("INITIAL SETUP: OPTIONS", "\n")
###############################################################################

rm(list=ls(all=TRUE))                              #Clear workspace

Sys.setenv(R_HISTSIZE=500)                         #Limit History so that it never contains more than 50 lines

repo <- c("http://cran.us.r-project.org")

options(repos=structure(repo))
options(install.packages.check.source=FALSE)
options(StringsAsFactors=FALSE)                    #String as factors is False -- used for read.csv
            
options(max.print=500)                             #Default maxprint option

#Set location (HOME=1,WORK=2)
#Location <- 1
Location <- 2

if (Location==1)
{
  #HOME
  
  setwd("C:/Research_temp/")                                                                                                  #Set working directory
  input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/Data/",winslash="\\", mustWork=NA)      #Create data input directory
  output_directory <- normalizePath("C:/Research_temp/",winslash="\\", mustWork=NA)                                       #Create data output directory
  function_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/R/",winslash="\\", mustWork=NA)                     #Create function directory
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=NA)                                          #Create treetag directory
  
} else if (Location==2)
{
  #WORK
  
  setwd("C:/Research_temp/")                                                                                                  #Set working directory
  input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/Data/",winslash="\\", mustWork=NA)  #Create data input directory
  output_directory <- normalizePath("C:/Research_temp/",winslash="\\", mustWork=NA)                                       #Create data output directory
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/R/",winslash="\\", mustWork=NA)                 #Create function directory
  treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=NA)                                          #Create treetag directory
  
} else
{
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
cat("INITIAL SETUP: FUNCTIONS", "\n")
###############################################################################
source(file=paste(function_directory,"functions_db.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_statistics.R",sep=""),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep=""),echo=FALSE)


###############################################################################
cat("INITIAL SETUP: LIBRARIES", "\n")
###############################################################################

update.packages(ask=FALSE, checkBuilt=TRUE)

external_packages <- c("data.table","foreign","gdata","gtools","Hmisc","pbapply","plyr","R.oo","RSQLite","sqldf")

invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)


###############################################################################
# cat("STEP 1: IMPORT TEXT DATA", "\n")
###############################################################################

text_data_name <- paste(input_directory,"Target_good_final.csv",sep="")

text_data <- read.csv(file=text_data_name, header=TRUE,na.strings="NA",stringsAsFactors=FALSE)

#Remove multiple spaces (run a couple times)
for (a in 1:5)
{
  #a <- 1
  text_data[,"investment_objective_f"] <- gsub(pattern=" {2,}", replacement=" ", x=text_data[,"investment_objective_f"])
  text_data[,"investment_strategy_f"]  <- gsub(pattern=" {2,}", replacement=" ", x=text_data[,"investment_strategy_f"])
  text_data[,"principal_risks_f"]  <- gsub(pattern=" {2,}", replacement=" ", x=text_data[,"principal_risks_f"])
  
}

#Identify NAs
for (i in 1:ncol(text_data))
{
  text_data[,i] <- unknownToNA(text_data[,i], unknown=c("",".","NA_character_","NA_Real_","NA",NA),force=TRUE)
  text_data[,i] <- ifelse(is.na(text_data[,i]),NA, text_data[,i])
} 


text_data <- text_data[rowSums(is.na(text_data[,1:ncol(text_data)]))<ncol(text_data),]
text_data_trim <- text_data[!(is.na(text_data[c("investment_objective_f")])) | 
                            !(is.na(text_data[c("investment_strategy_f")])) | 
                            !(is.na(text_data[c("principal_risks_f")])),]

text_data_trim <- text_data[!(is.na(text_data[c("investment_objective_f")])) & 
                            !(is.na(text_data[c("investment_strategy_f")])) & 
                            !(is.na(text_data[c("principal_risks_f")])),]


row.names(text_data_trim)  <- seq(nrow(text_data_trim))

text_data_num_to_pad_cols <- c("crsp_fundno")
for (i in 1:length(text_data_num_to_pad_cols))
{
  text_data_trim[,names(text_data_trim)==text_data_num_to_pad_cols[i]] <- paste("", formatC(as.integer(text_data_trim[,text_data_num_to_pad_cols[i]]), 
                                                                                  width=6, format="d", flag="0"), sep="")
} 


###############################################################################
# cat("STEP 2: IMPORT MFLINK1", "\n")
###############################################################################

in_db <- paste(output_directory,"MFLinks_Formatted.s3db",sep="")

in_tables <- ListTables(in_db)
in_fields <- ListFields(in_db)

mflink1_fields <- in_fields[in_fields[,1]=="mflink1",]
mflink1 <- runsql("SELECT * FROM mflink1",in_db)


###############################################################################
# cat("STEP 3: MERGE TEXT AND MLFINK1 DATA", "\n")
###############################################################################

merged_data <- merge(text_data_trim, mflink1, by.x="crsp_fundno", by.y="crsp_fundno", all.x=TRUE, all.y=FALSE, 
                     sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

merged_data <- merged_data[c("wficn","crsp_fundno","yr","investment_objective_f","investment_strategy_f","principal_risks_f")]

merged_data_trim <- merged_data[!(is.na(merged_data[,"wficn"])),]

merged_data_trim <- merged_data_trim[order(merged_data_trim[,"wficn"],merged_data_trim[,"crsp_fundno"], merged_data_trim[,"yr"]),] 

###############################################################################
# cat("STEP 4: DROP crsp_fundno AND FIND UNIQUE OBSERVATIONS BY WFICN", "\n")
###############################################################################

##NOTE:  WFICN (Wharton Financial Institution Center Number)

aggregate_data0 <- subset(merged_data_trim,select=-c(crsp_fundno))

aggregate_data <- unique(aggregate_data0, incomparables=FALSE)
aggregate_data <- aggregate_data[order(aggregate_data[,"wficn"], aggregate_data[,"yr"]),] 

aggregate_data_no_dup <- ddply(aggregate_data, c("wficn","yr"), function(z) head(z,1))
#aggregate_data_no_dup <- as.data.frame(data.table(aggregate_data)[, .SD[c(1,.N)], by="wficn,yr"], stringsAsFactors=FALSE) 


q_group_count1 <- ""
q_group_count1 <- paste(q_group_count1,"select distinct  *, Count(wficn) Count            ", sep=" ")
q_group_count1 <- paste(q_group_count1,"from             aggregate_data_no_dup                  ", sep=" ")
q_group_count1 <- paste(q_group_count1,"group by         wficn, yr                        ", sep=" ")
q_group_count1 <- gsub(" {2,}", " ", q_group_count1)
group_count <- sqldf(q_group_count1)

group_count_not_1 <- group_count[group_count[,"Count"]!=1,]

group_count_not_1_trim <- subset(group_count_not_1,select=-c(Count))

bad_wficn <- unique(group_count_not_1[,"wficn"], incomparables=FALSE)

bad_data1 <- merged_data_trim[merged_data_trim[,"wficn"] %in% bad_wficn,]

bad_data2 <- merge(aggregate_data, group_count_not_1_trim[,c("wficn","yr")], by.x=c("wficn","yr"), by.y=c("wficn","yr"), 
                   all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)

bad_data3 <- merge(merged_data_trim, group_count_not_1_trim[,c("wficn","yr")], by.x=c("wficn","yr"), by.y=c("wficn","yr"), 
                   all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"),incomparables=NA)
bad_data3 <- bad_data3[order(bad_data3[,"wficn"], bad_data3[,"yr"], bad_data3[,"crsp_fundno"]),] 


###############################################################################
# cat("STEP 5: OUTPUT DATA", "\n")
###############################################################################

write.csv(aggregate_data_no_dup, file=paste(input_directory,"Target_aggregate",".csv",sep=""), row.names=FALSE)
