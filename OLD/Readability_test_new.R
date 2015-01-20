#install.packages("pbapply")
#install.packages("snow")
#install.packages("snowfall")

library(koRpus)  
library(pbapply)
library(gtools)


input_directory <- normalizePath("C:/Users/Brad/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)  
#input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/3rd-Year_Paper/Data/",winslash = "\\", mustWork = NA)
output_directory <- normalizePath("C:/Research_temp/",winslash = "\\", mustWork = NA)  #Home

data <- read.csv(file=paste(input_directory,"Target_good_final.csv",sep=""),header=TRUE,na.strings="NA",nrows=1000,stringsAsFactors=FALSE)[-c(1)]

sample_data_all <- data[296:299,]
 

tagged_text_desc_stats <- c("lines","chars.no.space","letters.only","digits","normalized.space")
hyph_text_en_desc_stats <- c("num.syll")
readability_stats <- c("ARI","Coleman.Liau","Flesch.Kincaid","SMOG")
readability_desc_stats <- c("sentences","words","all.chars","punct","conjunctions","prepositions","pronouns","foreign","FOG.hard.words",
                               "TTR","sntc.per.word","avg.sentc.length","avg.word.length","avg.syll.word","sntc.per100","syll.per100","lett.per100")
token_stats <- c("token","desc")

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
     
    #return(c(tagged_text_desc_measures_temp,hyph_text_en_desc_measures_temp,readability_measures_temp,readability_desc_measures_temp))
    
    #rm(temp_text,tagged_text,tagged_text_desc_measures_temp,hyph_text_en,hyph_text_en_desc_measures_temp,readbl_text,readability_measures_temp,readability_desc_measures_temp)
    #capture.output(gc(),file='NUL')
    
  } else
  {
    readstats_df <- as.data.frame(do.call(cbind, as.list(rep(NA, length(c(tagged_text_desc_measures,hyph_text_en_desc_measures,readability_measures,readability_desc_measures))))),stringsAsFactors=FALSE)
    colnames(readstats_df) <- paste("V",1:ncol(readstats_df),sep="")
    
    tokens_df <- as.data.frame(do.call(cbind, as.list(rep(NA, length(token_measures)))),stringsAsFactors=FALSE)
    colnames(tokens_df) <- paste("V",1:ncol(tokens_df),sep="")
    
    return(list(readstats = readstats_df,tokens = tokens_df))
    
    #returnList <- list(readstats = rep(NA, length(c(tagged_text_desc_measures,hyph_text_en_desc_measures,readability_measures,readability_desc_measures))),
    #                   tokens = )

    
    #return (rep(NA, length(c(tagged_text_desc_measures,hyph_text_en_desc_measures,readability_measures,readability_desc_measures))))
    
  }
  
}


save_stats <- function(results,type){
  
  #results <- sample_results
  #type <- "readstats"
  
  
  
  sample_stats <- lapply(results, "[[",type)
  sample_stats <- lapply(seq_along(sample_stats), function(x) data.frame(ID=x,sample_stats[x],stringsAsFactors=FALSE))
  sample_stats_df <- as.data.frame(do.call(rbind, sample_stats),stringsAsFactors=FALSE)
  sample_stats_df[,1] <- paste("", formatC(sample_stats_df[,1], width=6, format="d", flag="0"), sep = "")
  colnames(sample_stats_df) <- c("ID",tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats)
  
  return(sample_stats_df)
  
}
xxx <- sapply(sample_results,save_stats,type="readstats",simplify = FALSE, USE.NAMES = FALSE)

sample_results <- pbsapply(sample_data_all[,3],compute_readability_stats,
                           tagged_text_desc_measures=tagged_text_desc_stats,
                           hyph_text_en_desc_measures=hyph_text_en_desc_stats,
                           readability_measures=readability_stats,
                           readability_desc_measures=readability_desc_stats,
                           token_measures=token_stats,
                           simplify = FALSE, USE.NAMES = FALSE)


sample_read_stats <- lapply(sample_results, "[[","readstats")
sample_read_stats <- lapply(seq_along(sample_read_stats), function(x) data.frame(ID=x,sample_read_stats[x],stringsAsFactors=FALSE))
sample_read_stats_df <- as.data.frame(do.call(rbind, sample_read_stats),stringsAsFactors=FALSE)
sample_read_stats_df[,1] <- paste("", formatC(sample_read_stats_df[,1], width=6, format="d", flag="0"), sep = "")
colnames(sample_read_stats_df) <- c("ID",tagged_text_desc_stats,hyph_text_en_desc_stats,readability_stats,readability_desc_stats)

sample_tokens <- lapply(sample_results, "[[","tokens") 
sample_tokens <- lapply(seq_along(sample_tokens), function(x) data.frame(ID=x,sample_tokens[x],stringsAsFactors=FALSE))
sample_tokens_df <- as.data.frame(do.call(rbind, sample_tokens),stringsAsFactors=FALSE)
sample_tokens_df[,1] <- paste("", formatC(sample_tokens_df[,1], width=6, format="d", flag="0"), sep = "")
colnames(sample_tokens_df) <- c("ID",token_stats)

