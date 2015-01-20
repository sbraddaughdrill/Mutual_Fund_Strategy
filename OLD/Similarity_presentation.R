library(arules)
library(clv)
library(data.table)
library(lsa)
library(plyr)
library(rJava)
library(Snowball)
library(sqldf)  

text_1999 <- c("APPRECIATION","CAPITAL","DESIRE","HIGH","INCOME","RETURN","SEEK")
id_text1_1999 <- data.frame(yr=1999,id="001",token=text_1999,DV=c(0,0,1,1,0,1,0),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text2_1999 <- data.frame(yr=1999,id="002",token=text_1999,DV=c(1,1,0,0,1,0,1),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text_all_1999 <- rbind(id_text1_1999,id_text2_1999)

text_2000 <- c("APPRECIATION","CAPITAL","DESIRE","GROW","HIGH","INCOME","INVESTMENT","MAKE","RETURN","SEEK")
id_text1_2000 <- data.frame(yr=2000,id="001",token=text_2000,DV=c(0,0,1,0,1,0,0,0,1,0),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text2_2000 <- data.frame(yr=2000,id="002",token=text_2000,DV=c(1,1,0,0,0,1,0,0,0,1),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text3_2000 <- data.frame(yr=2000,id="003",token=text_2000,DV=c(0,0,0,1,0,0,1,1,0,1),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text_all_2000 <- rbind(id_text1_2000,id_text2_2000,id_text3_2000)

text_2001 <- c("ACHIEVE","CAPITAL","DESIRE","GROW","HIGH","INVESTMENT","MAKE","RETURN","SEEK")
id_text2_2001 <- data.frame(yr=2001,id="002",token=text_2001,DV=c(0,0,1,0,1,0,0,1,0),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text3_2001 <- data.frame(yr=2001,id="003",token=text_2001,DV=c(0,0,0,1,0,1,1,0,1),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text4_2001 <- data.frame(yr=2001,id="004",token=text_2001,DV=c(1,1,0,1,0,0,0,0,1),cosine_normalized=NA,count_normalized=NA,stringsAsFactors=FALSE)
id_text_all_2001 <- rbind(id_text2_2001,id_text3_2001,id_text4_2001)

id_text_all <- rbind(id_text_all_1999,id_text_all_2000,id_text_all_2001)

id_text_all[,5:6] <- subset(data.table(id_text_all)[, list(cosine_normalized=DV/sqrt(crossprod(DV)), count_normalized=DV/crossprod(DV)), by = "yr,id"],select=c("cosine_normalized","count_normalized"))
id_text_all[is.na(id_text_all[,5]),5] <- NA
id_text_all[is.na(id_text_all[,6]),6] <- NA

id_text_all_length_check <- subset(data.table(id_text_all)[, list(cosine_normalized_length=as.numeric(crossprod(cosine_normalized)), count_normalized_length=sum(count_normalized)), by = "yr,id"],select=c("yr","id","cosine_normalized_length","count_normalized_length"))
id_text_all_length_check <- as.data.frame(id_text_all_length_check,stringsAsFactors=FALSE)
id_text_all_length_check[is.na(id_text_all_length_check[,3]),3] <- NA
id_text_all_length_check[is.na(id_text_all_length_check[,4]),4] <- NA

create_cosine_similarity <- function(global_dictionary_data,year,norm_type){
  
  #global_dictionary_data <- id_text_all
  #year <- 1999
  #norm_type <- "cosine_normalized"
  #norm_type <- "count_normalized"
  
  global_dict_temp <- global_dictionary_data[global_dictionary_data$yr==year,]
  
  global_dict_temp_ids <- unique(global_dict_temp[,names(global_dict_temp)=="id"], incomparables = FALSE)
  
  global_dict_temp_final <-  as.data.frame(matrix(NA, ncol = (length(global_dict_temp_ids)+2), nrow = length(global_dict_temp_ids)))
  colnames(global_dict_temp_final) <- append(c("yr","id"),global_dict_temp_ids)
  
  global_dict_temp_final[,names(global_dict_temp_final)=="yr"] <- year
  global_dict_temp_final[,names(global_dict_temp_final)=="id"] <- global_dict_temp_ids
  global_dict_temp_final <- global_dict_temp_final[order(global_dict_temp_final$yr,global_dict_temp_final$id),]
  
  populate_cosine_similarity <- function(temp_id,temp_dict,normalization_type){
    
    #j <- 1
    #temp_id <- global_dict_temp_ids[j]
    #temp_dict <- global_dict_temp
    #normalization_type <- norm_type
    
    global_dict_temp2 <- temp_dict[temp_dict$id==temp_id,]
    
    #normalized_col_num <- as.numeric(match(normalization_type,names(global_dictionary_data)))
    global_dict_temp3 <- global_dict_temp2[,names(global_dict_temp2)==normalization_type]
    
    temp_col <- ddply(temp_dict, "id", function(x) data.frame(x, cosine_similarity=crossprod(x$cosine_normalized,global_dict_temp3)) )
    temp_col[is.na(temp_col[,names(temp_col)=="cosine_similarity"]),names(temp_col)=="cosine_similarity"] <- NA
    temp_col2 <- as.data.frame(unique(temp_col[c("yr","id","cosine_similarity")], incomparables = FALSE),stringsAsFactors=FALSE)
    temp_col2 <- temp_col2[order(temp_col2$yr,temp_col2$id),]
    
    return(temp_col2[,names(temp_col2)=="cosine_similarity"])
  }
  
  #global_dict_temp_final[,names(global_dict_temp_final)==temp_id] 
  global_dict_temp_final[,3:ncol(global_dict_temp_final)]  <- sapply(global_dict_temp_ids,populate_cosine_similarity,
                                                                     temp_dict=global_dict_temp, normalization_type=norm_type,
                                                                     simplify = FALSE, USE.NAMES = FALSE)
  
  
  
  
  return(global_dict_temp_final)
}
cs_1999 <- create_cosine_similarity(id_text_all, 1999, "cosine_normalized")
cs_2000 <- create_cosine_similarity(id_text_all, 2000, "cosine_normalized")
cs_2001 <- create_cosine_similarity(id_text_all, 2001, "cosine_normalized")