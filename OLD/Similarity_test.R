#install.packages("arules")
#install.packages("clv")
#install.packages("data.table")
#install.packages("lsa")
#install.packages("rJava")
#install.packages("Snowball")

library(arules)
library(clv)
library(data.table)
library(lsa)
library(plyr)
library(rJava)
library(Snowball)
library(sqldf)          #Package for performing SQL selects on R Data Frames


#CREATE TEST DATA

id_text1 <- data.frame(id="000001",token=c("THE","RED","DOG","HAS","A","YELLOW","COLLAR"),stringsAsFactors=FALSE)
id_text2 <- data.frame(id="000002",token=c("SHE","HAS","A","YELLOW","HOUSE"),stringsAsFactors=FALSE)
id_text3 <- data.frame(id="000003",token=c("I","JUMPED","THE","RED","DOG"),stringsAsFactors=FALSE)
id_text4 <- data.frame(id="000004",token=c("I","WALKED","ON","THE","YELLOW","FENCE"),stringsAsFactors=FALSE)
id_text5 <- data.frame(id="000005",token=c("THE","RED","HOUSE","HAS","A","BLACK","FENCE"),stringsAsFactors=FALSE)
id_text6 <- data.frame(id="000006",token=c("A","GREEN","CAT","HOPPED","ON","TO","A","PURPLE","GATE"),stringsAsFactors=FALSE)
id_text_all <- rbind(id_text1,id_text2,id_text3,id_text4,id_text5,id_text6)

text_yr1 <- data.frame(token=c("THE","BLACK","DOG","JUMPED","OVER","THE","RED","FENCE"),yr=1999,stringsAsFactors=FALSE)
text_yr2 <- data.frame(token=c("I","WALKED","THE","BLACK","DOG"),yr=2000,stringsAsFactors=FALSE) 
text_yr3 <- data.frame(token=c("SHE","PAINTED","THE","RED","FENCE"),yr=2001,stringsAsFactors=FALSE) 
text_yr4 <- data.frame(token=c("THE","YELLOW","HOUSE","HAS","THE","BLACK","DOG","AND","RED","FENCE"),yr=2002,stringsAsFactors=FALSE)
text_yr_all <- rbind(text_yr1,text_yr2,text_yr3,text_yr4)

id_yr1<- data.frame(id=c("000001","000001","000001","000001"),yr=c(1999,2000,2001,2002),stringsAsFactors=FALSE)
id_yr2<- data.frame(id=c("000002","000002","000002"),yr=c(2000,2001,2002),stringsAsFactors=FALSE)
id_yr3<- data.frame(id=c("000003","000003","000003"),yr=c(1999,2000,2001),stringsAsFactors=FALSE)
id_yr4<- data.frame(id=c("000004","000004","000004","000004"),yr=c(1999,2000,2001,2002),stringsAsFactors=FALSE)
id_yr5<- data.frame(id=c("000005"),yr=2002,stringsAsFactors=FALSE)
id_yr6<- data.frame(id=c("000006"),yr=1999,stringsAsFactors=FALSE)
id_yr_all <- rbind(id_yr1,id_yr2,id_yr3,id_yr4,id_yr5,id_yr6)
id_yr_all <- id_yr_all[order(id_yr_all$yr,id_yr_all$id),]


rm(id_text1,id_text2,id_text3,id_text4,id_text5,id_text6)
rm(text_yr1,text_yr2,text_yr3,text_yr4)
rm(id_yr1,id_yr2,id_yr3,id_yr4,id_yr5,id_yr6)

#CREATE COMBINED TABLES

global_dict <- merge(id_yr_all, text_yr_all, by.x = c("yr"), by.y = c("yr"), all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)
global_dict <- cbind(global_dict,rep(NA,nrow(global_dict)),rep(NA,nrow(global_dict)),rep(NA,nrow(global_dict)))
colnames(global_dict)[4:6] <- c("DV","cosine_normalized","count_normalized")
tokens_all <- merge(id_yr_all, id_text_all, by.x = c("id"), by.y = c("id"), all.x = TRUE, all.y = FALSE, sort = TRUE, suffixes = c(".x",".y"),incomparables = NA)

#POPULATE DVs

query_intersect_temp <- "select            a.yr, a.ID, a.token
                         from              global_dict a
                         intersect
                         select            b.yr, b.ID, b.token
                         from              tokens_all b"
intersect_temp <- sqldf(query_intersect_temp)
colnames(intersect_temp) <- c("yr","ID","token")
intersect_temp$in_both <- 1
query_global_dict2 <- "select            in_both
                       from              global_dict a
                       left join         intersect_temp b
                       on                a.yr=b.yr
                       and               a.ID=b.ID
                       and               a.token=b.token"
global_dict[,4] <- as.numeric(sqldf(query_global_dict2)[,1])
global_dict[is.na(global_dict[,4]),4] <- 0 
global_dict <- global_dict[order(global_dict$yr,global_dict$id),]


#NORMALIZE VECTOR DVs

global_dict[,5:6] <- subset(data.table(global_dict)[, list(cosine_normalized=DV/sqrt(crossprod(DV)), count_normalized=DV/crossprod(DV)), by = "yr,id"],select=c("cosine_normalized","count_normalized"))
global_dict[is.na(global_dict[,5]),5] <- NA
global_dict[is.na(global_dict[,6]),6] <- NA

global_dict_normal_length_check <- subset(data.table(global_dict)[, list(cosine_normalized_length=as.numeric(crossprod(cosine_normalized)), count_normalized_length=sum(count_normalized)), by = "yr,id"],select=c("yr","id","cosine_normalized_length","count_normalized_length"))
global_dict_normal_length_check <- as.data.frame(global_dict_normal_length_check,stringsAsFactors=FALSE)
global_dict_normal_length_check[is.na(global_dict_normal_length_check[,3]),3] <- NA
global_dict_normal_length_check[is.na(global_dict_normal_length_check[,4]),4] <- NA

#COSINE SIMILARITY

create_cosine_similarity <- function(global_dictionary_data,year,norm_type){
  
  #global_dictionary_data <- global_dict
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

cs_1999 <- create_cosine_similarity(global_dict, 1999, "cosine_normalized")
cs_2000 <- create_cosine_similarity(global_dict, 2000, "cosine_normalized")
cs_2001 <- create_cosine_similarity(global_dict, 2001, "cosine_normalized")
cs_2002 <- create_cosine_similarity(global_dict, 2002, "cosine_normalized")

cs_1999_mat <- as.matrix(cs_1999[,3:ncol(cs_1999)])

diag(cs_1999_mat) <- NA

cs_1999_avg <- as.data.frame(matrix(NA, ncol=3, nrow=nrow(cs_1999)), stringsAsFactors=FALSE)
colnames(cs_1999_avg) <- c("yr","ID","Avg_Similarity")

cs_1999_avg[,1:2] <- cs_1999[,1:2]

cs_1999_avg[,3] <- as.data.frame(rowMeans(cs_1999_mat, na.rm = TRUE),stringsAsFactors=FALSE)
