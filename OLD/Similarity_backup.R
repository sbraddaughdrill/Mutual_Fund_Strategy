
#   populate_cosine_similarity <- function(temp_id,temp_dict,normalization_type,id_count){
#     
#     #j <- 1
#     #temp_id <- global_dict_temp_ids[j]
#     #temp_dict <- global_dict_temp
#     #normalization_type <- norm_type
#     #id_count <- length(global_dict_temp_ids)
#     
#     #global_dict_temp2 <- temp_dict[temp_dict$ID==temp_id,]
#     #normalized_col_num <- as.numeric(match(normalization_type,names(global_dictionary_data)))
#     
#     temp_id_scores <- temp_dict[temp_dict[,names(temp_dict)=="ID"]==temp_id,names(temp_dict)==normalization_type]
#     
#     #temp_col <- ddply(temp_dict, "ID", function(x) data.frame(x, cosine_similarity=crossprod(x$cosine_normalized,temp_id_scores)) )
#     temp_col <- data.frame(temp_dict, similarity=crossprod(temp_dict[,names(temp_dict)==normalization_type],rep(temp_id_scores,id_count)))
#     temp_col[is.na(temp_col[,names(temp_col)=="similarity"]),names(temp_col)=="similarity"] <- NA
#     
#     temp_col2 <- as.data.frame(unique(temp_col[c("yr","ID","similarity")], incomparables = FALSE),stringsAsFactors=FALSE)
#     temp_col2 <- temp_col2[order(temp_col2$yr,temp_col2$ID),]
#     
#     rm(temp_dict,temp_id_scores,temp_col)
#     capture.output(gc(),file='NUL')
#     
#     return(temp_col2[,names(temp_col2)=="similarity"])
#     
#   }

create_cosine_similarity <- function(year,global_dictionary_data,norm_type){
  
  #year <- 1999
  #year <- 9999
  #global_dictionary_data <- temp_dictionary
  #norm_type <- "cosine_normalized"
  #norm_type <- "count_normalized"
  
  global_dict_temp <- global_dictionary_data[global_dictionary_data$yr==year,]
  #global_dict_temp <-global_dict_temp[1:4140,]
  global_dict_temp_ids <- unique(global_dict_temp[,names(global_dict_temp)=="ID"], incomparables = FALSE)
  
  #temp_id_count <- length(global_dict_temp_ids)
  
  #     global_dict_temp_final <-  as.data.frame(matrix(NA, ncol = (temp_id_count+2), nrow = temp_id_count),stringsAsFactors=FALSE)
  #     colnames(global_dict_temp_final) <- append(c("yr","ID"),global_dict_temp_ids)
  #     
  #     global_dict_temp_final[,names(global_dict_temp_final)=="yr"] <- year
  #     global_dict_temp_final[,names(global_dict_temp_final)=="ID"] <- global_dict_temp_ids
  #     global_dict_temp_final <- global_dict_temp_final[order(global_dict_temp_final$yr,global_dict_temp_final$ID),]
  
  #global_dict_temp_final[,names(global_dict_temp_final)==temp_id] 
  #global_dict_temp_final[,3:ncol(global_dict_temp_final)]  <- pbsapply(global_dict_temp_ids,populate_cosine_similarity,
  #                                                                     temp_dict=global_dict_temp, 
  #                                                                     normalization_type=norm_type,
  #                                                                     id_count=temp_id_count,
  #                                                                     simplify = FALSE, USE.NAMES = FALSE)
  
  #global_dict_temp_ids_trim <- global_dict_temp_ids[1:10]
  
  
  #     
  #     a <- pbsapply(global_dict_temp_ids_trim,populate_cosine_similarity,
  #                                                                          temp_dict=global_dict_temp, 
  #                                                                          normalization_type=norm_type,
  #                                                                          id_count=temp_id_count,
  #                                                                          simplify = FALSE, USE.NAMES = FALSE)
  #     global_dict_temp_final[,3:12] <- as.data.frame(a,stringsAsFactors=FALSE)
  #     
  
  #gd_norm_col_num <- as.numeric(match(norm_type,names(global_dict_temp)))
  
  #token_count_per_id <- length(global_dict_temp[,(names)==])/temp_id_count
  
  global_dict_temp_col_mat <- matrix(global_dict_temp[,names(global_dict_temp)==norm_type],nrow=length(global_dict_temp_ids), byrow=TRUE)
  global_dict_temp_col_mat_df <- as.data.frame(t(global_dict_temp_col_mat),stringsAsFactors=FALSE)
  colnames(global_dict_temp_col_mat_df) <- global_dict_temp_ids
  
  global_dict_temp_col_mat_df2 <- as.data.frame(pbapply(global_dict_temp_col_mat_df, 2, function(x){crossprod(x,as.matrix(global_dict_temp_col_mat_df))}))
  
  #global_dict_temp_col_mat_df2 <- cbind(year,as.character(tokens),f_t_df)
  
  global_dict_temp_final <- as.data.frame(matrix(NA, ncol=(length(global_dict_temp_ids)+2), nrow=length(global_dict_temp_ids)), stringsAsFactors=FALSE)
  colnames(global_dict_temp_final) <- append(c("yr","ID"),global_dict_temp_ids)
  
  global_dict_temp_final[,names(global_dict_temp_final)=="yr"] <- year
  global_dict_temp_final[,names(global_dict_temp_final)=="ID"] <- global_dict_temp_ids
  global_dict_temp_final <- global_dict_temp_final[order(global_dict_temp_final$yr,global_dict_temp_final$ID),]
  global_dict_temp_final[,3:ncol(global_dict_temp_final)] <- as.data.frame(global_dict_temp_col_mat_df2,stringsAsFactors=FALSE)
  
  rm(global_dict_temp,global_dict_temp_ids)
  capture.output(gc(),file='NUL')
  
  return(global_dict_temp_final)
  
}

calculate_cosine_similarity <- function(temp_percentile,temp_measures,temp_freq,norm_type,output_path){
  
  #a <- 3
  #temp_percentile <- percentiles
  #temp_measures <- "word_grand"
  #temp_freq <- "agg"
  #norm_type <- "cosine_normalized"
  #output_path <- output_directory
  
  #rm(temp_percentile,temp_measures,temp_freq,output_path)
  
  for (a in 1:nrow(percentiles))
  {
    #a <- 1
    
    #temp_measure_name <- temp_measures[a]
    
    temp_percentile <- percentiles[a,names(percentiles)=="Confidence_lbl"]
    
    temp_dictionary_name <- paste("global_",temp_freq,"_comb_", temp_measures, "_expand", temp_percentile, sep = "")
    
    temp_dictionary <- read.csv(file=paste(output_path, temp_dictionary_name, ".csv",sep=""),header=TRUE,na.strings="NA",stringsAsFactors=FALSE)[-c(1)]
    
    temp_dictionary <- cbind(temp_dictionary, rep(NA,nrow(temp_dictionary)), rep(NA,nrow(temp_dictionary)))
    colnames(temp_dictionary)[(ncol(temp_dictionary)-1)] <- c("cosine_normalized")
    colnames(temp_dictionary)[ncol(temp_dictionary)] <- c("count_normalized")
    
    yr_col_num <- as.numeric(match("yr",names(temp_dictionary)))
    ID_col_num <- as.numeric(match("ID",names(temp_dictionary)))
    token_col_num <- as.numeric(match("token",names(temp_dictionary)))
    token_dv_col_num <- as.numeric(match("token_dv",names(temp_dictionary)))
    cosine_normalized_col_num <- as.numeric(match("cosine_normalized",names(temp_dictionary)))
    count_normalized_col_num <- as.numeric(match("count_normalized",names(temp_dictionary)))
    
    temp_dictionary[,ID_col_num] <- paste("", formatC(temp_dictionary[,ID_col_num], width=6, format="d", flag="0"), sep = "")
    
    temp_dictionary[,yr_col_num] <- as.numeric(temp_dictionary[,yr_col_num])
    temp_dictionary[,ID_col_num] <- as.character(temp_dictionary[,ID_col_num])
    temp_dictionary[,token_col_num] <- as.character(temp_dictionary[,token_col_num])
    temp_dictionary[,token_dv_col_num] <- as.numeric(temp_dictionary[,token_dv_col_num])
    temp_dictionary[,cosine_normalized_col_num] <- as.numeric(temp_dictionary[,cosine_normalized_col_num])
    temp_dictionary[,count_normalized_col_num] <- as.numeric(temp_dictionary[,count_normalized_col_num])
    
    temp_dictionary[,cosine_normalized_col_num] <- subset(data.table(temp_dictionary)[, list(cosine_normalized=token_dv/sqrt(crossprod(token_dv))), by = "yr,ID"], select=c("cosine_normalized"))
    temp_dictionary[is.na(temp_dictionary[,cosine_normalized_col_num]),cosine_normalized_col_num] <- NA
    
    temp_dictionary[,count_normalized_col_num] <- subset(data.table(temp_dictionary)[, list(count_normalized=token_dv/crossprod(token_dv)), by = "yr,ID"], select=c("count_normalized"))
    temp_dictionary[is.na(temp_dictionary[,count_normalized_col_num]),count_normalized_col_num] <- NA
    
    temp_dictionary_normal_length_check <- subset(data.table(temp_dictionary)[, list(cosine_normalized_length=as.numeric(crossprod(cosine_normalized)), count_normalized_length=sum(count_normalized)), by = "yr,ID"],select=c("yr","ID","cosine_normalized_length","count_normalized_length"))
    temp_dictionary_normal_length_check <- as.data.frame(temp_dictionary_normal_length_check,stringsAsFactors=FALSE)
    temp_dictionary_normal_length_check[is.na(temp_dictionary_normal_length_check[,3]),3] <- NA
    temp_dictionary_normal_length_check[is.na(temp_dictionary_normal_length_check[,4]),4] <- NA
    
    temp_length_check_name <- paste("text_similarity_",temp_freq,"_",temp_measures, "_", temp_percentile, "_length_check", sep = "")
    write.csv(temp_dictionary_normal_length_check, file = paste(output_path,temp_length_check_name,".csv",sep=""))
    
    rm(temp_length_check_name,temp_dictionary_normal_length_check)
    capture.output(gc(),file='NUL')
    
    years <- unique(temp_dictionary[,c("yr")], incomparables = FALSE)    
    
    for (c in 1:length(years))
    {
      #c <- 1
      
      temp_similarity_name <- paste("text_similarity_", temp_freq, "_", temp_measures, "_", temp_percentile, "_", as.character(years[c]), sep = "")
      
      #temp_similarity <- create_cosine_similarity(years[c],temp_dictionary,norm_type)
      write.csv(create_cosine_similarity(years[c],temp_dictionary,norm_type), file = paste(output_path,temp_similarity_name,".csv",sep=""))
      #assign(temp_similarity_name, temp_similarity, envir = .GlobalEnv)
      
      #rm(temp_similarity_name,temp_similarity)
      
      rm(temp_similarity_name)
      capture.output(gc(),file='NUL')
      
      progress_function(outer_loop_count=a, outer_loop_start_val=1, outer_loop_end_val=nrow(percentiles), inner_loop_count=c, inner_loop_start_val=1, inner_loop_end_val=length(years))
      
    }
    
    temp_dictionary_name <- paste("text_similarity_", temp_measures, "_", temp_percentile, "_dictionary", sep = "")
    #write.csv(temp_dictionary_name, file = paste(output_path,temp_dictionary,".csv",sep=""))
    
    rm(temp_percentile,temp_dictionary_name,temp_dictionary,years)
    capture.output(gc(),file='NUL')
    
  }
  
  rm(temp_measures,temp_freq)
  capture.output(gc(),file='NUL')
  
}


#AGGREGATE

#calculate_cosine_similarity(percentiles[3,names(percentiles_data)=="Confidence_lbl"],measures,"agg")

#invisible(sapply(percentiles[,names(percentiles)=="Confidence_lbl"],calculate_cosine_similarity, 
#                 temp_measures=measures, temp_freq="agg", simplify=FALSE, USE.NAMES=FALSE))

for (b in 1:length(measures))
{
  #b <- 1
  
  calculate_cosine_similarity(percentiles,measures[b],temp_freq="agg",norm_type="cosine_normalized",output_path=output_directory)
  
  progress_function(outer_loop_count=b, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
}


#YEARLY


for (b in 1:length(measures))
{
  #b <- 1
  
  calculate_cosine_similarity(percentiles,measures[b],temp_freq="year",norm_type="cosine_normalized",output_path=output_directory)
  
  progress_function(outer_loop_count=b, outer_loop_start_val=1, outer_loop_end_val=length(measures), inner_loop_count=1, inner_loop_start_val=1, inner_loop_end_val=1)
  
}
