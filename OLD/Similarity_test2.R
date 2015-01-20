a <- c(1,0,0)
b <- c(0,1,1)
c <- c(1,1,1)
y <- c(1,0,1)
z <- c(0,0,0)

d <- c(a,b,c,y,z)

tokens_per_id <- 3

ids <- c("000001","000002","000003","000004","000005")
tokens <- as.character(c("word1","word2","word3"))
yr <- 1999
f <- matrix(d,nrow=(length(d)/tokens_per_id), byrow=TRUE)
f_t_df <- as.data.frame(t(f),stringsAsFactors=FALSE)
colnames(f_t_df) <- ids

f_t_df2 <- cbind(yr,as.character(tokens),f_t_df)

#Normalize columns
f_t_df2a <- f_t_df2

test1a <- sqrt(f_t_df2a[,3] %*% f_t_df2a[,3])
test1b <- sqrt(crossprod(f_t_df2a[,3]))

test2a <- sqrt(f_t_df2a[,4] %*% f_t_df2a[,4])
test2b <- sqrt(crossprod(f_t_df2a[,4]))

test3a <- sqrt(f_t_df2a[,5] %*% f_t_df2a[,5])
test3b <- sqrt(crossprod(f_t_df2a[,5]))

test4a <- sqrt(f_t_df2a[,6] %*% f_t_df2a[,6])
test4b <- sqrt(crossprod(f_t_df2a[,6]))

test5a <- sqrt(f_t_df2a[,7] %*% f_t_df2a[,7])
test5b <- sqrt(crossprod(f_t_df2a[,7]))

f_t_df2a[,3:ncol(f_t_df2)] <- apply(f_t_df2[,3:ncol(f_t_df2)], 2, function(x) {x/sqrt(crossprod(x))})

f_t_df3 <- as.data.frame(apply(f_t_df2[,3:ncol(f_t_df2)], 2, function(x) {crossprod(x,as.matrix(f_t_df2[,3:ncol(f_t_df2)]))}))

f_t_df4 <- cbind(1999,ids,f_t_df3)
f_t_df4[,2] <- as.character(f_t_df4[,2])

#rm(a,b,c,y,z,d,tokens_per_id,ids,tokens,yr,f,f_t_df,f_t_df2,f_t_df3,f_t_df4)