e <- read_lines(paste0(repAoC,"data/day13.txt"))
e %>% tail

coupures <- which(e=="")
coupures %>% tail

coupures <- c(coupures,length(e)+1)
l <- length(coupures)

e[1:13]

matrices <- list()

for(k in 1:l){
  j <- coupures[k]
  i <- max(c(0,coupures[coupures<j]))+1
  m <- paste0(e[i:(j-1)],collapse='') %>% 
    str_split_1(pattern = '') %>% 
  matrix(nrow = j-i,byrow = TRUE)
  matrices[[k]]<-m
}


# A ----
res <- tibble(numero_mat=1:l,horiz=0,vert=0)

num_mat <- 1
calc_refl <- function(num_mat){
  print(num_mat)
  mat <- matrices[[num_mat]]
  k <- nrow(mat)
  l <- ncol(mat)
  for(i in 2:k){
    if(any(mat[i-1,]!=mat[i,])) next
    nrep <- min(k-i,i-2)
    while(nrep>=1){
      if(any(mat[i-1-nrep,]!=mat[i+nrep,])) {nrep<- (-1)}
      nrep <- nrep-1
    }
    if(nrep==0) {
      res <<- res %>% mutate(horiz=if_else(numero_mat==num_mat,i-1,horiz))
      return(TRUE)
    }
    }
  for(i in 2:l){
    if(any(mat[,i-1]!=mat[,i])) next
    nrep <- min(l-i,i-2)
    while(nrep>=1){
      if(any(mat[,i-1-nrep]!=mat[,i+nrep])) {nrep <- (-1)}
      nrep <- nrep-1
    }
    if(nrep==0) {
      res <<- res %>% mutate(vert=if_else(numero_mat==num_mat,i-1,vert))
      return(TRUE)
    }
  }
  return(FALSE)
}

sapply(X = 1:l,FUN = calc_refl)

res %>% summarise(sum(horiz)*100+sum(vert))
# 41859


# B ----
res <- tibble(numero_mat=1:l,horiz=0,vert=0)

num_mat <- 1
calc_refl_2 <- function(num_mat){
  print(num_mat)
  mat <- matrices[[num_mat]]
  k <- nrow(mat)
  l <- ncol(mat)
  for(i in 2:k){
    s <- sum(mat[i-1,]!=mat[i,])
    if(s>1) next
    nrep <- min(k-i,i-2)
    while(nrep>=1 & s<2){
      s<-s+sum(mat[i-1-nrep,]!=mat[i+nrep,])
      nrep <- nrep-1
    }
    if(s==1) {
      res <<- res %>% mutate(horiz=if_else(numero_mat==num_mat,i-1,horiz))
      return(TRUE)
    }
  }
  for(i in 2:l){
    s <- sum(mat[,i-1]!=mat[,i])
    if(s>1) next
    nrep <- min(l-i,i-2)
    while(nrep>=1 & s<2){
      s<-s+sum(mat[,i-1-nrep]!=mat[,i+nrep])
      nrep <- nrep-1
    }
    if(s==1) {
      res <<- res %>% mutate(vert=if_else(numero_mat==num_mat,i-1,vert))
      return(TRUE)
    }
  }
  return(FALSE)
}

sapply(X = 1:l,FUN = calc_refl_2)

res %>% summarise(sum(horiz)*100+sum(vert))
# 



