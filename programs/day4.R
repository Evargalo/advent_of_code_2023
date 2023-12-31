
e <- readLines(paste0(repAoC,"data/day4.txt"))

# A ----

s <- e[1]

score_card <- function(s){
  v <- str_split_1(s,' ')
  v <- v[v!='']
  l<-length(v[duplicated(v)])
  if(l==0) return(0)
  return (2^(l-1))
}
score_card(s)

map_int(e,score_card) %>% sum

# 28750

# B ----

score_card <- function(s){
  v <- str_split_1(s,' ')
  v <- v[v!='']
  l<-length(v[duplicated(v)])
  return (l)
}

mat <- matrix(ncol=2,byrow = FALSE,c(map_int(e,score_card),rep(1,length(e))))

for(i in 1:(nrow(mat)-1)){
  k<-mat[i,1]
  l<-mat[i,2]
  if(k==0) next()
  mat[i+(1:k),2] <- mat[i+(1:k),2]+l
}
sum(mat[,2])

# 10212704