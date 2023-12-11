# m <- fileToMatrix(paste0(repAoC,"data/day11test.txt"))
m <- fileToMatrix(paste0(repAoC,"data/day11.txt"))

# A ----

expand_universe <- function(m){
  k <- nrow(m)
  l <- ncol(m)
  i <- 2
  while(i<k){
    if(all(m[i,]==".")) {
      m <- rbind(m[1:i,],m[i,],m[(i+1):k,])
      k <- k+1
      i <- i+2
    }
    else{i <- i+1}
  }
  if(all(m[k,]==".")) {
    m <- rbind(m,m[k,])
    k <- k+1
  }
  if(all(m[1,]==".")) {
    m <- rbind(m[1,],m)
    k <- k+1
  }
  j <- 2
  while(j<l){
    if(all(m[,j]==".")) {
      m <- cbind(m[,1:j],m[,j],m[,(j+1):l])
      l <- l+1
      j <- j+2
    }
    else{j <- j+1}
  }
  if(all(m[,l]==".")) {
    m <- cbind(m,m[,l])
    l <- l+1
  }
  if(all(m[,1]==".")) {
    m <- cbind(m[,1],m)
    l <- l+1
  }
  m
}

m <- expand_universe(m)

df <- tibble(x=integer(),y=integer())
k <- nrow(m)
l <- ncol(m)
for(i in 1:k){
  for(j in 1:l){
    if(m[i,j]=="#") df %<>% add_row(x=i,y=j)
  }
}
df %<>% mutate(id=row_number(),dummy=1)

table_dist <- df %>% full_join(df,by="dummy") 
  filter(id.x<id.y) %>% 
  mutate(dist=abs(y.y-y.x)+abs(x.y-x.x)) 

table_dist %>% 
  summarise(sum(dist))

# 9623138

# B----

m <- fileToMatrix(paste0(repAoC,"data/day11.txt"))
k <- nrow(m)
l <- ncol(m)

empty_rows <- c()
empty_files <- c()
for(i in 1:k){
  if(all(m[i,]==".")) empty_rows <- c(empty_rows,i)
}  
  for(j in 1:l){
    if(all(m[,j]==".")) empty_files <- c(empty_files,j)
}

df <- tibble(x=integer(),y=integer())
for(i in 1:k){
  for(j in 1:l){
    if(m[i,j]=="#") df %<>% add_row(x=i,y=j)
  }
}
df %<>% mutate(id=row_number(),dummy=1)

calc_ins_col <-function(y1,y2){
  abs(sum(y2>empty_files)-sum(y1>empty_files))
}
calc_ins_row <-function(y1,y2){
  abs(sum(y2>empty_rows)-sum(y1>empty_rows))
}

table_dist <- df %>% full_join(df,by="dummy") 
  filter(id.x<id.y) %>% rowwise() %>% 
  mutate(ins_col=calc_ins_col(y.x,y.y),
         ins_row=calc_ins_row(x.x,x.y)) %>% ungroup %>% 
  mutate(dist=abs(y.y-y.x)+abs(x.y-x.x)+(10^6-1)*(ins_col+ins_row)) 

table_dist %>% summarise(sum(dist))
# 726820169514