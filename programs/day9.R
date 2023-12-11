e <- read.csv2(paste0(repAoC,"data/day9.txt"),header = FALSE,sep = " ")
e %>% anyNA
e %<>% as.matrix
l <- nrow(e)

# A ----
next_elem <- function(v){
  if(all(v==0)) return(0)
  last(v) + next_elem(diff(v))
}
n_e <- function(i){next_elem(e[i,])}

map_int(1:l,n_e) %>% sum
# 1782868781

# B ----

prev_elem <- function(v){
  if(all(v==0)) return(0)
  first(v) - prev_elem(diff(v))
}
p_e <- function(i){prev_elem(e[i,])}

map_int(1:l,p_e) %>% sum
# 1057