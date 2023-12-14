m <- fileToMatrix(paste0(repAoC,"data/day14.txt"))

# test<-"
# O....#....
# O.OO#....#
# .....##...
# OO.#O....O
# .O.....O#.
# O.#..O.#.#
# ..O..#O..O
# .......O..
# #....###..
# #OO..#....
# "
# m <- fileToMatrix(test) %>% unname()


# A ----

calc_vect <- function(v){
  s<-paste(v,collapse="")
  ss<-""
  while(ss!=s) {ss<-s;s<-str_replace_all(s,"\\.O","O\\.")}
  str_split_1(s,'')
}
lean_north <- function(m){
  l<-ncol(m)
  for(i in 1:l){
    m[,i]<-calc_vect(m[,i])
  }
  m
}
calc_load <- function(v){
  sum(which(rev(v)=="O"))
}
calc_north_load<-function(m){
  s<-0
  for(i in 1:ncol(m)){
    s<-s+calc_load(m[,i])
  }
  s
}
calc_north_load(lean_north(m))
# 112046


# B ----

nr <- nrow(m)
nc <- ncol(m)

apply_cycle <- function(m){
  k<-nrow(m)
  l<-ncol(m)
  # North
  for(i in 1:l){
    m[,i]<-calc_vect(m[,i])
  }
  # West
  for(i in 1:k){
    m[i,]<-calc_vect(m[i,])
  }
  # South
  for(i in 1:l){
    m[,i]<-rev(calc_vect(rev(m[,i])))
  }
  # East
  for(i in 1:k){
    m[i,]<-rev(calc_vect(rev(m[i,])))
  }
  m
}

# tibble for saving results until a duplicate is found
res <- tibble(cycle=0,situation=paste(m,collapse=('')))
cycle <- 0
while(!any(duplicated(res$situation))){
  cycle <- cycle + 1
  m <- apply_cycle(m)
  res %<>% add_row(cycle,situation=paste(m,collapse=('')))
}
# Duplicates
sit_rep <- res %>% last %>% pull(situation)
doublons <- res %>% filter(situation==sit_rep) %>% pull(cycle)
l_cycle <- doublons[2]-doublons[1]
final<- doublons[1] + (1000000000-doublons[1]) %% l_cycle
# Rebuild matrix
show_mat <- function(k){
  res %>% filter(cycle==k) %>% pull(situation) %>% strsplit('') %>% unname %>% unlist %>% matrix(nrow=nr,ncol=nc,byrow = FALSE)
}

show_mat(final) %>% calc_north_load

# 104619
