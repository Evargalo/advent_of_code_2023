e <- read.csv2(paste0(repAoC,"data/day10.txt"),header = FALSE,sep = "")
# e <- read.csv2(paste0(repAoC,"data/day10test.txt"),header = FALSE,sep = "")
# e <- read.csv2(paste0(repAoC,"data/day10test_1.txt"),header = FALSE,sep = "")
e %>% anyNA


# A ----
m <- buildMaze(e$V1)
maze <- m$Maze %>% mutate(x_y=paste0(x,'_',y))
m$CharSSpots
maze %<>% mutate(dist_to_s=if_else(t=="S",0,-1))

find4Neighbours(76,54,maze)
go_left <- c("-","J","7","S")
go_right <- c("-","L","F","S")
go_up <- c("|","J","L","S")
go_down <- c("|","F","7","S")

connect <- function(xx,yy,tt,x,y,t){
  if(abs(xx-x) + abs(yy-y)>1 | t=="." | tt==".") return(FALSE)
  if(xx==x & yy==y-1) return(t %in% go_left & tt %in% go_right)
  if(xx==x & yy==y+1) return(tt %in% go_left & t %in% go_right)
  if(xx==x-1 & yy==y) return(tt %in% go_down & t %in% go_up)
  if(xx==x+1 & yy==y) return(t %in% go_down & tt %in% go_up)
  return(FALSE)
}

findNext <- function(xx,yy,tt,dd){
  nei <- find4Neighbours(xx,yy,maze) %>% filter(dist_to_s==-1)
  if(nrow(nei)==0) return(nei)
  nei %>% rowwise %>% filter(connect(xx,yy,tt,x,y,t)) %>% ungroup %>% 
    mutate(dist_to_s=dd+1)
}
findNext(76,54,"S",0)

loop <- maze %>% filter(t=="S") 
new_loop <- loop
while(nrow(new_loop)>0){
  new_loop <- pmap_dfr(new_loop %>% select(xx=x,yy=y,tt=t,dd=dist_to_s),findNext)
  new_loop %<>% filter(x_y %notin% loop$x_y) 
  loop %<>% bind_rows(new_loop) 
}
loop %>% summarise(max(dist_to_s))
# 6682

# B ----

ground <- maze %>% filter(x_y %notin% loop$x_y)

inside_loop <- function(xx,yy){
  loop %<>% mutate(t=if_else(t=="S","-",t)) # for my data
  lr <- loop %>% filter(x==xx & y>yy & t %in% c(go_up,go_down)) %>% arrange(y) %>% pull(t)
  llr <- lag(lr)
  llr[1] <- "."
  s <- sum(lr %in% c("|","F","L") | (lr=="7" & llr=="F") | (lr=="J" & llr=="L"))
  return(as.logical(s%%2))
}
ground %>% select(xx=x,yy=y) %>% pmap_lgl(inside_loop) %>% sum

# 353
