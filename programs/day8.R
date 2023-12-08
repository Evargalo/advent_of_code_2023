instr <- "LRRLRLRRRLLRLRRRLRLLRLRLRRLRLRRLRRLRLRLLRRRLRRLLRRRLRRLRRRLRRLRLRLLRRLRLRRLLRRRLLLRRRLLLRRLRLRRLRLLRRRLRRLRRRLRRLLRRRLRRRLRRRLRLRRLRLRRRLRRRLRRLRLRRLLRRRLRRLLRRLRRLRLRLRRRLRLLRRRLRRLRRRLLRRLLLLLRRRLRRLLLRRRLRRRLRRLRLLLLLRLRRRLRRRLRLRRLLLLRLRRRLLRRRLRRRLRLRLRRLRRLRRLRLRLLLRLRRLRRLRRRLRRRLLRRRR"

instr %<>% str_split_1('')
l <- length(instr)

e <- read.csv2(paste0(repAoC,"data/day8.txt"),header = FALSE,sep = " ")
e %<>% mutate(L=substr(V3,2,4),
              R=substr(V4,1,3)
) %>% 
  select(-V4,-V3,-V2)

# Part A----

step <- 0
pos <- "AAA"
while(pos!="ZZZ"){
  step <- step+1
  if(step %% 1000 == 0) print(step)
  pos <- e %>% filter(V1==pos) %>% pull(instr[((step-1) %% l)+1])
}
step

# 21389

# Part B----
starting_points <- e %>% filter(substr(V1,3,3)=='A') %>% pull(V1)
time <- function(pos){
  step <- 0
  while(substr(pos,3,3)!="Z"){
    step <- step+1
    if(step %% 1000 == 0) print(step)
    pos <- e %>% filter(V1==pos) %>% pull(instr[((step-1) %% l)+1])
  }
  step
}
times <- map_int(starting_points,time) 

ppcm(times[1],times[2])
ppcm(1550849,times[3])
ppcm(66686507,times[4])
ppcm(4868115011,times[5])
ppcm(296955015671,times[6])

# 821083806112641

reduce(times[1:3],ppcm) ; reduce(times[4:6],ppcm)
ppcm(66686507,92635759)

reduce(times[1:6],ppcm)
#