# data ----

e <- read.csv2(paste0(repAoC,"data/day7.txt"),header = FALSE,sep = " ")

# Part A----

s <- e$V1[1]

dec_hand <- function(s){
  compo<-tibble(card=s %>% str_split_1('')) %>% group_by(card) %>% count %>% arrange(-n) %>% pull(n)
if(compo[1]==5) return("1:5k")
  if(compo[1]==4) return("2:4k")
  if(compo[1]==3 & compo[2]==2) return("3:full")
  if(compo[1]==3) return("4:3k")
  if(compo[1]==2 & compo[2]==2) return("5:2p")
  if(compo[1]==2) return("6:2k")
  return("7:1k")
}
dec_hand(s)

e %<>% separate_wider_position(V1,widths = c(c1=1,c2=1,c3=1,c4=1,c5=1),cols_remove = FALSE) 

e %<>% rowwise %>% mutate(type=dec_hand(V1)) %>% ungroup 

e %<>% 
  mutate(across(.cols = starts_with("c"),.fns=\(x)ordered(x,levels=c("A","K","Q","J","T",9:2))))

e %<>% arrange(type,c1,c2,c3,c4,c5) %>% 
  mutate(rank=(n()-row_number()+1))
e %>% summarise(sum(rank*V2))

# 247961593

# Part B ----

e %<>% 
  mutate(across(.cols = starts_with("c"),.fns=\(x)ordered(x,levels=c("A","K","Q","T",9:2,"J"))))

dec_hand <- function(s){
  compo <- tibble(card=s %>% str_split_1('')) %>% group_by(card) %>% count %>% arrange(-n) 
  if(all(compo$card=="J")){
   return("1:5k")
  }
  if(any(compo$card=="J")){
    new_card <- compo %>% filter(card!="J") %>% pull(card) %>% first()
    compo %<>% mutate(card=if_else(card=="J",new_card,card)) %>% group_by(card) %>% summarise(n=sum(n)) %>% arrange(-n) 
  }
  compo %<>% pull(n)
  if(compo[1]==5) return("1:5k")
  if(compo[1]==4) return("2:4k")
  if(compo[1]==3 & compo[2]==2) return("3:full")
  if(compo[1]==3) return("4:3k")
  if(compo[1]==2 & compo[2]==2) return("5:2p")
  if(compo[1]==2) return("6:2k")
  return("7:1k")
}
dec_hand(s)

e %<>% rowwise %>% mutate(type=dec_hand(V1)) %>% ungroup 

e %<>% arrange(type,c1,c2,c3,c4,c5) %>% 
  mutate(rank=(n()-row_number()+1))
e %>% summarise(sum(rank*V2))

# 248750699
