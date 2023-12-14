e <- read.csv2(paste0(repAoC,"data/day12.txt"),header = FALSE,sep = " ")
# e <- read.csv2(paste0(repAoC,"data/day12test.txt"),header = FALSE,sep = " ")
e %>% anyNA

# A ----

count_splits <- function(s,instr){
  if(length(s)==0) return(as.integer(sum(instr)==0))
  if(length(instr)==0) return(as.integer(all(s!="#")))
  if(sum(s %in% c('#','?')) < sum(instr)) return(0)
  
  if(s[1]==".") return(count_splits(s[-1],instr))
  if(s[1]=="#" & instr[1]>1) {
    if(s[2]=='.') return(0)
    instr[1] <- instr[1]-1
    return(count_splits(c("#",s[-(1:2)]),instr))
  }
  if(s[1]=="#" & instr[1]==1) {
    if(length(s)==1) return(as.integer(length(instr)==1))
    if(s[2]=='#') return(0)
    return(count_splits(s[-(1:2)],instr[-1]))  
  }
  if(s[1]=="?"){
    return(count_splits(c(".",s[-1]),instr) + count_splits(c("#",s[-1]),instr))  
  }
  return(0)
}

c_s <- function(V1,V2){
  s <- str_split_1(V1,'')
  instr<- str_split_1(V2,',') %>% as.integer
  count_splits(s,instr)
}

pmap_int(e,c_s) %>% sum
# 7599

# A & B ----

rm_dots <- function(V1){
  while(firstChar(V1)==".") V1 <- rmFirstChar(V1)
  while(grepl("\\.\\.",V1)) V1 <- str_replace_all(string = V1,pattern = "\\.\\.",replacement = "\\.")
  V1
}

calc_config <- function(V1,V2){
  V1<-rm_dots(V1)
  s <-str_split_1(V1,"")
  if(last(s)!=".") s<- c(s,'.')
  instr <- str_split_1(V2,",") %>% as.integer
  ns <- length(s)
  ni <- length(instr)
  nb_sol <- tibble(i=0,j=1,k=0,sol=as.double(1),cur_ins=0)
  for(ii in 1:ns){
    new_sol <- nb_sol %>% filter(i==ii-1)
    spr <- s[ii]
    if(spr=="#"){
      new_sol %<>% mutate(i=ii,k=k+1)
    }
    if(spr=="."){
      new_sol <- new_sol %>% mutate(cur_ins=instr[j]) %>% 
        filter(k==cur_ins) %>% mutate(i=ii,k=0,j=j+1) %>%
        bind_rows(
          new_sol %>% 
            filter(k==0) %>% mutate(i=ii)
        )
    }
    if(spr=="?"){
      new_sol <- new_sol %>% mutate(i=ii,k=k+1) %>% bind_rows(
        new_sol %>% mutate(cur_ins=instr[j]) %>% 
          filter(k==cur_ins) %>% mutate(i=ii,k=0,j=j+1)
      ) %>% bind_rows(
        new_sol %>% 
          filter(k==0) %>% mutate(i=ii)
      )
    }
    nb_sol %<>% add_row(new_sol) 
    nb_sol %<>% 
      group_by(i,j,k,cur_ins) %>% 
      summarise(sol=sum(sol),.groups="drop" )
  }
  nb_sol %>% filter(i==ns,j==ni+1) %>% summarise(s=sum(sol)) %>% pull(s)
}


# A----
pmap_int(e,calc_config) %>% sum

# B ----

d <- e %>% mutate(V1=paste(V1,V1,V1,V1,V1,sep="?"),V2=paste(V2,V2,V2,V2,V2,sep=','))
v <- pmap_dbl(d,calc_config) 
sum(v)
# 15454556629917
