
e <- fileToMatrix(paste0(repAoC,"data/day3.txt"))
# e <- fileToMatrix(paste0(repAoC,"data/day3test2.txt"))

e %>% table %>% names -> content
content[content %notin% c(0:9,".")] -> symbols

numbers <- tibble(value=integer(),line=integer(),pos_left=integer(),pos_right=integer())
for(i in 1:nrow(e)){
  for(j in 1:ncol(e)){
    val <- ""
    while(j <= ncol(e) && e[i,j] %in% 0:9){
      val <- paste0(val,e[i,j])
      j <- j+1
    }
    if(nchar(val)>0){
      pos_left <- j-nchar(val)
      pos_right <- j-1
      numbers <<- numbers %>% add_row(value=as.integer(val),line=i,pos_left,pos_right)
    }     
  }
}

# numbers %<>% group_by(line,pos_right) %>% filter(row_number()==1) %>% ungroup
numbers %<>% group_by(line,pos_right) %>% filter(value==max(value)) %>% ungroup
# e[1,25:30]

# A ----

include_number <- function(line,pos_left,pos_right){
  f <- e[max(line-1,1):min(line+1,nrow(e)),max(pos_left-1,1):min(pos_right+1,ncol(e))]
  if(any(f %in% symbols)) return(TRUE)
  return(FALSE)
}

count_adjacent_symbols  <- function(line,pos_left,pos_right){
  f <- e[max(line-1,1):min(line+1,nrow(e)),max(pos_left-1,1):min(pos_right+1,ncol(e))]
  return(sum(f %in% symbols))
}

numbers %<>% rowwise %>% 
  mutate(include=include_number(line,pos_left,pos_right)) %>% 
  ungroup

numbers %<>% rowwise %>% 
  mutate(nb_symbols=count_adjacent_symbols(line,pos_left,pos_right)) %>% 
  ungroup

numbers %>% group_by(include,nb_symbols) %>% count

numbers %>% filter(include) %>% 
  summarise(sum(value))

numbers %>% 
  summarise(sum(value * nb_symbols))

# test 4361 ok

# 526404


# B ----

gears <- tibble(i=integer(),j=integer(),val=integer())

for(i in 1:nrow(e)){
  for(j in 1:ncol(e)){
    if(e[i,j]=='*'){
    gears <<- gears %>% add_row(i,j)
    }     
  }
}
gears %<>% mutate(id=row_number())

find_adjacent_gears <- function(line,pos_left,pos_right){
  f <- gears %>% filter(i %in% max(line-1,1):min(line+1,nrow(e)),
                        j %in% max(pos_left-1,1):min(pos_right+1,ncol(e))
  )
  if(nrow(f)==1) return((f$id))
  return(0)
}
find_adjacent_gears(1,53,55)
find_adjacent_gears(2,21,23)


numbers %<>% filter(include) %>% rowwise %>% 
  mutate(gear=find_adjacent_gears(line,pos_left,pos_right)) %>% 
  ungroup


numbers %>% filter(gear>0) %>% group_by(gear) %>%
  summarise(n_parts=n(),val=prod(value)) %>% 
  ungroup %>% filter(n_parts==2) %>% 
  summarise(sum(val))

# 84399773
