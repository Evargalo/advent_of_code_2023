
e <- readLines(paste0(repAoC,"data/day5.txt"))
# e <- readLines(paste0(repAoC,"data/day5test.txt"))

e <- e[e!=""]
seeds <- e[1] %>% cutBefore(": ") %>% str_split_1(" ") %>% as.numeric()
e <- e[-1]


# A ----

calc_loc <- function(seed){
  # step <- 0
  val <- seed
  for(i in 1:length(e)){
    # print(paste(i,val))
    if(grepl(pattern =  '-to-',x = e[i])) {
      # step <- step+1
      hasChanged <- FALSE
    } else{
      if(!hasChanged){
        v <- e[i] %>% str_split_1(" ") %>% as.numeric()
        if(val >=v[2] & val < v[2]+v[3]){
          val <- val+v[1]-v[2]
          hasChanged <- TRUE
        }
      }
    }
  }
  val
}

# calc_loc(seed = seeds[1])

map_dbl(.x = seeds,.f = calc_loc) %>% min
# 836040384


# B ----

# Initial seeds
input <- tibble(
  step=1,
  start=seeds[(1:(length(seeds)/2))*2-1],
  range=seeds[(1:(length(seeds)/2))*2]
)
input %<>% mutate(id_seed=row_number() %>% as.character,
                  id_seed_old="")

# table of the conversion rules
conversion <- tibble(step=integer())
step <- 0
id_rule<-1
for(i in 1:length(e)){
  if(grepl(pattern =  '-to-',x = e[i])) {
    step <- step+1
  } else{
    v <- e[i] %>% str_split_1(" ") %>% as.numeric()
    conversion <- conversion %>% bind_rows(
      tibble(
        step=step,
        id_rule=id_rule,
        V2=v[2],
        V1=v[1],
        V3=v[3]
      )
    )
    id_rule <- id_rule+1
  }
}

new_val<-tibble(step=integer(),
                id_seed=character(),
                start=numeric(),
                range=numeric()
)

input <- tibble(
  step=1,
  start=seeds[(1:(length(seeds)/2))*2-1],
  range=seeds[(1:(length(seeds)/2))*2]
)
input %<>% mutate(id_seed=row_number() %>% as.character)

# Apply a rule (id_rule,V2,V1,V3) at (step) to (id_seed,start,range)
affect_rule2 <- function(step,start,range,id_seed,id_rule,V2,V1,V3){
  if( start >= (V2+V3) || start+range <= V2 ) {
    return(new_val)
  }
  return(
    tibble(
      step= step+1,
      start= start-V2+V1,
      range= range,
      id_seed= id_seed
    )
  )
}

# Cut input in shorter intervals that fit within rules' range
split_range <- function(cut,step,start,range,id_seed){
  if(cut<start | cut>=start+range) return(tibble(step,start,range,id_seed))
  range_old <- range
  start_old <- start
  return(tibble(step,start,range=cut-start,id_seed) %>% 
           add_row(step,start=cut,range=range_old-cut+start_old,id_seed))
}

prep_tab <- function(tab,cur_step){
  cuts <- conversion %>% filter(step==cur_step) %>% 
    reframe(a=V2,b=V2+V3) %>% unlist %>% sort
  for(cut in cuts){
    tab<-pmap_dfr(tab,split_range,cut) %>% 
      filter(range>0)
  }
  tab %>% unique %>% 
    mutate(id_seed=as.character(row_number()))
}

# sum(input$range)

# Apply all rules to all seeds
tab <- input
all_tab <- tab
for(i in 1:7) {
  tab <- prep_tab(tab,i)
  new_tab <- purrr::pmap_dfr(.f = affect_rule2, tab %>% left_join(conversion)) 
  tab <- new_tab %>% bind_rows(tab %>% filter(id_seed %notin% new_tab$id_seed) %>% mutate(step=i+1)) # if no rule applies, the value remains the same
  all_tab <- all_tab %>% bind_rows(tab %>% filter(step==i+1))
  # print(all_tab %>% group_by(step) %>% summarise(sum(range)))
}

all_tab %>% filter(step==8) %>% summarise(min(start))

# 10834440