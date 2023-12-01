
d <- read_delim(paste0(repAoC,"data/day1test.txt"),col_names = FALSE, delim=' ')
d <- read_delim(paste0(repAoC,"data/day1.txt"),col_names = FALSE, delim=' ')
v<-d$X1


# A ----

score<-0
for(a in v){
  b<-str_split(a,'') %>% unlist 
  b1<-b[!b %in% letters] %>% as.integer
  score<-score+10*b1[1]+b1[length(b1)]
}
score

# 54953


# B ----

replace_numbers <- function(s){
  s %>% 
    str_replace_all("one","1") %>% 
    str_replace_all("two","2") %>% 
    str_replace_all("three","3") %>% 
    str_replace_all("four","4") %>% 
    str_replace_all("five","5") %>% 
    str_replace_all("six","6") %>% 
    str_replace_all("seven","7") %>% 
    str_replace_all("eight","8") %>% 
    str_replace_all("nine","9")
}

regex_expr <- paste0("one|two|three|four|five|six|seven|eight|nine|",paste(0:9,collapse = '|'))

find_first <- function(s){
  spot <- stri_locate_first(str = s,regex = regex_expr)
  substr(s,spot[1],spot[2]) %>% replace_numbers %>% as.integer()
}

find_last <- function(s){
  spot <- stri_locate_last(str = s,regex = regex_expr)
  substr(s,spot[1],spot[2]) %>% replace_numbers %>% as.integer()
}

stri_locate_last_perso <- function(s,regex = regex_expr){
  l<-nchar(s)
  for(i in l:1){
    spot <- stri_locate_first(str = substr(s,i,l),regex = regex)
    if(!is.na(spot[1])) return(spot+i-1) 
  }
  return(stri_locate_first(str = s,regex = regex))
}
s<-"five8mpkpdfiveeightfoursevenine"
stri_locate_last_perso(s)

new_find_last <- function(s){
  spot <- stri_locate_last_perso(s = s,regex = regex_expr)
  substr(s,spot[1],spot[2]) %>% replace_numbers %>% as.integer()
}

res <- tibble(v) %>% rowwise %>% mutate(first_num=find_first(v),last_num=find_last(v),new_last_num=new_find_last(v)) %>% ungroup
res %<>% mutate(val=first_num*10+last_num,new_val = first_num*10+new_last_num) 
res %>% summarise(sum(val),sum(new_val))

# 53885 too high
# 53868 correct

res %>% filter(last_num != new_last_num)
