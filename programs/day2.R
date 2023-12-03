
e <- read.csv2(paste0(repAoC,"data/day2test.txt"),sep = ';',header = FALSE)
e <- read.csv2(paste0(repAoC,"data/day2.txt"),sep = ';',header = FALSE)


# A ----

# 12 red cubes, 13 green cubes, and 14 blue cubes

d <- e %>% mutate(V1=cutBefore(V1,'Game ')) %>% 
               mutate(
             V7=cutBefore(V1,':'),
             id=cutAfter(V1,':')
             ) %>% 
  mutate(id=as.integer(id)) %>% 
  select(-V1) %>% 
  pivot_longer(cols=-id) %>% 
  filter(value!='')

is_valid <- function(s){
  color <- cutBefore(s,' ')
  val <- cutAfter(s,' ') %>% as.integer
  return(is.na(color) | (val+(color=='green')+2*(color=='red'))<15)
}

separate_longer_delim(data = d,cols = value,delim=",") %>% 
  mutate(value=substr(value,1,1)) %>% 
  group_by(value) %>% 
  count

separate_longer_delim(data = d,cols = value,delim=",") %>% 
  mutate(value=substr(value,2,nchar(value))) %>% 
  mutate(valid=is_valid(value)) -> f

f %>% 
  group_by(id) %>% 
  summarise(ok=all(valid)) %>% 
  ungroup -> games

games %>% filter(ok) %>% summarise(sum(id))


# 2416

# B ----
g <- f %>% separate_wider_delim(value,' ',names = c('a','b')) %>% 
  group_by(id,b) %>% 
  mutate(a=as.integer(a)) %>% 
  arrange(desc(a)) %>% 
  filter(row_number()==1) %>% 
  ungroup %>% 
  arrange(id,b) %>% 
  select(-valid,-name) %>% 
  ungroup
g %>% group_by(id) %>% count %>% group_by(n) %>% count

g %>% group_by(id) %>% summarise(power=prod(a)) %>% 
  ungroup %>% summarise(sum(power))
