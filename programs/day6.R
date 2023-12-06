
e <- readLines(paste0(repAoC,"data/day6.txt"))

times <- e[1] %>% str_split_1(" ") 
times <- times[times!=""][-1] %>% as.numeric

scores <- e[2] %>% str_split_1(" ") 
scores <- scores[scores!=""][-1] %>% as.numeric


# A----
calc_race <- function(time,press){
  if(press>=time) return(0)
  (time-press)*press
}
calc_race(7,4)

res<-1
for(i in 1:4){
  t<-times[i]
  dist <- map2_int(.x = t,.y = 1:(t-1),calc_race)
  res<-res*(sum(dist>scores[i]))
}
res
# 4811940

# B ----
t<-paste0(times,collapse = "") %>% as.numeric
s<-paste0(scores,collapse = "") %>% as.numeric

# p= press ; t= race duration ; s= current record
# p² - t*p + s = 0
delta=t^2-4*s
p1 = (-t-sqrt(delta))/2
p2 = (-t+sqrt(delta))/2
p2-p1+1 %>% round
# 30077773