library(tidyverse)
SeoulEmp = read_delim("SeoulEmpl.txt",delim='\t')
x<-SeoulEmp %>% filter(SeoulEmp$기간 == 2015) %>% select(starts_with("취업시간별_")) 
y<-as.numeric(gsub(",", "", x))
pie(y,labels=names(x))

