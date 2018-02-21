install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("choroplethrMaps")
install.packages("dplyr")
install.packages("dtplyr")
library(plyr)
library(choroplethr)
library(choroplethrMaps)
library(readr)
library(dplyr)
library(data.table)
library(tidyverse)
library(dtplyr)
dat <- fread("curl https://www.fhwa.dot.gov/bridge/nbi/2017hwybronlyonefile.zip | funzip") %>% as.tbl

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008" , "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020" , "ADT_029", "YEAR_ADT_030", "YEAR_BUILT_027" , "DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060", "CHANNEL_COND_061","CULVERT_COND_062", "DATE_OF_INSPECT_090", "FRACTURE_092A", "UNDWATER_LOOK_SEE_092B" , "SPEC_INSPECT_092C")

x = select(dat, one_of(keep))  

wi = filter(x, STATE_CODE_001 == 55)
wi = mutate(wi, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

wi$rate = rateIt(wi$cond)
table(wi$cond)
table(wi$rate)
wi = filter(wi, cond>1)
ggplot(data = wi, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = rate)) +geom_point() + geom_smooth()

map = ggplot(data = wi, mapping = aes(y = lat, x = lon))
map + geom_point(aes(col=rate))+ scale_colour_brewer(palette = "Spectral")  
wi = mutate(wi, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
wi = wi %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)
wi$fips %>% head

wi = wi %>% mutate(good = (rate == "good"))
table(wi$good)
fipsdat = wi %>% group_by(fips) %>% summarize(propGoodRoads = mean(good))
fipsdat %>% transmute(region = fips, value = propGoodRoads) %>% county_choropleth(state_zoom = "wisconsin")


usa = dat
usa = usa %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)
ADT = usa %>% group_by(fips) %>% summarize(mean_traffic = mean(ADT_029))

ADT2 = na.omit(ADT)
ADT2_2 <- as.data.frame(ADT2)
ADT2_2 %>% transmute(region = fips, value = mean_traffic) %>% county_choropleth(title = "USA Traffic Condition")
