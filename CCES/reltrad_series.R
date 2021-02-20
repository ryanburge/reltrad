
library(tidyverse)
# library(haven)
library(car)


# cces <- read_dta("D://cces/data/cces.dta")

cces <- cces %>% 
  mutate(white = car::recode(race, "1=1; else=0")) %>% 
  mutate(black = car::recode(race, "2=1; else=0"))

## Baptist

cces <- cces %>%
  mutate(sbc = car::recode(baptist, "1=1; else=0")) %>% 
  mutate(sbc = sbc - black) %>% 
  mutate(sbc = car::recode(sbc, "1=1; else=0"))

cces <- cces %>%
  mutate(abc = car::recode(baptist, "2=1; else=0")) %>% 
  mutate(abc = abc - black) %>% 
  mutate(abc = car::recode(abc, "1=1; else=0"))

cces <- cces %>%
  mutate(ibc = car::recode(baptist, "5=1; else=0")) 

cces <- cces %>%
  mutate(bgc = car::recode(baptist, "6=1; else=0")) 

cces <- cces %>%
  mutate(mbc = car::recode(baptist, "7=1; else=0")) %>% 
  mutate(mbc = mbc - black) %>% 
  mutate(mbc = car::recode(mbc, "1=1; else=0"))

cces <- cces %>%
  mutate(cb = car::recode(baptist, "8=1; else=0")) 

cces <- cces %>%
  mutate(fwb = car::recode(baptist, "9=1; else=0")) 

cces <- cces %>%
  mutate(gabb = car::recode(baptist, "10=1; else=0")) 

cces <- cces %>%
  mutate(obc = car::recode(baptist, "90=1; else=0")) %>% 
  mutate(obc = obc - black) %>% 
  mutate(obc = car::recode(obc, "1=1; else=0"))

cces <- cces %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)


## Methodist
cces <- cces %>%
  mutate(fmc = car::recode(cces$methodist, "2=1; else=0")) 

cces <- cces %>%
  mutate(omc = car::recode(cces$methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = car::recode(omc, "2=1; else=0"))
  
cces <- cces %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces <- cces %>% 
  mutate(hiatt = car::recode(pew_attendance, "1:3=1; else=0")) %>% 
  mutate(nd = car::recode(nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  car::recode(evannd, "2=1; else=0"))

## Lutheran 

cces <- cces %>% 
  mutate(mz = car::recode(lutheran, "2=1; else=0")) %>% 
  mutate(wi = car::recode(lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces <- cces %>% 
  mutate(pca = car::recode(presbyterian, "2=1; else=0")) %>% 
  mutate(epc = car::recode(presbyterian, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces <- cces %>% 
  mutate(evanpent = car::recode(pentecostal, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces <- cces %>% 
  mutate(evancong = car::recode(congregational, "2=1; else=0"))

## Holiness
cces <- cces %>% 
  mutate(evanholy = car::recode(holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces <- cces %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = car::recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces <- cces %>% 
  mutate(abc = car::recode(cces$baptist, "2=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(epis = car::recode(cces$episcop, "1:90=1; else=0"))

cces <- cces %>% 
  mutate(luth = car::recode(cces$lutheran, "1=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(meth = car::recode(cces$methodist, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(pres = car::recode(cces$presby, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(cong = car::recode(cces$congreg, "1=1; 3=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(doc = car::recode(cces$protestant, "8=1; else=0"))

cces <- cces %>% 
  mutate(reform = car::recode(cces$protestant, "11=1; else=0"))

cces <- cces %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = car::recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces <- cces %>% 
  mutate(black = car::recode(race, "2=1; else=0"))

cces <- cces %>% 
  mutate(meth = car::recode(cces$methodist, "3:4=1; else=0"))

cces <- cces %>%
  mutate(sbc = car::recode(cces$baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = car::recode(sbc, "2=1; else=0"))

cces <- cces %>% 
  mutate(nbap = car::recode(cces$baptist, "3=1; else=0"))

cces <- cces %>%
  mutate(abc = car::recode(cces$baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = car::recode(abc, "2=1; else=0"))

cces <- cces %>%
  mutate(miss = car::recode(cces$baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = car::recode(miss, "2=1; else=0"))

cces <- cces %>%
  mutate(obap = car::recode(cces$baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = car::recode(obap, "2=1; else=0"))

cces <- cces %>%
  mutate(ometh = car::recode(cces$methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = car::recode(ometh, "2=1; else=0"))

cces <- cces %>% 
  mutate(apos = car::recode(cces$pentecost, "6=1; 7=1; else=0"))

cces <- cces %>%
  mutate(open = car::recode(cces$pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = car::recode(open, "2=1; else=0"))

cces <- cces %>%
  mutate(holy = car::recode(cces$holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = car::recode(holy, "2=1; else=0"))


cces <- cces %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = car::recode(bprot, "1:2=1; else=0"))

## Everything Else

cces <- cces %>% 
  mutate(catholic = car::recode(religion, "2=1; else=0"))

cces <- cces %>% 
  mutate(jewish = car::recode(religion, "5=1; else=0"))

cces <- cces %>% 
  mutate(other = car::recode(religion, "3=1; 6:8=1; 12=1; else=0"))

cces <- cces %>% 
  mutate(none = car::recode(religion, "9:11=1; else=0"))




