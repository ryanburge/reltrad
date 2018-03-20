# cces12 <- read_dta("D://cces/data/cces12.dta")


library(tidyverse)
# library(haven)
library(car)


# cces12 <- read_dta("D://cces/data/cces12.dta")

cces12 <- cces12 %>% 
  mutate(white = recode(race, "1=1; else=0"))

## Baptist

cces12 <- cces12 %>%
  mutate(sbc = recode(cces12$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(abc = recode(cces12$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(ibc = recode(cces12$religpew_baptist, "5=1; else=0")) 

cces12 <- cces12 %>%
  mutate(bgc = recode(cces12$religpew_baptist, "6=1; else=0")) 

cces12 <- cces12 %>%
  mutate(mbc = recode(cces12$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(cb = recode(cces12$religpew_baptist, "8=1; else=0")) 

cces12 <- cces12 %>%
  mutate(fwb = recode(cces12$religpew_baptist, "9=1; else=0")) 

cces12 <- cces12 %>%
  mutate(gabb = recode(cces12$religpew_baptist, "10=1; else=0")) 

cces12 <- cces12 %>%
  mutate(obc = recode(cces12$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces12 <- cces12 %>%
  mutate(fmc = recode(cces12$religpew_methodist, "2=1; else=0")) 

cces12 <- cces12 %>%
  mutate(omc = recode(cces12$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces12 <- cces12 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces12 <- cces12 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces12 <- cces12 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces12 <- cces12 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces12 <- cces12 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces12 <- cces12 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces12 <- cces12 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces12 <- cces12 %>% 
  mutate(abc = recode(cces12$religpew_baptist, "2=1; 4=1; else=0"))

cces12 <- cces12 %>% 
  mutate(epis = recode(cces12$religpew_episcop, "1:90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(luth = recode(cces12$religpew_lutheran, "1=1; 4=1; else=0"))

cces12 <- cces12 %>% 
  mutate(meth = recode(cces12$religpew_methodist, "1=1; 90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(pres = recode(cces12$religpew_presby, "1=1; 90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(cong = recode(cces12$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(doc = recode(cces12$religpew_protestant, "8=1; else=0"))

cces12 <- cces12 %>% 
  mutate(reform = recode(cces12$religpew_protestant, "11=1; else=0"))

cces12 <- cces12 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces12 <- cces12 %>% 
  mutate(black = recode(race, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(meth = recode(cces12$religpew_methodist, "3:4=1; else=0"))

cces12 <- cces12 %>%
  mutate(sbc = recode(cces12$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(nbap = recode(cces12$religpew_baptist, "3=1; else=0"))

cces12 <- cces12 %>%
  mutate(abc = recode(cces12$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(miss = recode(cces12$religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(obap = recode(cces12$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(ometh = recode(cces12$religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(apos = recode(cces12$religpew_pentecost, "6=1; 7=1; else=0"))

cces12 <- cces12 %>%
  mutate(open = recode(cces12$religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(holy = recode(cces12$religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces12 <- cces12 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces12 <- cces12 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

cces12 <- cces12 %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

cces12 <- cces12 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))




