
library(tidyverse)
# library(haven)
library(car)


# cces17 <- read_dta("D://cces/data/cces17.dta")

cces17 <- cces17 %>% 
  mutate(white = recode(race, "1=1; else=0"))

## Baptist

cces17 <- cces17 %>%
  mutate(sbc = recode(cces17$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = white + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(abc = recode(cces17$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = white + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(ibc = recode(cces17$religpew_baptist, "5=1; else=0")) 

cces17 <- cces17 %>%
  mutate(bgc = recode(cces17$religpew_baptist, "6=1; else=0")) 
  
cces17 <- cces17 %>%
  mutate(mbc = recode(cces17$religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = white + mbc) %>% 
  mutate(mbc = recode(mbc, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(cb = recode(cces17$religpew_baptist, "8=1; else=0")) 

cces17 <- cces17 %>%
  mutate(fwb = recode(cces17$religpew_baptist, "9=1; else=0")) 

cces17 <- cces17 %>%
  mutate(gabb = recode(cces17$religpew_baptist, "10=1; else=0")) 

cces17 <- cces17 %>%
  mutate(obc = recode(cces17$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = white + obc) %>% 
  mutate(obc = recode(obc, "2=1; else=0"))

cces17 <- cces17 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces17 <- cces17 %>%
  mutate(fmc = recode(cces17$religpew_methodist, "2=1; else=0")) 

cces17 <- cces17 %>%
  mutate(omc = recode(cces17$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))
  
cces17 <- cces17 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces17 <- cces17 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces17 <- cces17 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces17 <- cces17 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces17 <- cces17 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces17 <- cces17 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces17 <- cces17 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces17 <- cces17 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces17 <- cces17 %>% 
  mutate(abc = recode(cces17$religpew_baptist, "2=1; 4=1; else=0"))

cces17 <- cces17 %>% 
  mutate(epis = recode(cces17$religpew_episcop, "1:90=1; else=0"))

cces17 <- cces17 %>% 
  mutate(luth = recode(cces17$religpew_lutheran, "1=1; 4=1; else=0"))

cces17 <- cces17 %>% 
  mutate(meth = recode(cces17$religpew_methodist, "1=1; 90=1; else=0"))

cces17 <- cces17 %>% 
  mutate(pres = recode(cces17$religpew_presby, "1=1; 90=1; else=0"))

cces17 <- cces17 %>% 
  mutate(cong = recode(cces17$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces17 <- cces17 %>% 
  mutate(doc = recode(cces17$religpew_protestant, "8=1; else=0"))

cces17 <- cces17 %>% 
  mutate(reform = recode(cces17$religpew_protestant, "11=1; else=0"))

cces17 <- cces17 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces17 <- cces17 %>% 
  mutate(black = recode(race, "2=1; else=0"))

cces17 <- cces17 %>% 
  mutate(meth = recode(cces17$religpew_methodist, "3:4=1; else=0"))

cces17 <- cces17 %>%
  mutate(sbc = recode(cces17$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces17 <- cces17 %>% 
  mutate(nbap = recode(cces17$religpew_baptist, "3=1; else=0"))

cces17 <- cces17 %>%
  mutate(abc = recode(cces17$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(miss = recode(cces17$religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(obap = recode(cces17$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(ometh = recode(cces17$religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces17 <- cces17 %>% 
  mutate(apos = recode(cces17$religpew_pentecost, "6=1; 7=1; else=0"))

cces17 <- cces17 %>%
  mutate(open = recode(cces17$religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces17 <- cces17 %>%
  mutate(holy = recode(cces17$religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces17 <- cces17 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces17 <- cces17 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

cces17 <- cces17 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

cces17 <- cces17 %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

cces17 <- cces17 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))




