
library(tidyverse)
# library(haven)
library(car)


# cces <- read_dta("D://cces/data/cces.dta")

cces <- cces %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else=0"))

## Baptist

cces <- cces %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = sbc - black) %>% 
  mutate(sbc = recode(sbc, "1=1; else=0"))

cces <- cces %>%
  mutate(abc = recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = abc - black) %>% 
  mutate(abc = recode(abc, "1=1; else=0"))

cces <- cces %>%
  mutate(ibc = recode(religpew_baptist, "5=1; else=0")) 

cces <- cces %>%
  mutate(bgc = recode(religpew_baptist, "6=1; else=0")) 

cces <- cces %>%
  mutate(mbc = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = mbc - black) %>% 
  mutate(mbc = recode(mbc, "1=1; else=0"))

cces <- cces %>%
  mutate(cb = recode(religpew_baptist, "8=1; else=0")) 

cces <- cces %>%
  mutate(fwb = recode(religpew_baptist, "9=1; else=0")) 

cces <- cces %>%
  mutate(gabb = recode(religpew_baptist, "10=1; else=0")) 

cces <- cces %>%
  mutate(obc = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = obc - black) %>% 
  mutate(obc = recode(obc, "1=1; else=0"))

cces <- cces %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)


## Methodist
cces <- cces %>%
  mutate(fmc = recode(cces$methodist, "2=1; else=0")) 

cces <- cces %>%
  mutate(omc = recode(cces$methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))
  
cces <- cces %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces <- cces %>% 
  mutate(hiatt = recode(pew_attendance, "1:3=1; else=0")) %>% 
  mutate(nd = recode(nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces <- cces %>% 
  mutate(mz = recode(lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces <- cces %>% 
  mutate(pca = recode(presbyterian, "2=1; else=0")) %>% 
  mutate(epc = recode(presbyterian, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces <- cces %>% 
  mutate(evanpent = recode(pentecostal, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces <- cces %>% 
  mutate(evancong = recode(congregational, "2=1; else=0"))

## Holiness
cces <- cces %>% 
  mutate(evanholy = recode(holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces <- cces %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces <- cces %>% 
  mutate(abc = recode(cces$baptist, "2=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(epis = recode(cces$episcop, "1:90=1; else=0"))

cces <- cces %>% 
  mutate(luth = recode(cces$lutheran, "1=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(meth = recode(cces$methodist, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(pres = recode(cces$presby, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(cong = recode(cces$congreg, "1=1; 3=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(doc = recode(cces$protestant, "8=1; else=0"))

cces <- cces %>% 
  mutate(reform = recode(cces$protestant, "11=1; else=0"))

cces <- cces %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces <- cces %>% 
  mutate(black = recode(race, "2=1; else=0"))

cces <- cces %>% 
  mutate(meth = recode(cces$methodist, "3:4=1; else=0"))

cces <- cces %>%
  mutate(sbc = recode(cces$baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces <- cces %>% 
  mutate(nbap = recode(cces$baptist, "3=1; else=0"))

cces <- cces %>%
  mutate(abc = recode(cces$baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces <- cces %>%
  mutate(miss = recode(cces$baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces <- cces %>%
  mutate(obap = recode(cces$baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces <- cces %>%
  mutate(ometh = recode(cces$methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces <- cces %>% 
  mutate(apos = recode(cces$pentecost, "6=1; 7=1; else=0"))

cces <- cces %>%
  mutate(open = recode(cces$pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces <- cces %>%
  mutate(holy = recode(cces$holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces <- cces %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces <- cces %>% 
  mutate(catholic = recode(religion, "2=1; else=0"))

cces <- cces %>% 
  mutate(jewish = recode(religion, "5=1; else=0"))

cces <- cces %>% 
  mutate(other = recode(religion, "3=1; 6:8=1; 12=1; else=0"))

cces <- cces %>% 
  mutate(none = recode(religion, "9:11=1; else=0"))




