
library(tidyverse)
# library(haven)
library(car)


# cces19 <- read_dta("D://cces/data/cces19.dta")

cces19 <- cces19 %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else=0"))

## Baptist


cces19 <- cces19 %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = sbc - black) %>% 
  mutate(sbc = recode(sbc, "1=1; else=0"))

cces19 <- cces19 %>%
  mutate(abc = recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = abc - black) %>% 
  mutate(abc = recode(abc, "1=1; else=0"))

cces19 <- cces19 %>%
  mutate(ibc = recode(religpew_baptist, "5=1; else=0")) 

cces19 <- cces19 %>%
  mutate(bgc = recode(religpew_baptist, "6=1; else=0")) 

cces19 <- cces19 %>%
  mutate(mbc = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = mbc - black) %>% 
  mutate(mbc = recode(mbc, "1=1; else=0"))

cces19 <- cces19 %>%
  mutate(cb = recode(religpew_baptist, "8=1; else=0")) 

cces19 <- cces19 %>%
  mutate(fwb = recode(religpew_baptist, "9=1; else=0")) 

cces19 <- cces19 %>%
  mutate(gabb = recode(religpew_baptist, "10=1; else=0")) 

cces19 <- cces19 %>%
  mutate(obc = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = obc - black) %>% 
  mutate(obc = recode(obc, "1=1; else=0"))

cces19 <- cces19 %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces19 <- cces19 %>%
  mutate(fmc = recode(cces19$religpew_methodist, "2=1; else=0")) 

cces19 <- cces19 %>%
  mutate(omc = recode(cces19$religpew_methodist, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces19 <- cces19 %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces19 <- cces19 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces19 <- cces19 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces19 <- cces19 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces19 <- cces19 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces19 <- cces19 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces19 <- cces19 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces19 <- cces19 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces19 <- cces19 %>% 
  mutate(abc = recode(cces19$religpew_baptist, "2=1; 4=1; else=0"))

cces19 <- cces19 %>% 
  mutate(epis = recode(cces19$religpew_episcop, "1:90=1; else=0"))

cces19 <- cces19 %>% 
  mutate(luth = recode(cces19$religpew_lutheran, "1=1; 4=1; else=0"))

cces19 <- cces19 %>% 
  mutate(meth = recode(cces19$religpew_methodist, "1=1; 90=1; else=0"))

cces19 <- cces19 %>% 
  mutate(pres = recode(cces19$religpew_presby, "1=1; 90=1; else=0"))

cces19 <- cces19 %>% 
  mutate(cong = recode(cces19$religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces19 <- cces19 %>% 
  mutate(doc = recode(cces19$religpew_protestant, "8=1; else=0"))

cces19 <- cces19 %>% 
  mutate(reform = recode(cces19$religpew_protestant, "11=1; else=0"))

cces19 <- cces19 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces19 <- cces19 %>% 
  mutate(black = recode(race, "2=1; else=0"))

cces19 <- cces19 %>% 
  mutate(meth = recode(cces19$religpew_methodist, "3:4=1; else=0"))

cces19 <- cces19 %>%
  mutate(sbc = recode(cces19$religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces19 <- cces19 %>% 
  mutate(nbap = recode(cces19$religpew_baptist, "3=1; else=0"))

cces19 <- cces19 %>%
  mutate(abc = recode(cces19$religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces19 <- cces19 %>%
  mutate(miss = recode(cces19$religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces19 <- cces19 %>%
  mutate(obap = recode(cces19$religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces19 <- cces19 %>%
  mutate(ometh = recode(cces19$religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces19 <- cces19 %>% 
  mutate(apos = recode(cces19$religpew_pentecost, "6=1; 7=1; else=0"))

cces19 <- cces19 %>%
  mutate(open = recode(cces19$religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces19 <- cces19 %>%
  mutate(holy = recode(cces19$religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces19 <- cces19 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces19 <- cces19 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

cces19 <- cces19 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

cces19 <- cces19 %>% 
  mutate(other = recode(religpew, "3=1; 6:8=1; 12=1; else=0"))

cces19 <- cces19 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))





## Making Trads ####

cces19 <- cces19 %>% 
  mutate(trad = frcode(evangelical == 1 & race == 1 ~ "White\nEvangelical",
                       evangelical == 1 & race != 1 ~ "Non-White\nEvangelical",
                       mainline == 1 ~ "Mainline",
                       religpew == 1 & race == 2 ~ "Black\nProtestant",
                       catholic == 1 & race == 1 ~ "White\nCatholic",
                       catholic == 1 & race != 1 ~ "Non-White\nCatholic",
                       religpew == 3 ~ "Mormon",
                       religpew == 4 ~ "Orthodox",
                       religpew == 5 ~ "Jewish",
                       religpew == 6 ~ "Muslim",
                       religpew == 7 ~ "Buddhist",
                       religpew == 8 ~ "Hindu",
                       religpew == 9 ~ "Atheist",
                       religpew == 10 ~ "Agnostic",
                       religpew == 11 ~ "Nothing in Particular",
                       TRUE ~ "Unclassified"))  

