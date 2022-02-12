
library(socsci) # remotes::install_github("ryanburge/socsci")
library(car)

## Read your data in as cces12
# cces12 <- read_dta("D://cces/data/cces12.dta")

cces12 <- cces12 %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else=0"))

## Baptist

cces12 <- cces12 %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = sbc - black) %>% 
  mutate(sbc = recode(sbc, "1=1; else=0"))

cces12 <- cces12 %>%
  mutate(ibc = recode(religpew_baptist, "5=1; else=0")) 

cces12 <- cces12 %>%
  mutate(bgc = recode(religpew_baptist, "6=1; else=0")) 

cces12 <- cces12 %>%
  mutate(mbc = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = mbc - black) %>% 
  mutate(mbc = recode(mbc, "1=1; else=0"))

cces12 <- cces12 %>%
  mutate(cb = recode(religpew_baptist, "8=1; else=0")) 

cces12 <- cces12 %>%
  mutate(fwb = recode(religpew_baptist, "9=1; else=0")) 

cces12 <- cces12 %>%
  mutate(gabb = recode(religpew_baptist, "10=1; else=0")) 

cces12 <- cces12 %>%
  mutate(obc = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = obc - black) %>% 
  mutate(obc = recode(obc, "1=1; else=0"))

cces12 <- cces12 %>% 
  mutate(evanbap = sbc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces12 <- cces12 %>%
  mutate(fmc = recode(religpew_methodist, "2=1; else=0")) 

cces12 <- cces12 %>% 
  mutate(evanmeth = fmc)

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
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0")) %>% 
  mutate(evanpent = evanpent - black) %>% 
  mutate(evanpent = recode(evanpent, "1=1; else=0"))

## Episcopal 

## Christian #### 
cces12 <- cces12 %>% 
  mutate(evanxtn = recode(religpew_christian, "1=1; else = 0"))

## None

## Congregregational

cces12 <- cces12 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces12 <- cces12 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0")) %>% 
  mutate(evanholy = evanholy - black) %>% 
  mutate(evanholy = recode(evanholy, "1=1; else=0"))

## Advent
## None 

## Totaling Up

cces12 <- cces12 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evanxtn + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces12 <- cces12 %>% 
  mutate(abc = recode(religpew_baptist, "2=1; 4=1; else=0")) %>% 
  mutate(black = case_when(race == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate(abc = case_when(abc == 1 & black != 1 ~ 1, TRUE ~ 0)) 

cces12 <- cces12 %>% 
  mutate(epis = recode(religpew_episcop, "1:90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(luth = recode(religpew_lutheran, "1=1; 4=1; else=0"))

cces12 <- cces12 %>% 
  mutate(meth = recode(religpew_methodist, "1=1; 90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(pres = recode(religpew_presby, "1=1; 90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(cong = recode(religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(doc = recode(religpew_christian, "2:90=1; else=0"))

cces12 <- cces12 %>% 
  mutate(reform = recode(religpew_protestant, "11=1; else=0"))

cces12 <- cces12 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 


cces12 <- cces12 %>% 
  mutate(meth = recode(religpew_methodist, "3:4=1; else=0"))

cces12 <- cces12 %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(nbap = recode(religpew_baptist, "3=1; else=0"))

cces12 <- cces12 %>%
  mutate(abc = recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(miss = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(obap = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(ometh = recode(religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces12 <- cces12 %>% 
  mutate(apos = recode(religpew_pentecost, "6=1; 7=1; else=0"))

cces12 <- cces12 %>%
  mutate(open = recode(religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces12 <- cces12 %>%
  mutate(holy = recode(religpew_holiness, "90=1; else=0")) %>% 
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
  mutate(other = recode(religpew, "3:4=1; 6:8=1; 12=1; else=0"))

cces12 <- cces12 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))
