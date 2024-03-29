
library(socsci) # remotes::install_github("ryanburge/socsci")
library(car)

## Read your data in as cces18

# cces18 <- read_dta("D://cces/data/cces18.dta")

cces18 <- cces18 %>% 
  mutate(white = recode(race, "1=1; else=0")) %>% 
  mutate(black = recode(race, "2=1; else=0"))

## Baptist

cces18 <- cces18 %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = sbc - black) %>% 
  mutate(sbc = recode(sbc, "1=1; else=0"))

cces18 <- cces18 %>%
  mutate(ibc = recode(religpew_baptist, "5=1; else=0")) 

cces18 <- cces18 %>%
  mutate(bgc = recode(religpew_baptist, "6=1; else=0")) 

cces18 <- cces18 %>%
  mutate(mbc = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(mbc = mbc - black) %>% 
  mutate(mbc = recode(mbc, "1=1; else=0"))

cces18 <- cces18 %>%
  mutate(cb = recode(religpew_baptist, "8=1; else=0")) 

cces18 <- cces18 %>%
  mutate(fwb = recode(religpew_baptist, "9=1; else=0")) 

cces18 <- cces18 %>%
  mutate(gabb = recode(religpew_baptist, "10=1; else=0")) 

cces18 <- cces18 %>%
  mutate(obc = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obc = obc - black) %>% 
  mutate(obc = recode(obc, "1=1; else=0"))

cces18 <- cces18 %>% 
  mutate(evanbap = sbc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## Methodist
cces18 <- cces18 %>%
  mutate(fmc = recode(religpew_methodist, "2=1; else=0")) 

cces18 <- cces18 %>% 
  mutate(evanmeth = fmc)

##Non-Denom

cces18 <- cces18 %>% 
  mutate(hiatt = recode(pew_churatd, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces18 <- cces18 %>% 
  mutate(mz = recode(religpew_lutheran, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces18 <- cces18 %>% 
  mutate(pca = recode(religpew_presby, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces18 <- cces18 %>% 
  mutate(evanpent = recode(religpew_pentecost, "1:90 =1; else=0")) %>% 
  mutate(evanpent = evanpent - black) %>% 
  mutate(evanpent = recode(evanpent, "1=1; else=0"))

## Episcopal 

## Christian #### 
cces18 <- cces18 %>% 
  mutate(evanxtn = recode(religpew_christian, "1=1; else = 0"))

## None

## Congregregational

cces18 <- cces18 %>% 
  mutate(evancong = recode(religpew_congreg, "2=1; else=0"))

## Holiness
cces18 <- cces18 %>% 
  mutate(evanholy = recode(religpew_holiness, "1:90 =1; else=0")) %>% 
  mutate(evanholy = evanholy - black) %>% 
  mutate(evanholy = recode(evanholy, "1=1; else=0"))

## Advent
## None 

## Totaling Up

cces18 <- cces18 %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evanxtn + evancong + evanholy) %>% 
  mutate(evangelical = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces18 <- cces18 %>% 
  mutate(abc = recode(religpew_baptist, "2=1; 4=1; else=0")) %>% 
  mutate(black = case_when(race == 2 ~ 1, TRUE ~ 0)) %>% 
  mutate(abc = case_when(abc == 1 & black != 1 ~ 1, TRUE ~ 0)) 

cces18 <- cces18 %>% 
  mutate(epis = recode(religpew_episcop, "1:90=1; else=0"))

cces18 <- cces18 %>% 
  mutate(luth = recode(religpew_lutheran, "1=1; 4=1; else=0"))

cces18 <- cces18 %>% 
  mutate(meth = recode(religpew_methodist, "1=1; 90=1; else=0"))

cces18 <- cces18 %>% 
  mutate(pres = recode(religpew_presby, "1=1; 90=1; else=0"))

cces18 <- cces18 %>% 
  mutate(cong = recode(religpew_congreg, "1=1; 3=1; 90=1; else=0"))

cces18 <- cces18 %>% 
  mutate(doc = recode(religpew_christian, "2:90=1; else=0"))

cces18 <- cces18 %>% 
  mutate(reform = recode(religpew_protestant, "11=1; else=0"))

cces18 <- cces18 %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline = recode(mainline, "1:5=1; else=0"))

## Black Protestant 


cces18 <- cces18 %>% 
  mutate(meth = recode(religpew_methodist, "3:4=1; else=0"))

cces18 <- cces18 %>%
  mutate(sbc = recode(religpew_baptist, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces18 <- cces18 %>% 
  mutate(nbap = recode(religpew_baptist, "3=1; else=0"))

cces18 <- cces18 %>%
  mutate(abc = recode(religpew_baptist, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces18 <- cces18 %>%
  mutate(miss = recode(religpew_baptist, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces18 <- cces18 %>%
  mutate(obap = recode(religpew_baptist, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces18 <- cces18 %>%
  mutate(ometh = recode(religpew_methodist, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces18 <- cces18 %>% 
  mutate(apos = recode(religpew_pentecost, "6=1; 7=1; else=0"))

cces18 <- cces18 %>%
  mutate(open = recode(religpew_pentecost, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces18 <- cces18 %>%
  mutate(holy = recode(religpew_holiness, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces18 <- cces18 %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces18 <- cces18 %>% 
  mutate(catholic = recode(religpew, "2=1; else=0"))

cces18 <- cces18 %>% 
  mutate(jewish = recode(religpew, "5=1; else=0"))

cces18 <- cces18 %>% 
  mutate(other = recode(religpew, "3:4=1; 6:8=1; 12=1; else=0"))

cces18 <- cces18 %>% 
  mutate(none = recode(religpew, "9:11=1; else=0"))

