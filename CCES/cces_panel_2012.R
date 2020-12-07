

cces <- cces %>% 
  mutate(white = recode(race_12, "1=1; else=0")) %>% 
  mutate(black = recode(race_12, "2=1; else=0"))

## baptist_12


cces <- cces %>%
  mutate(sbc = recode(religpew_baptist_12, "1=1; else=0")) %>% 
  mutate(sbc = sbc - black) %>% 
  mutate(sbc = recode(sbc, "1=1; else=0"))

cces <- cces %>%
  mutate(abc = recode(religpew_baptist_12, "2=1; else=0")) %>% 
  mutate(abc = abc - black) %>% 
  mutate(abc = recode(abc, "1=1; else=0"))

cces <- cces %>%
  mutate(ibc = recode(religpew_baptist_12, "5=1; else=0")) 

cces <- cces %>%
  mutate(bgc = recode(religpew_baptist_12, "6=1; else=0")) 

cces <- cces %>%
  mutate(mbc = recode(religpew_baptist_12, "7=1; else=0")) %>% 
  mutate(mbc = mbc - black) %>% 
  mutate(mbc = recode(mbc, "1=1; else=0"))

cces <- cces %>%
  mutate(cb = recode(religpew_baptist_12, "8=1; else=0")) 

cces <- cces %>%
  mutate(fwb = recode(religpew_baptist_12, "9=1; else=0")) 

cces <- cces %>%
  mutate(gabb = recode(religpew_baptist_12, "10=1; else=0")) 

cces <- cces %>%
  mutate(obc = recode(religpew_baptist_12, "90=1; else=0")) %>% 
  mutate(obc = obc - black) %>% 
  mutate(obc = recode(obc, "1=1; else=0"))

cces <- cces %>% 
  mutate(evanbap = sbc + abc + ibc + bgc + mbc + cb + fwb + gabb + obc)

## methodist_12
cces <- cces %>%
  mutate(fmc = recode(cces$religpew_methodist_12, "2=1; else=0")) 

cces <- cces %>%
  mutate(omc = recode(cces$religpew_methodist_12, "90=1; else=0")) %>% 
  mutate(omc = white + omc) %>% 
  mutate(omc = recode(omc, "2=1; else=0"))

cces <- cces %>% 
  mutate(evanmeth = fmc + omc)

##Non-Denom

cces <- cces %>% 
  mutate(hiatt = recode(pew_churatd_12, "1:3=1; else=0")) %>% 
  mutate(nd = recode(religpew_nondenom_12, "1:90=1; else=0")) %>% 
  mutate(evannd = nd + hiatt) %>% 
  mutate(evannd =  recode(evannd, "2=1; else=0"))

## Lutheran 

cces <- cces %>% 
  mutate(mz = recode(religpew_lutheran_12, "2=1; else=0")) %>% 
  mutate(wi = recode(religpew_lutheran_12, "3=1; else=0")) %>% 
  mutate(evanluth = mz + wi)

## Presbyterian

cces <- cces %>% 
  mutate(pca = recode(religpew_presby_12, "2=1; else=0")) %>% 
  mutate(epc = recode(religpew_presby_12, "6=1; else=0")) %>% 
  mutate(evanpres = pca + epc)

## Pentecostal 

cces <- cces %>% 
  mutate(evanpent = recode(religpew_pentecost_12, "1:90 =1; else=0"))

## Episcopal 
## None

## Congregregational

cces <- cces %>% 
  mutate(evancong = recode(religpew_congreg_12, "2=1; else=0"))

## Holiness
cces <- cces %>% 
  mutate(evanholy = recode(religpew_holiness_12, "1:90 =1; else=0"))

## Advent
## None 

## Totaling Up

cces <- cces %>% 
  mutate(evangelical = evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evancong + evanholy) %>% 
  mutate(evangelical_12 = recode(evangelical, "1:4=1; else=0"))

## Making Mainline

cces <- cces %>% 
  mutate(abc = recode(cces$religpew_baptist_12, "2=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(epis = recode(cces$religpew_episcop_12, "1:90=1; else=0"))

cces <- cces %>% 
  mutate(luth = recode(cces$religpew_lutheran_12, "1=1; 4=1; else=0"))

cces <- cces %>% 
  mutate(meth = recode(cces$religpew_methodist_12, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(pres = recode(cces$religpew_presby_12, "1=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(cong = recode(cces$religpew_congreg_12, "1=1; 3=1; 90=1; else=0"))

cces <- cces %>% 
  mutate(doc = recode(cces$religpew_protestant_12, "8=1; else=0"))

cces <- cces %>% 
  mutate(reform = recode(cces$religpew_protestant_12, "11=1; else=0"))

cces <- cces %>% 
  mutate(mainline = abc + epis + luth + meth + pres + cong + doc + reform) %>% 
  mutate(mainline_12 = recode(mainline, "1:5=1; else=0"))

## Black Protestant 

cces <- cces %>% 
  mutate(black = recode(race_12, "2=1; else=0"))

cces <- cces %>% 
  mutate(meth = recode(cces$religpew_methodist_12, "3:4=1; else=0"))

cces <- cces %>%
  mutate(sbc = recode(cces$religpew_baptist_12, "1=1; else=0")) %>% 
  mutate(sbc = black + sbc) %>% 
  mutate(sbc = recode(sbc, "2=1; else=0"))

cces <- cces %>% 
  mutate(nbap = recode(cces$religpew_baptist_12, "3=1; else=0"))

cces <- cces %>%
  mutate(abc = recode(cces$religpew_baptist_12, "2=1; else=0")) %>% 
  mutate(abc = black + abc) %>% 
  mutate(abc = recode(abc, "2=1; else=0"))

cces <- cces %>%
  mutate(miss = recode(cces$religpew_baptist_12, "7=1; else=0")) %>% 
  mutate(miss = black + miss) %>% 
  mutate(miss = recode(miss, "2=1; else=0"))

cces <- cces %>%
  mutate(obap = recode(cces$religpew_baptist_12, "90=1; else=0")) %>% 
  mutate(obap = black + obap) %>% 
  mutate(obap = recode(obap, "2=1; else=0"))

cces <- cces %>%
  mutate(ometh = recode(cces$religpew_methodist_12, "90=1; else=0")) %>% 
  mutate(ometh = black + ometh) %>% 
  mutate(ometh = recode(ometh, "2=1; else=0"))

cces <- cces %>% 
  mutate(apos = recode(cces$religpew_pentecost_12, "6=1; 7=1; else=0"))

cces <- cces %>%
  mutate(open = recode(cces$religpew_pentecost_12, "90=1; else=0")) %>% 
  mutate(open = black + open) %>% 
  mutate(open = recode(open, "2=1; else=0"))

cces <- cces %>%
  mutate(holy = recode(cces$religpew_holiness_12, "90=1; else=0")) %>% 
  mutate(holy = black + holy) %>% 
  mutate(holy = recode(holy, "2=1; else=0"))


cces <- cces %>% 
  mutate(bprot = meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) %>% 
  mutate(bprot_12 = recode(bprot, "1:2=1; else=0"))

## Everything Else

cces <- cces %>% 
  mutate(catholic_12 = recode(religpew_12, "2=1; else=0"))

cces <- cces %>% 
  mutate(jewish_12 = recode(religpew_12, "5=1; else=0"))

cces <- cces %>% 
  mutate(other_12 = recode(religpew_12, "3=1; 6:8=1; 12=1; else=0"))

cces <- cces %>% 
  mutate(none_12 = recode(religpew_12, "9:11=1; else=0"))


cces <- cces %>% 
  mutate(reltrad_12 = frcode(evangelical_12 == 1 ~ "Evangelical",
                             mainline_12 == 1 ~ "Mainline",
                             bprot_12 == 1 ~ "Black Prot.",
                             catholic_12 == 1 ~ "Catholic",
                             jewish_12 == 1 ~ "Jewish",
                             other_12 == 1 ~ "Other",
                             none_12 == 1 ~ "None"))


