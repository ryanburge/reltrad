
## This is a close approximation of RELTRAD evangelical in the CCES
## This is for the 2016 CCES file and it's read into R as cces16

cces16$evanbaptist <- Recode(cces16$religpew_baptist, "1=1; 5:90=1; else=0")
cces16$evanmeth <- Recode(cces16$religpew_methodist, "2=1; else=0")
cces16$evannd <- Recode(cces16$religpew_nondenom, "1:90=1; else=0")
cces16$evanluth <- Recode(cces16$religpew_lutheran, "2:3=1; else=0")
cces16$evanpres <- Recode(cces16$religpew_presby, "6=1; else=0")
cces16$pente <- Recode(cces16$religpew_pentecost, "1:90=1; else=0")
cces16$evanchrist <- Recode(cces16$religpew_christian, "1=1; 3:4=1; else=0")
cces16$evancong <- Recode(cces16$religpew_congreg, "2=1; else=0")
cces16$evanholy <- Recode(cces16$religpew_holiness, "1:90=1; else=0")
cces16$evanadvent <- Recode(cces16$religpew_advent, "1:90=1; else=0")

cces16$evangelical <- cces16$evanbaptist + cces16$evanmeth + cces16$evannd + cces16$evanluth + cces16$evanpres + cces16$pente + cces16$evanchrist + cces16$evancong + cces16$evanholy + cces16$evanadvent
cces16$evangelical <- Recode(cces16$evangelical, "1:4=1; else=0")

## Now, there needs to be a racial filter placed on  evangelicals. 
## The variable is 'race' and 1 is white, 2 is black, 3 is hispanic, etc. 
## Traditionally RELTRAD includes all not black individuals in the evangelical category. 


## This is a close approximation of RELTRAD evangelical in the CCES
## This is for the 2012 CCES file and it's read into R as cces12

cces12$evanbaptist <- Recode(cces12$religpew_baptist, "1=1; 5:90=1; else=0")
cces12$evanmeth <- Recode(cces12$religpew_methodist, "2=1; else=0")
cces12$evannd <- Recode(cces12$religpew_nondenom, "1:90=1; else=0")
cces12$evanluth <- Recode(cces12$religpew_lutheran, "2:3=1; else=0")
cces12$evanpres <- Recode(cces12$religpew_presby, "6=1; else=0")
cces12$pente <- Recode(cces12$religpew_pentecost, "1:90=1; else=0")
cces12$evanchrist <- Recode(cces12$religpew_christian, "1=1; 3:4=1; else=0")
cces12$evancong <- Recode(cces12$religpew_congreg, "2=1; else=0")
cces12$evanholy <- Recode(cces12$religpew_holiness, "1:90=1; else=0")
cces12$evanadvent <- Recode(cces12$religpew_advent, "1:90=1; else=0")


cces12$evangelical <- cces12$evanbaptist + cces12$evanmeth + cces12$evannd + cces12$evanluth + cces12$evanpres + cces12$pente + cces12$evanchrist + cces12$evancong + cces12$evanholy + cces12$evanadvent
cces12$evangelical <- Recode(cces12$evangelical, "1:4=1; else=0")

## Now, there needs to be a racial filter placed on  evangelicals. 
## The variable is 'race' and 1 is white, 2 is black, 3 is hispanic, etc. 
## Traditionally RELTRAD includes all not black individuals in the evangelical category. 

## This is a close approximation of RELTRAD evangelical in the CCES
## This is for the 2008 CCES file and it's read into R as cces08

cces08$evanbaptist <- Recode(cces08$V222, "1=1; 5:90=1; else=0")
cces08$evanmeth <- Recode(cces08$V223, "2=1; else=0")
cces08$evannd <- Recode(cces08$V224, "1:90=1; else=0")
cces08$evanluth <- Recode(cces08$V225, "2:3=1; else=0")
cces08$evanpres <- Recode(cces08$V226, "6=1; else=0")
cces08$pente <- Recode(cces08$V227, "1:90=1; else=0")
cces08$evanchrist <- Recode(cces08$V229, "1=1; 3:4=1; else=0")
cces08$evancong <- Recode(cces08$V230, "2=1; else=0")
cces08$evanholy <- Recode(cces08$V231, "1:90=1; else=0")
cces08$evanreform <- Recode(cces08$V232, "2=1; else=0")
cces08$evanadvent <- Recode(cces08$V233, "1:90=1; else=0")

cces08$evangelical <- cces08$evanbaptist + cces08$evanmeth + cces08$evannd + cces08$evanluth + cces08$evanpres + cces08$pente + cces08$evanchrist + cces08$evancong + cces08$evanholy + cces08$evanadvent + cces08$evanreform
cces08$evangelical <- Recode(cces08$evangelical, "1:4=1; else=0")


## Now, there needs to be a racial filter placed on  evangelicals. 
## The variable is 'V211' and 1 is white, 2 is black, 3 is hispanic, etc. 
## Traditionally RELTRAD includes all not black individuals in the evangelical category. 
