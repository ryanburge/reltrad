library(tidyverse)

cces_trad <- function(df, var){
  var_name <- deparse(substitute(var))
  df %>%
    mutate(trad2 = frcode(
      evangelical == 1 & race == 1 ~ "White Evangelical",
      evangelical == 1 & race != 1 ~ "Non-White Evangelical",
      mainline == 1 ~ "Mainline",
      .data[[var_name]] == 1 & race == 2 ~ "Black Protestant",
      catholic == 1 & race == 1 ~ "White Catholic",
      catholic == 1 & race != 1 ~ "Non-White Catholic",
      .data[[var_name]] == 3 ~ "LDS",
      .data[[var_name]] == 4 ~ "Orthodox",
      .data[[var_name]] == 5 ~ "Jewish",
      .data[[var_name]] == 6 ~ "Muslim",
      .data[[var_name]] == 7 ~ "Buddhist",
      .data[[var_name]] == 8 ~ "Hindu",
      .data[[var_name]] == 9 ~ "Atheist",
      .data[[var_name]] == 10 ~ "Agnostic",
      .data[[var_name]] == 11 ~ "Nothing in Particular",
      TRUE ~ "Unclassified"))
}

# Race indicators
cces17 <- cces17 %>%
  mutate(
    white = case_when(race == 1 ~ 1, TRUE ~ 0),
    black = case_when(race == 2 ~ 1, TRUE ~ 0)
  )

## Baptist
cces17 <- cces17 %>%
  mutate(
    sbc   = case_when(religpew_baptist == 1 & black != 1 ~ 1, TRUE ~ 0),
    ibc   = case_when(religpew_baptist == 5 ~ 1, TRUE ~ 0),
    bgc   = case_when(religpew_baptist == 6 ~ 1, TRUE ~ 0),
    mbc   = case_when(religpew_baptist == 7 & black != 1 ~ 1, TRUE ~ 0),
    cb    = case_when(religpew_baptist == 8 ~ 1, TRUE ~ 0),
    fwb   = case_when(religpew_baptist == 9 ~ 1, TRUE ~ 0),
    gabb  = case_when(religpew_baptist == 10 ~ 1, TRUE ~ 0),
    obc   = case_when(religpew_baptist == 90 & black != 1 ~ 1, TRUE ~ 0),
    evanbap = case_when((sbc + ibc + bgc + mbc + cb + fwb + gabb + obc) >= 1 ~ 1, TRUE ~ 0)
  )

## Methodist
cces17 <- cces17 %>%
  mutate(
    fmc     = case_when(religpew_methodist == 2 ~ 1, TRUE ~ 0),
    evanmeth = fmc
  )

## Non-Denom
cces17 <- cces17 %>%
  mutate(
    hiatt  = case_when(pew_churatd %in% 1:3 ~ 1, TRUE ~ 0),
    nd     = case_when(religpew_nondenom %in% 1:90 ~ 1, TRUE ~ 0),
    evannd = case_when(nd == 1 & hiatt == 1 ~ 1, TRUE ~ 0)
  )

## Lutheran
cces17 <- cces17 %>%
  mutate(
    mz       = case_when(religpew_lutheran == 2 ~ 1, TRUE ~ 0),
    wi       = case_when(religpew_lutheran == 3 ~ 1, TRUE ~ 0),
    evanluth = case_when((mz + wi) >= 1 ~ 1, TRUE ~ 0)
  )

## Presbyterian
cces17 <- cces17 %>%
  mutate(
    pca      = case_when(religpew_presby == 2 ~ 1, TRUE ~ 0),
    epc      = case_when(religpew_presby == 6 ~ 1, TRUE ~ 0),
    evanpres = case_when((pca + epc) >= 1 ~ 1, TRUE ~ 0)
  )

## Pentecostal
cces17 <- cces17 %>%
  mutate(
    evanpent = case_when(religpew_pentecost %in% 1:90 & black != 1 ~ 1, TRUE ~ 0)
  )

## Christian
cces17 <- cces17 %>%
  mutate(
    evanxtn = case_when(religpew_christian == 1 ~ 1, TRUE ~ 0)
  )

## Congregational
cces17 <- cces17 %>%
  mutate(
    evancong = case_when(religpew_congreg == 2 ~ 1, TRUE ~ 0)
  )

## Holiness
cces17 <- cces17 %>%
  mutate(
    evanholy = case_when(religpew_holiness %in% 1:90 & black != 1 ~ 1, TRUE ~ 0)
  )

## Total Evangelical
cces17 <- cces17 %>%
  mutate(
    evangelical = case_when(
      (evanbap + evanmeth + evannd + evanluth + evanpres + evanpent + evanxtn + evancong + evanholy) >= 1 ~ 1,
      TRUE ~ 0
    )
  )

## Mainline Protestant
cces17 <- cces17 %>%
  mutate(
    abc    = case_when(religpew_baptist %in% c(2, 4) & black != 1 ~ 1, TRUE ~ 0),
    epis   = case_when(religpew_episcop %in% 1:90 ~ 1, TRUE ~ 0),
    luth   = case_when(religpew_lutheran %in% c(1, 4) ~ 1, TRUE ~ 0),
    meth   = case_when(religpew_methodist %in% c(1, 90) ~ 1, TRUE ~ 0),
    pres   = case_when(religpew_presby %in% c(1, 90) ~ 1, TRUE ~ 0),
    cong   = case_when(religpew_congreg %in% c(1, 3, 90) ~ 1, TRUE ~ 0),
    doc    = case_when(religpew_christian %in% 2:90 ~ 1, TRUE ~ 0),
    reform = case_when(religpew_protestant == 11 ~ 1, TRUE ~ 0),
    mainline = case_when(
      (abc + epis + luth + meth + pres + cong + doc + reform) >= 1 ~ 1,
      TRUE ~ 0
    )
  )

## Black Protestant
cces17 <- cces17 %>%
  mutate(
    meth  = case_when(religpew_methodist %in% 3:4 ~ 1, TRUE ~ 0),
    sbc   = case_when(religpew_baptist == 1 & black == 1 ~ 1, TRUE ~ 0),
    nbap  = case_when(religpew_baptist == 3 ~ 1, TRUE ~ 0),
    abc   = case_when(religpew_baptist == 2 & black == 1 ~ 1, TRUE ~ 0),
    miss  = case_when(religpew_baptist == 7 & black == 1 ~ 1, TRUE ~ 0),
    obap  = case_when(religpew_baptist == 90 & black == 1 ~ 1, TRUE ~ 0),
    ometh = case_when(religpew_methodist == 90 & black == 1 ~ 1, TRUE ~ 0),
    apos  = case_when(religpew_pentecost %in% c(6, 7) ~ 1, TRUE ~ 0),
    open  = case_when(religpew_pentecost == 90 & black == 1 ~ 1, TRUE ~ 0),
    holy  = case_when(religpew_holiness == 90 & black == 1 ~ 1, TRUE ~ 0),
    bprot = case_when(
      (meth + sbc + nbap + abc + miss + obap + ometh + apos + open + holy) >= 1 ~ 1,
      TRUE ~ 0
    )
  )

## Other traditions
cces17 <- cces17 %>%
  mutate(
    catholic = case_when(religpew == 2 ~ 1, TRUE ~ 0),
    jewish   = case_when(religpew == 5 ~ 1, TRUE ~ 0),
    other    = case_when(religpew %in% c(3, 4, 6, 7, 8, 12) ~ 1, TRUE ~ 0),
    none     = case_when(religpew %in% 9:11 ~ 1, TRUE ~ 0)
  )

cces17 <- cces_trad(cces17, religpew)
