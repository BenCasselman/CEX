library(tidyverse)
library(bigvis)

# Aaron Cobet, BLS
# 202-691-5018

download.file("https://www.bls.gov/cex/pumd/data/comma/intrvw18.zip", "intrvw18.zip")
download.file("https://www.bls.gov/cex/pumd/data/comma/diary18.zip", "diary18.zip")

fmld <- list("diary18/fmld181.csv",
  "diary18/fmld182.csv",
  "diary18/fmld183.csv",
  "diary18/fmld184.csv") %>% 
  map_dfr(~read_csv(unz("diary18.zip", .x)) %>% 
            mutate_all(as.character))

fmld

fmli <- list("intrvw18/fmli181x.csv",
             "intrvw18/fmli182.csv",
             "intrvw18/fmli183.csv",
             "intrvw18/fmli184.csv",
             "intrvw18/fmli191.csv") %>% 
  map_dfr(~read_csv(unz("intrvw18.zip", .x)) %>% 
            mutate_all(as.character) %>% 
            mutate(quarter = substr(.x, 14, 16)))


fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  mutate(mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                              QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                              QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                              QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                              TRUE ~ 3),
         pop = (as.numeric(FINLWT21)/4)*(mo_scope/3),
         tot_shelter = as.numeric(SHELTPQ) + as.numeric(SHELTCQ)) %>% 
  summarize(cq = sum(as.numeric(SHELTCQ)*as.numeric(FINLWT21))/sum(pop),
            pq = sum(as.numeric(SHELTPQ)*as.numeric(FINLWT21))/sum(pop),
            tot = sum(tot_shelter * as.numeric(FINLWT21)/sum(pop)),
            pop = sum(pop))

fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  mutate(tot_shelter = as.numeric(SHELTPQ) + as.numeric(SHELTCQ),
         tot_food = as.numeric(FDHOMEPQ)) %>% 
  summarize(avg = weighted.mean(tot_shelter, as.numeric(FINLWT21))*4,
            fd = weighted.mean(tot_food, as.numeric(FINLWT21)) * 4,
            pop = sum(as.numeric(FINLWT21)) / 5)


fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  mutate(shelt1 = case_when(as.numeric(SHELTCQ) > 0 ~ as.numeric(SHELTCQ)),
         shelt2 = case_when(as.numeric(SHELTPQ) > 0 ~ as.numeric(SHELTPQ))) %>% 
  summarize(avg = weighted.mean(shelt1, as.numeric(FINLWT21), na.rm = T)*4 +
              weighted.mean(shelt2, as.numeric(FINLWT21), na.rm = T)*4)

fmli %>% 
  mutate(zero = case_when(as.numeric(SHELTCQ) == 0 ~ "zero",
                          TRUE ~ "not zero")) %>% 

fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  select(SHELTPQ, SHELTCQ)

fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  mutate(zero = case_when(as.numeric(SHELTCQ) == 0 ~ "zero",
                          TRUE ~ "not zero")) %>% 
  group_by(zero) %>% 
  summarize(sum(as.numeric(FINLWT21)))

fmli %>% 
  filter(as.numeric(AGE_REF) %in% 25:54) %>% 
  mutate(zero = case_when(as.numeric(SHELTCQ) == 0 ~ "zero",
                          TRUE ~ "not zero")) %>% 
  group_by(zero) %>% 
  summarize(sum(as.numeric(FINLWT21)))






fmli %>% 
  filter(quarter == "181") %>% 
  summarize(sum(as.numeric(FINLWT21)))


fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  rename(cq = SHELTPQ, pq = SHELTCQ) %>% 
  mutate(mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                              QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                              QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                              QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                              TRUE ~ 3),
         pop = (as.numeric(FINLWT21)/4)*(mo_scope/3),
         tot_shelter = as.numeric(cq) + as.numeric(pq)) %>% 
  summarize(cq = sum(as.numeric(cq)*as.numeric(FINLWT21))/sum(pop),
            pq = sum(as.numeric(pq)*as.numeric(FINLWT21))/sum(pop),
            tot = sum(tot_shelter * as.numeric(FINLWT21)/sum(pop)),
            pop = sum(pop))


fmli %>% 
  # filter(as.numeric(AGE_REF) < 25,
  #        quarter == "191") %>% 
  rename(cq = SHELTCQ, pq = SHELTPQ) %>% 
  mutate(mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                              QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                              QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                              QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                              TRUE ~ 3),
         pop = (as.numeric(FINLWT21))*(mo_scope/3),
         tot_shelter = as.numeric(cq) + as.numeric(pq)) %>% 
  summarize(cq = 4*sum(as.numeric(cq)*as.numeric(FINLWT21))/sum(pop),
            pq = 4*sum(as.numeric(pq)*as.numeric(FINLWT21))/sum(pop),
            tot = 4*sum(tot_shelter * as.numeric(FINLWT21)/sum(pop)),
            pop = sum(pop))


fmld %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  mutate(weight = as.numeric(FINLWT21)) %>% 
  summarize(52*sum(as.numeric(FOODHOME) * weight)/sum(weight))


mtbi_1 <- read_csv(unz("intrvw18.zip", "intrvw18/mtbi181x.csv"))
         

mtbi_all <- list("intrvw18/mtbi181x.csv",
                 "intrvw18/mtbi182.csv",
                 "intrvw18/mtbi183.csv",
                 "intrvw18/mtbi184.csv",
                 "intrvw18/mtbi191.csv") %>% 
  map_dfr(~read_csv(unz("intrvw18.zip", .x)))


shelter_1 <- mtbi_1 %>% 
  filter(UCC %in% c("220311", "220313", "880110", "220321", "220211", "220121", "220111", "220121", "210901", "320625", "230112", "230113", "230114", "230115", "230151", "230122", "230142", "240112", "240122", "240312", "240322", "320622", "240213", "240212", "240222", "320632", "320612", "990930", "230901", "340911", "220901", "210110", "800710", "350110", "320624", "230150", "230121", "230141", "240111", "240121", "240211", "240221", "240311", "240321", "320611", "990910", "990920", "790690", "320621", "320631", "220312", "220314", "880310", "220322", "220212", "320626", "220122", "220112", "220122", "210902", "230152", "230123", "240113", "240123", "240214", "240223", "240313", "240323", "320613", "990940", "320623", "320633", "230902", "340912", "220902", "210310", "210210")) %>% 
  group_by(NEWID) %>% 
  summarize(shelter = sum(COST))

fmli %>% 
  filter(quarter == "181") %>% 
  mutate(weight = as.numeric(FINLWT21)) %>% 
  left_join(shelter_1, by = "NEWID") %>% 
  summarize(calc = 4*sum(shelter*weight, na.rm = T)/sum(weight),
            pre_calc = 4*sum(as.numeric(SHELTPQ)*weight, na.rm = T)/sum(weight),
            pop = sum(weight))

fmli %>% 
  filter(quarter == "181",
         as.numeric(AGE_REF) < 25) %>% 
  mutate(weight = as.numeric(FINLWT21)) %>% 
  left_join(shelter_1, by = "NEWID") %>% 
  mutate(zero = case_when(is.na(shelter) ~ "zero",
                        TRUE ~ "not zero")) %>% 
  group_by(zero) %>% 
  summarize(total = sum(weight)) %>% 
  mutate(share = total/sum(total))


fmli %>% 
  filter(quarter == "181",
         as.numeric(AGE_REF) < 25) %>% 
  mutate(weight = as.numeric(FINLWT21)) %>% 
  left_join(shelter_1, by = "NEWID") %>% 
  group_by(zero) %>% 
  summarize(total = sum(weight)) %>% 
  mutate(share = total/sum(total))

summarize(total = 4*sum(shelter*weight, na.rm = T)/sum(weight),
          pop = sum(weight))


fmli %>% 
  filter(quarter == "181") %>% 
  mutate(weight = as.numeric(FINLWT21),
         tot_shelt = as.numeric(SHELTPQ) + as.numeric(SHELTCQ)) %>% 
  left_join(shelter_1, by = "NEWID") %>% 
  select(NEWID, QINTRVMO, shelter, tot_shelt) %>% 
  mutate(diff = shelter - tot_shelt) %>% 
  arrange(desc(abs(diff)))





fmli %>% 
  # filter(as.numeric(AGE_REF) < 25) %>%
  rename(cq = SHELTPQ, pq = SHELTCQ) %>% 
  mutate(mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                              QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                              QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                              QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                              TRUE ~ 3),
         pop = (as.numeric(FINLWT21)/4)*(mo_scope/3),
         tot_shelter = (as.numeric(cq) + as.numeric(pq)) * (mo_scope/3)) %>% 
  summarize(tot = sum(tot_shelter * as.numeric(FINLWT21)/sum(pop)),
            pop = sum(pop))



rent <- mtbi_all %>% 
  filter(UCC == 210110) %>% 
  group_by(NEWID) %>% 
  summarize(rent = sum(COST))

fmli %>% 
  filter(as.numeric(AGE_REF) < 25) %>% 
  left_join(rent, 
            by = "NEWID") %>% 
  mutate(weight = as.numeric(FINLWT21),
         mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                              QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                              QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                              QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                              QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                              TRUE ~ 3),
         pop = (weight/4)*(mo_scope/3),
         rent_all = case_when(!is.na(rent) ~ rent * (mo_scope/3),
                              TRUE ~ 0)) %>% 
  summarize(rent = sum(rent * weight/sum(pop), na.rm = T),
            rent_all = sum(rent_all * weight/sum(pop)),
            pop = sum(pop))
  
  summarize(calc = 4*sum(shelter*weight, na.rm = T)/sum(weight),
            pre_calc = 4*sum(as.numeric(SHELTPQ)*weight, na.rm = T)/sum(weight),
            pop = sum(weight))


  
  
  fmli %>% 
    filter(as.numeric(AGE_REF) < 25) %>%
    mutate(weight = as.numeric(FINLWT21),
           mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                                QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                                QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                                QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                                QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                                QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                                TRUE ~ 3),
           pop = (weight/4)*(mo_scope/3),
           income = as.numeric(FINCBTAX) * (mo_scope/3)/4,
           shelter = (as.numeric(SHELTPQ) + as.numeric(SHELTCQ))* (mo_scope/3),
           shelter_share = shelter/income) %>% 
    filter(shelter > 0) %>% 
    summarize(tot = sum(income * weight/sum(pop)),
              shelter = sum(shelter * weight/sum(pop)),
              pop = sum(pop),
              share = weighted.median(shelter_share, weight, na.rm = T))
  
  
  fmli %>% 
    filter(as.numeric(AGE_REF) < 25) %>% 
    left_join(rent, 
              by = "NEWID") %>% 
    mutate(weight = as.numeric(FINLWT21),
           mo_scope = case_when(QINTRVYR == "2018" & QINTRVMO == "01" ~ 0,
                                QINTRVYR == "2018" & QINTRVMO == "02" ~ 1,
                                QINTRVYR == "2018" & QINTRVMO == "03" ~ 2,
                                QINTRVYR == "2019" & QINTRVMO == "01" ~ 3,
                                QINTRVYR == "2019" & QINTRVMO == "02" ~ 2,
                                QINTRVYR == "2019" & QINTRVMO == "03" ~ 1,
                                TRUE ~ 3),
           pop = (weight/4)*(mo_scope/3),
           income = as.numeric(FINCBTAX) * (mo_scope/3)/4,
           rent_share = rent/income) %>% 
    filter(!is.na(rent)) %>% 
    summarize(income = sum(income * weight/sum(pop)),
              rent = sum(rent * weight/sum(pop), na.rm = T),
              pop = sum(pop),
              share = weighted.median(rent_share, weight, na.rm = T))
  