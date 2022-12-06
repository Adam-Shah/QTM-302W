library(tidyverse)






gss_cat %>% count(marital) %>% ggplot(aes(x = n, y = marital)) +
  geom_point()

gss_cat %>% count(marital) %>% ggplot(aes(x = n, y = fct_reorder(marital, n))) + geom_point()

ggplot(relig_summary, 
       aes(x = tvhours, 
           y = fct_relevel(relig, "Don't know", after=0))) +
  geom_point()

ggplot(relig_summary, 
       aes(x=tvhours, 
           y = fct_reorder(relig, tvhours) %>%
             fct_relevel("Don't know", after=0))) + 
  geom_point()

gss_cat %>%
  count(partyid) %>%
  ggplot(aes(x=n, 
             y = fct_reorder(partyid, n) %>% 
               fct_relevel("No answer", after=0) %>%
               fct_relevel("Independent", after=1))) + 
  geom_point()

gss_cat %>%
  mutate(marital = fct_infreq(marital) %>% 
           fct_rev()) %>%
  ggplot(aes(marital)) +
  geom_bar()

gss_cat %>% ggplot(aes(race %>% fct_infreq() %>% fct_rev())) + geom_bar()

gss_cat %>% 
  drop_na(tvhours) %>% 
  group_by(partyid) %>%
  summarise(meantv=mean(tvhours)) %>%
  ggplot(aes(x=meantv, y=fct_reorder(partyid, meantv)))+
  geom_point()

gss_cat %>%
  drop_na(tvhours) %>%
  mutate(partyidnew = fct_recode(partyid,
                                 "Republican, strong"    = "Strong republican",
                                 "Republican, weak"      = "Not str republican",
                                 "Independent, near rep" = "Ind,near rep",
                                 "Independent, near dem" = "Ind,near dem",
                                 "Democrat, weak"        = "Not str democrat",
                                 "Democrat, strong"      = "Strong democrat",
                                 "Other"                 = "No answer",
                                 "Other"                 = "Don't know",
                                 "Other"                 = "Other party")) %>%    
  group_by(partyidnew) %>%
  summarise(meantvhours = mean(tvhours)) %>%
  ggplot(aes(x=meantvhours, 
             y=fct_reorder(partyidnew, meantvhours))) +
  geom_point()

gss %>% 
  drop_na(health) %>%
  mutate(health2 = fct_recode(health, 
                              "Other" = "DK", 
                              "Excellent" = "EXCELLENT", 
                              "Not that ok" = "FAIR", 
                              "Good" = "GOOD", 
                              "No idea" = "IAP", 
                              "Not good at all" = "POOR")) %>%
  count(health2)

gss_cat %>%
  mutate(partyidnew = fct_collapse(partyid,
                                   other = c("No answer", "Don't know", "Other party"),
                                   rep = c("Strong republican", "Not str republican"),
                                   ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                   dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyidnew)

gss %>%
  mutate(wrkstatnew = fct_collapse(wrkstat,
                                   Working = c("KEEPING HOUSE", "WORKING FULLTIME", "WORKING PARTTIME"),
                                   `Not Working` = c("TEMP NOT WORKING", "UNEMPL LAID OFF"),
                                   Studying = c("SCHOOL"),
                                   Other = c("OTHER", "RETIRED")
  )) %>%
  count(wrkstatnew)

gss_cat %>%
  mutate(relignew = fct_lump(relig, 5)) %>%
  count(relignew)


