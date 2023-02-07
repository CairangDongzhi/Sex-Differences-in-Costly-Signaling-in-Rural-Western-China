#
###Code for Sex Differences in Costly Signaling in Rural Western China
# ### Includes code for: 
# #---1. Descriptive Results for Reputation Nomination
# #---2. Data Analyse-Reputation nomination and religious investment
# #---3. Models Separated by Gender
# #---4. Data Analyse-Reputation nomination and religion_Separated by Gender
# #---5. Consensus Analyze
# #---6. Plots

------------------------------------------------------------------------------
#---1. Descriptive Results for Reputation Nomination
library(pscl)
library(RColorBrewer)
library(texreg)
library(sna)
library(statnet)
library(ggnet)
library(ggplot2)
library(sjmisc)
library(magrittr)
library(tidyverse)
library(xtable)
library(igraph)
library(intergraph)
library(tableone)
library(kableExtra)
library(knitr)

### ***First, the data distribution based on household level***

##### Number of households

CB_demographic %>%
  filter(Invillagenow == 1) %>% 
  distinct(HosNum)%>% 
  nrow()
##### Households grouped by economic rank

CB_Households %>% 
  count(Rank1)
##### Households grouped by Four tribes

CB_Households %>% 
  count(WhichTribe)

### ***Second, the data distribution based on individual level***

##### Population size in the village
CB_demographic %>%
  filter(Invillagenow == 1) %>% 
  count(Gender)

##### Population grouped by age cohorts

CB_demographic %>%
  filter(Invillagenow == 1) %>% 
  count(Age_cohort)

##### Gender & Age cohorts

CB_demographic %>%
  filter(Invillagenow == 1) %>% 
  count(Gender,Age_cohort)

##### The distribution of age

CB_demographic %>%
  filter(Invillagenow == 1) %>% 
  ggplot(aes(x = Age))+
  geom_bar()+
  scale_y_continuous(breaks = c(0,3,6,9),labels = c(0,3,6,9) )+
  facet_grid( ~ Gender)


CB_demo_repu_reli%>%
  filter(Invillagenow == 1) %>% 
  group_by(Gender) %>% 
  summarise(mean_relatives = mean(relatives))

CB_demo_repu_reli%>%
  filter(Invillagenow == 1) %>% 
  t.test(relatives ~ Gender, data = .)
##### Mean values of of some basic demographic variables

CB_demographic %>%
  filter(Invillagenow == 1) %>% 
  group_by(Gender) %>% 
  summarise_at(c("Siblinginvillage","Age","Totaloffspring"),mean,na.rm =T)

### *Third, the descriptive statistic of reputation nomination*

#  How many People nominate others

n_distinct(Reputation$ID)

#  How many People being nominated

n_distinct(Reputation$Nominee_ID)

#  How many People being nominated, separated by different types of nomination
Reputation %>% 
  group_nest(Quality) %>% 
  mutate(Count = map_dbl(.$data,~ nrow(.x)),
         Distinct_Count = map_dbl(.$data,~ n_distinct(.x$Nominee_ID))) %>% 
  select(1,3,4) %>% 
  kable()
##### The distribution of the frequency of receiving nominations, faceted by 
##### different reputation qualities and gender.

library(patchwork)

p1 <- CB_demo_repu %>% 
  filter(Age > 18) %>% 
  ggplot(aes(x = Generous)) +
  geom_bar() + 
  xlim(0,25)+
  ylim(0,50)+
  facet_grid( ~ Gender)

p2 <- CB_demo_repu %>% 
  filter(Age > 18) %>% 
  ggplot(aes(x = Devout)) +
  geom_bar() + 
  xlim(0,25)+
  ylim(0,50)+
  facet_grid( ~ Gender)

p3 <- CB_demo_repu %>% 
  filter(Age > 18) %>% 
  ggplot(aes(x = HardWorking)) +
  geom_bar() + 
  xlim(0,25)+
  ylim(0,50)+
  facet_grid( ~ Gender)

p4 <- CB_demo_repu %>% 
  filter(Age > 18) %>% 
  ggplot(aes(x = GoodCharacter)) +
  geom_bar() + 
  xlim(0,25)+
  ylim(0,50)+
  facet_grid( ~ Gender)

p1+p2+p3+p4

##### The distribution of the frequency of nominating others, faceted by 
##### different reputation qualities and gender.

library(patchwork)

Plot_Distribution_times_nominations <- 
  list("Devote","Generous","GoodCharacter","HardWorking") %>% 
  map( ~ get.inducedSubgraph(Reputation_network,eid = which(Reputation_network %e% "Quality" == .x))) %>% 
  map( ~ asIgraph(.x)) %>% 
  map(., ~ degree(.x,mode = "out")) %>%
  map(.,~ tibble(.x)) %>% 
  map2(., list("Devote","Generous","GoodCharacter","HardWorking"),
       ~ ggplot( aes(x=.x),data = .x) +
         geom_bar()+
         xlab(.y))

Plot_Distribution_times_nominations[[1]]+xlab("Devout")+
  Plot_Distribution_times_nominations[[2]]+
  Plot_Distribution_times_nominations[[3]]+
  Plot_Distribution_times_nominations[[4]]

### This (Above) is the Figure S3 in Supplementary Material
##### ***The correlation plot showing the relationship between different nominations.***

# The corrplot of nomination
library(RColorBrewer)

CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(Totalscore) & !is.na(DailyRegularscore)) %>%
  select(14:19) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("Var1") %>% 
  pivot_longer(-Var1,names_to = "Var2", values_to = "CorValue") %>% 
  ggplot(aes(x=Var1,y=Var2,fill=CorValue,label=CorValue))+
  geom_tile(colour = "pink")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(6,"Set1")[2],"white",brewer.pal(6,"Set1")[1]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))

### Fourth, the descriptive statistic of religious activities.
  
##### [The distribution of score for long distant pilgrimage.]{.ul}
  
##### Here, we calculated 4 types of score: 1;Total score, the 
##### score of the total pilgrimages taking part in since birth. 
##### 2; After18score, the score of the pilgrimages participanted 
##### after 18 years old. 3; Tenyearscore, the score of pilgrimages 
##### participanted in recent 10 years . 4;Fiveyearscore, the score 
##### of pilgrimages participanted in recent 5 years .
  

M_score %>% 
  group_by(NonLocalPilgrim) %>% 
  count()


p5 <- M_score %>% 
  filter(NonLocalPilgrim == 1) %>% 
  ggplot(aes(x = Totalscore))+
  geom_bar()

p6 <-M_score %>% 
  filter(NonLocalPilgrim == 1) %>% 
  ggplot(aes(x = After18score))+
  geom_bar()

p7 <-M_score %>% 
  filter(NonLocalPilgrim == 1) %>% 
  ggplot(aes(x = Tenyearscore))+
  geom_bar()

p8 <-M_score %>% 
  filter(NonLocalPilgrim == 1) %>% 
  ggplot(aes(x = Fiveyearscore))+
  geom_bar()

p5+p6+p7+p8

##### The difference of score for distant pilgrimage between gender.

# by Gender
M_score %>% 
  tableone::CreateTableOne(var = c("Totalscore","After18score","Fiveyearscore","Tenyearscore"),
                           strata = c("Gender"),
                           factorVars = c("Regularityscore"),
                           data = .) %>% 
  print() %>% 
  write.csv("Descriptive_plot_pilgrimage_gender.csv")


##### The difference of score for distant pilgrimage between different economic ranks.

# by family economic level  

M_score %>% 
  tableone::CreateTableOne(var = c("Totalscore","After18score","Fiveyearscore","Tenyearscore"), strata = c("Rank1"),
                           data = .) %>% 
  print() %>% 
  write.csv("Descriptive_plot_pilgrimage_economic_rank.csv")

##### [The distribution of score for daily religious acts.]{.ul}

##### The difference of score for daily religious acts between age cohorts.

Daily_score %>% 
  select(2:4,6,8,10,12,22,24,26,28) %>% 
  tableone::CreateTableOne(var = colnames(.)[c(1,3:7)] ,strata = c("Age_cohort"),
                           factorVars = c("LocalPilgrimageRegOrUnreg",
                                          "Kowtow",
                                          "TurnBeads",
                                          "BurningLastMonth",
                                          "FastingLY",
                                          "PerambulationLY"),data = .) %>% 
  print() %>% 
  write.csv("Descriptive_plot_Daily_Age_cohorts.csv")

##### The difference of score for daily religious acts between gender.

Daily_score %>% 
  select(2:4,6,8,10,12,22,24,26,28,29) %>% 
  tableone::CreateTableOne(var = colnames(.)[c(1,3:7,12)] ,strata = c("Gender"),
                           factorVars = c("LocalPilgrimageRegOrUnreg",
                                          "Kowtow",
                                          "TurnBeads",
                                          "BurningLastMonth",
                                          "FastingLY",
                                          "PerambulationLY",
                                          "DailyRegularscore"),data = .)%>% 
  print() %>% 
  write.csv("Descriptive_plot_Daily_Gender.csv") 

------------------------------------------------------------------------------
#---2. Data Analyse-Reputation nomination and religious investment
install.packages("GGally") # ggnet2
library(pscl)
library(RColorBrewer)
library(texreg)
library(sna)
library(statnet)
library(intergraph)
library(ggnet)
library(ggplot2)
library(sjmisc)
library(magrittr)
library(tidyverse)
library(btergm)
library(naniar) #miss_var_summary
library(GGally)
CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%  
  # Now 289 who have data of religious investment
  filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %>%
  # the NA are rare
  miss_var_summary()

###### nominator
# interviewees named an average of A people with B times

# Count A
CB_repu_final %>% group_by(ID) %>% 
  distinct(Nominee_ID) %>% 
  count(ID) %$% n %>% mean() 

# Count B
CB_repu_final %>% group_by(ID) %>% 
  count(ID) %$% n %>% mean() 

###### nominee

# each villager was named an average of C times by D individuals
CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %>%
  select(14:20) %>% 
  summarise(across(everything(),~ mean(.))) %>% sum()

# Count C
CB_repu_final %>% count(Nominee_ID) %$% n %>% mean()

# Count D
CB_repu_final %>% group_by(Nominee_ID) %>% 
  distinct(ID) %>% 
  count(Nominee_ID) %$% n %>% mean() 
######
# The correlation
######

CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %>%
  select(14:19) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("Var1") %>% 
  pivot_longer(-Var1,names_to = "Var2", values_to = "CorValue") %>% 
  ggplot(aes(x=Var1,y=Var2,fill=CorValue,label=CorValue))+
  geom_tile(colour = "pink")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(6,"Set1")[2],"white",brewer.pal(6,"Set1")[1]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))

##  build hurdle models

CB_demo_repu_reli <- CB_demo_repu_reli %>% 
  left_join(.,Relatedness_summary[,c(1,2,4)],by = c("ID" = "Ego"))

Reputation_hurdle_model <-  list()                        
Reputation_hurdle_model[[1]] <- hurdle(Devout ~ Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives+
                                         ScaleFiveyearscore+ScaleDailyRegularscore, 
                                       data = CB_demo_repu_reli %>% 
                                         filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                         filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 

Reputation_hurdle_model[[2]] <- hurdle(Generous ~ Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives+
                                         ScaleFiveyearscore+ScaleDailyRegularscore, 
                                       data = CB_demo_repu_reli %>% 
                                         filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                         filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 

Reputation_hurdle_model[[3]] <- hurdle(GoodCharacter ~ Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives+
                                         ScaleFiveyearscore+ScaleDailyRegularscore, 
                                       data = CB_demo_repu_reli %>% 
                                         filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                         filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 

Reputation_hurdle_model[[4]] <- hurdle(HardWorking ~  Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives+
                                         ScaleFiveyearscore+ScaleDailyRegularscore, 
                                       data = CB_demo_repu_reli %>% 
                                         filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                         filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 


screenreg(Reputation_hurdle_model,custom.model.names = c("Devout","Generous","GoodCharacter","HardWorking"))

### This (Above) is the Table S4 B in Supplementary Material

Reputation_hurdle_model_control <-  list()                        
Reputation_hurdle_model_control[[1]] <- hurdle(Devout ~ Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives, 
                                               data = CB_demo_repu_reli %>% 
                                                 filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                 filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 

Reputation_hurdle_model_control[[2]] <- hurdle(Generous ~ Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives, 
                                               data = CB_demo_repu_reli %>% 
                                                 filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                 filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 

Reputation_hurdle_model_control[[3]] <- hurdle(GoodCharacter ~ Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives, 
                                               data = CB_demo_repu_reli %>% 
                                                 filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                 filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 

Reputation_hurdle_model_control[[4]] <- hurdle(HardWorking ~  Gender+Age_cohort+Rank1+Rank2+Government+WhichTribe+relatives, 
                                               data = CB_demo_repu_reli %>% 
                                                 filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                 filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) ) 


screenreg(Reputation_hurdle_model_control,custom.model.names = c("Devout","Generous","GoodCharacter","HardWorking"))

### This (Above) is the Table S4 A in Supplementary Material, only containing control factors.
###---------------------------
## build nomination network
###---------------------------

# Edge list: 

# 1.Full nomination network
# 2.Based on their correlations, the qualities are grouped into Devout; Hardworking; Others. 

###
##1.Full nomination network
###

CB_Edgelist_reputation <- CB_repu_final %>% 
  mutate( Quality2 = rec(.$Qualities, rec= "Devote = Devout;
                                                             HardWorking = Hardworking;
                                                             else = Others")) %>% 
  group_by(ID,Nominee_ID) %>% 
  mutate(Count = n()) %>% 
  ungroup() %>% 
  distinct(.,ID,Nominee_ID,.keep_all = T) %>% 
  select(ID,Nominee_ID,Count)

# check if anyone being nominated 
# but didn't have any religious investment data

CB_Edgelist_reputation %>% 
  filter( !ID %in%  (CB_demo_repu_reli %>% 
                       filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                       filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %$%ID))

CB_Edgelist_reputation %>% 
  filter( !Nominee_ID %in%  (CB_demo_repu_reli %>% 
                               filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                               filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %$%ID))

# 2 are not in vertex data frame,which means don't have religious data
#--------------

# So rechoose edgelist to make sure all vertexs in edgelist are also in vertex attribute data frame

CB_Edgelist_reputation <- CB_repu_final %>% 
  mutate( Quality2 = rec(.$Qualities, rec= "Devote = Devout;
                                          HardWorking = Hardworking;
                                          else = Others")) %>% 
  group_by(ID,Nominee_ID) %>% 
  mutate(Count = n()) %>% 
  ungroup() %>% 
  distinct(.,ID,Nominee_ID,.keep_all = T) %>% 
  select(ID,Nominee_ID,Count) %>% 
  filter( Nominee_ID %in%  (CB_demo_repu_reli %>% 
                              filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                              filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %$%ID)) 

# transform into network and then make a plot 

# note: Dots sized by total pilgrimage score
# !!! NOTE: Here, "Reputation_network" object only contains distinct edges. 
CB_Reputaion_network <- igraph::graph.data.frame(d= CB_Edgelist_reputation,
                                                 vertices = CB_demo_repu_reli %>% 
                                                   filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                   filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)),
                                                 directed = T) %>% 
  intergraph::asNetwork() 


network.size(CB_Reputaion_network)
summary(CB_Reputaion_network ~ odegree(0:25))

CB_Reputaion_network %>% 
  ggnet2(., color = "Rank1",
         palette = c("L" = "steelblue","M" = "Yellow" ,"H" = "tomato"),
         alpha = 0.9, size = (.%v% "Totalscore")/100, 
         edge.alpha = 0.8,
         arrow.size = 3, arrow.gap = 0.00001,
         layout.par = list(cell.jitter = 0.75),
         mode = "kamadakawai")+
  guides(size=F,color =guide_legend( title = "Economic rank"))

ggsave("Reputation_nomination_network.tiff", units="in", width=7, height=6, dpi=300, compression = 'lzw') 
###
## 2. Plot of Separate nomination network
#------------------------------------------
###     Plot these in Gephi
#------------------------------------------
# ERG Models
library(ergm)

CB_Reputation_network <- igraph::graph.data.frame(d= CB_repu_final %>% 
                                                    select(1,3,2) %>% 
                                                    filter( Nominee_ID %in%  (CB_demo_repu_reli %>% 
                                                                                filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                                                filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)) %$%ID)),
                                                  vertices = CB_demo_repu_reli %>% 
                                                    filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                    filter(!is.na(Totalscore) & !is.na(ScaleDailyRegularscore)),
                                                  directed = T) %>% 
  intergraph::asNetwork() 

CB_Reputaion_network_model_ctr <- ergm(CB_Reputation_network ~ edges  + 
                                         nodeicov("Age")+
                                         nodeifactor("Rank1")+
                                         nodeofactor("Rank1")+
                                         nodematch("WhichTribe")+
                                         nodematch("Gender")+
                                         edgecov(kinship_matrix, attr = "r")+
                                         edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank"),
                                       control = control.ergm(seed=10,MCMC.burnin=10000,
                                                              MCMC.samplesize=50000,MCMC.interval=1000),
                                       verbose = F) 


Reputaion_network_model_ctr_reli <- ergm(Reputation_network ~ edges  + 
                                           nodeicov("Age")+
                                           nodeifactor("Rank1")+
                                           nodeofactor("Rank1")+
                                           nodematch("WhichTribe")+
                                           nodematch("Gender")+
                                           nodeicov("ScaleFiveyearscore")+
                                           nodeicov("ScaleDailyRegularscore")+
                                           edgecov(kinship_matrix, attr = "r")+
                                           edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank"),
                                         control = control.ergm(seed=10,MCMC.burnin=10000,
                                                                MCMC.samplesize=50000,MCMC.interval=1000),
                                         verbose = F) 


Reputaion_network_model <- ergm(Reputation_network ~ edges  + 
                                  nodeicov("Age")+
                                  nodeifactor("Rank1")+
                                  nodeofactor("Rank1")+
                                  nodematch("WhichTribe")+
                                  nodematch("Gender")+
                                  nodeicov("ScaleFiveyearscore")+
                                  nodeicov("ScaleDailyRegularscore")+
                                  edgecov(kinship_matrix, attr = "r")+
                                  edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank")+
                                  mutual,
                                control = control.ergm(seed=10,MCMC.burnin=10000,
                                                       MCMC.samplesize=50000,MCMC.interval=1000),
                                verbose = F) 

summary(Reputaion_network_model)
mcmc.diagnostics(Reputaion_network_model)
plot(gof(Reputaion_network_model))

screenreg(list(Reputaion_network_model_ctr,
               Reputaion_network_model_ctr_reli,
               Reputaion_network_model),
          custom.model.names = c("Control","Ctr+Reli","Ctr+Reli+Structure"))  
### This (Above) is the Table S6 in SI.

------------------------------------------------------------------------------
#---3. Models Separated by Gender
library(pscl)
library(texreg)
# Male
CB_demo_repu_reli %>% 
  filter(Gender == "Male") %>% 
  left_join(.,CB_survival[,c(1,6)]) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  mutate(Age_cohort2 = 
           case_when(
             Age >= 16 & Age <= 35 ~ "16-35",
             Age >= 36 & Age <= 55 ~ "36-55",
             Age >55 ~ ">55",
           )) -> Male

map(list("Devout","Generous","GoodCharacter","HardWorking"),
    ~ 
      hurdle( as.formula(paste0(.,"~ Age_cohort2+
                                     Rank1+
                                  ScaleFiveyearscore+ScaleDailyRegularscore")), 
              data = Male )) %>% 
  screenreg(.,custom.model.names = 
              c("Devout","Generous","GoodCharacter","HardWorking"))

# Female
CB_demo_repu_reli %>% 
  filter(Gender == "Female") %>% 
  left_join(.,CB_survival[,c(1,6)]) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  mutate(Age_cohort2 = 
           case_when(
             Age >= 16 & Age <= 35 ~ "16-35",
             Age >= 36 & Age <= 55 ~ "36-55",
             Age >55 ~ ">55",
           )) -> Female

map(list("Devout","Generous","GoodCharacter","HardWorking"),
    ~ hurdle( as.formula(paste0(.,"~ Age_cohort2+
                   Rank1+
                   ScaleFiveyearscore+ScaleDailyRegularscore")), 
              data = Female)) %>% 
  screenreg(.,custom.model.names =
              c("Devout","Generous","GoodCharacter","HardWorking"))

  #---3. Data Analyse-Reputation nomination and religisity, Old & Young
##### Testing the influence of elaborate/routine behaviors on the times of nominations from different age groups
--##1.
  CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)])
#-----------------

##How to devide Elder and Young villigers
# cut the ages; ages of nominators 

# Cut the Age`s of 246 nominators
CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %$% Age %>% 
  cut(., breaks = c(min(.),40,60,max(.))) %>% table()

# Cut the age according to the all number of nomination
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %$% nominator_age %>% 
  cut(., breaks = c(min(.),40,60,max(.))) %>% table()

# cut the ages; ages of nominees 
CB_repu_final %>% 
  mutate(nominee_age = CB_demo_repu$Age[match(.$Nominee_ID, CB_demo_repu$ID)])%$% nominee_age %>% 
  cut(., breaks = c(min(.),40,60,max(.))) %>% table()
#--- age cut by 40, 60 age point.
##3. The nomination from young villagers and elder villagers

# Counting the all number of nomination

#3.1 The distribution of nominations from young people, 
#    note the distribution of old people nominations is identical.
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
  filter(nominator_age <= 40) %>% 
  count(Nominee_ID) %>% 
  left_join(CB_demo_repu_reli,.,by = c("ID" = "Nominee_ID")) %>% 
  mutate( Nomination_Young = replace_na(.$n,0)) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  ggplot(aes(Nomination_Young))+
  geom_bar()

#3.2 The young people nominations model 
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
  filter(nominator_age <= 40) %>% 
  count(Nominee_ID) %>% 
  left_join(CB_demo_repu_reli,.,by = c("ID" = "Nominee_ID")) %>% 
  mutate( Nomination_Young = replace_na(.$n,0)) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  hurdle(Nomination_Young ~ Gender+Age+Rank1+Rank2+WhichTribe+relatives+
           ScaleFiveyearscore+ScaleDailyRegularscore,
         data = .) %>% 
  summary()

#3.3 The old people nominations model 
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
  filter(nominator_age > 40 & nominator_age <= 60) %>% 
  count(Nominee_ID) %>% 
  left_join(CB_demo_repu_reli,.,by = c("ID" = "Nominee_ID")) %>% 
  mutate( Nomination_Old = replace_na(.$n,0)) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  hurdle(Nomination_Old ~ Gender+Age+Rank1+Rank2+Government+WhichTribe+relatives+
           ScaleFiveyearscore+ScaleDailyRegularscore,
         data = .) %>% 
  summary()

### Using the proportion of nomination by young/old people

# Young
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
  filter(nominator_age <= 40) %>% 
  count(Nominee_ID) %>%  
  left_join(CB_demo_repu_reli,.,by = c("ID" = "Nominee_ID")) %>% 
  mutate( Nomination_Young = replace_na(.$n,0)) %>% 
  left_join(., Reputation %>% 
              mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
              count(Nominee_ID) %>% 
              rename( total_n = n),
            by = c("ID" = "Nominee_ID")) %>% 
  mutate( total_n = replace_na(.$total_n,0),
          prop_n = Nomination_Young/total_n,
          prop_n = if_else( prop_n == "NaN",0,prop_n)) %>%
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  lm(prop_n ~ Gender+Age+Rank1+Rank2+WhichTribe+relatives+
       ScaleFiveyearscore+ScaleDailyRegularscore,
     data = .) %>% 
  summary()

# Old
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
  filter(nominator_age > 40 & nominator_age <= 60) %>% 
  count(Nominee_ID) %>%  
  left_join(CB_demo_repu_reli,.,by = c("ID" = "Nominee_ID")) %>% 
  mutate( Nomination_Old = replace_na(.$n,0)) %>% 
  left_join(., Reputation %>% 
              mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
              count(Nominee_ID) %>% 
              rename( total_n = n),
            by = c("ID" = "Nominee_ID")) %>% 
  mutate( total_n = replace_na(.$total_n,0),
          prop_n = Nomination_Old/total_n,
          prop_n = if_else( prop_n == "NaN",0,prop_n)) %>%
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  lm(prop_n ~ Gender+Age+Rank1+Rank2+WhichTribe+relatives+
       ScaleFiveyearscore+ScaleDailyRegularscore,
     data = .) %>% 
  summary()

# Change to Tenyearscore/ After18score / Totalscore
CB_repu_final %>% 
  mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
  filter(nominator_age <= 40) %>% 
  count(Nominee_ID) %>%  
  left_join(CB_demo_repu_reli,.,by = c("ID" = "Nominee_ID")) %>% 
  mutate( Nomination_Young = replace_na(.$n,0)) %>% 
  left_join(., Reputation %>% 
              mutate(nominator_age = CB_demo_repu$Age[match(.$ID, CB_demo_repu$ID)]) %>% 
              count(Nominee_ID) %>% 
              rename( total_n = n),
            by = c("ID" = "Nominee_ID")) %>% 
  mutate( total_n = replace_na(.$total_n,0),
          prop_n = Nomination_Young/total_n,
          prop_n = if_else( prop_n == "NaN",0,prop_n)) %>%
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  lm(prop_n ~ Gender+Age+Rank1+Rank2+WhichTribe+relatives+
       scale(Tenyearscore)+ScaleDailyRegularscore,
     data = .) %>% 
  summary()

### This (Above) is the Table S5 A & B in Supplementary Material.
------------------------------------------------------------------------------
#---4. Data Analyse-Reputation nomination and religion_Separated by Gender

# Male
Suheri_demo_repu_reli %>% 
  filter(Gender == "Male") %>% 
  left_join(.,Suheri_survival[,c(1,6)]) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  mutate(Age_cohort2 = 
           case_when(
             Age >= 16 & Age <= 35 ~ "16-35",
             Age >= 36 & Age <= 55 ~ "36-55",
             Age >55 ~ ">55",
           )) -> Male

map(list("Devout","Generous","GoodCharacter","HardWorking"),
    ~ 
      hurdle( as.formula(paste0(.,"~ Age_cohort2+Rank1+
                                  WhichTribe+
                                  ScaleFiveyearscore+ScaleDailyRegularscore")), 
              data = Male )) %>% 
  screenreg(.,custom.model.names = 
              c("Devout","Generous","GoodCharacter","HardWorking"))

# Female
Suheri_demo_repu_reli %>% 
  filter(Gender == "Female") %>% 
  left_join(.,Suheri_survival[,c(1,6)]) %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(ScaleFiveyearscore) & !is.na(ScaleDailyRegularscore)) %>% 
  mutate(Age_cohort2 = 
           case_when(
             Age >= 16 & Age <= 35 ~ "16-35",
             Age >= 36 & Age <= 55 ~ "36-55",
             Age >55 ~ ">55",
           )) -> Female

map(list("Devout","Generous","GoodCharacter","HardWorking"),
    ~ hurdle( as.formula(paste0(.,"~ Age_cohort2+
                   Rank1+
                   WhichTribe+
                   ScaleFiveyearscore+ScaleDailyRegularscore")), 
              data = Female)) %>% 
  screenreg(.,custom.model.names =
              c("Devout","Generous","GoodCharacter","HardWorking"))
### This (Above) is the Table S8 A & B in Supplementary Material.
------------------------------------------------------------------------------
#---5. Consensus Analyze

### Consensus Analyses
##
install.packages("devtools")

library(devtools)
source("http://alastair.io/install_github/install_github.R")
install_github("alastair-JL/AnthroTools")

install.packages("AnthroTools")
library(AnthroTools)

Results_of_Consensus_Analyze <- Results_of_Consensus_Analyze %>% 
  rownames_to_column()

names(Results_of_Consensus_Analyze) <- c("Religious_acts","Weights")

Results_of_Consensus_Analyze <-   Results_of_Consensus_Analyze %>% 
  mutate(Religious_acts = str_sub(Religious_acts,
                                  start = str_locate(Religious_acts,"[[:punct:]]")[,1]+1,
                                  end = str_length(Religious_acts)),
         terms = str_split_fixed(Religious_acts,"[[:alpha:]] ",2)[,2])

Results_of_Consensus_Analyze <-  Results_of_Consensus_Analyze %>% 
  mutate(Religious_acts = str_sub(Religious_acts,
                                  start = 1,
                                  end = str_locate(Religious_acts,"[[:punct:]]")[,1]-1))

Results_of_Consensus_Analyze %>% 
  mutate( terms = str_replace(Results_of_Consensus_Analyze$terms,"Investment","Cost"))


Results_of_Consensus_Analyze <- Results_of_Consensus_Analyze %>% 
  pivot_wider(names_from = "terms",values_from = "Weights") %>% 
  set_names("Religious_acts","Energy","Money","Time","Willing")%>% 
  rowwise() %>% 
  mutate(score = sum(c(Energy,Money,Time)))
### This (Above) is the Table S2 in Supplementary Material.
------------------------------------------------------------------------------
#---6. Plots
### Percentage of people who participated in daily religious practices
# We made a new data frame according to the results of data cleaning.
New_Daily_Prac_Agecohort  = tribble(
  ~Practice,          ~Age,   ~Freq,
  "LocalPilgrimage", "16-25", 0,
  "LocalPilgrimage", "26-35", 0.048,
  "LocalPilgrimage", "36-45", 0.092,
  "LocalPilgrimage", "46-55", 0.238,
  "LocalPilgrimage", ">55", 0.448,
  "Fasting", "16-25", 0.067,
  "Fasting", "26-35", 0.262,
  "Fasting", "36-45", 0.494,
  "Fasting", "46-55", 0.381,
  "Fasting", ">55", 0.448,
  "Prostration", "16-25", 0.167,
  "Prostration", "26-35", 0.214,
  "Prostration", "36-45", 0.425,
  "Prostration", "46-55", 0.413,
  "Prostration", ">55", 0.388,
  "AccountingBeads", "16-25", 0.067,
  "AccountingBeads", "26-35", 0.214,
  "AccountingBeads", "36-45", 0.563,
  "AccountingBeads", "46-55", 0.587,
  "AccountingBeads", ">55", 0.925,
  "Burning", "16-25", 0.4,
  "Burning", "26-35", 0.571,
  "Burning", "36-45", 0.874,
  "Burning", "46-55", 0.794,
  "Burning", ">55", 0.866,
) %>% 
  mutate(Age = factor(Age, levels = c("16-25","26-35","36-45","46-55",">55")))

New_Daily_Prac_Agecohort$Practice <- factor(New_Daily_Prac_Agecohort$Practice,
                                            levels = c("LocalPilgrimage","Fasting","Prostration","AccountingBeads","Burning"),
                                            labels = c("Local Pilgrimage","Fasting","Prostration","Counting Beads","Burning"))

ggplot(data = New_Daily_Prac_Agecohort,aes(x = Age, y = Freq, fill = Age ))+
  facet_wrap(~ Practice,scales = "free_x") + geom_col(width = 0.8) +
  geom_text(aes(x = Age,y = Freq, label = str_c(Freq*100, "%"),label = Freq),vjust = -0.3,size = 3)+
  theme(legend.position='none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white",colour = "grey"),
        strip.text = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(size = rel(0.8)))+ 
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1.00), linetype = "dotted", color = "grey",size = 0.3) +
  labs(x = "Age Cohort",y = "Percent of Respondents") +
  scale_fill_manual(values = c("#A8D2FF","#4DAAAC","#176199","#21943E","#947529"))
### This (Above) is the Figure S2 in Supplementary Material.

###Daily_Gender  
New_Daily_Prac_Gender  = tribble(
  ~Practice,          ~Gender,   ~Freq,
  "LocalPilgrimage", "Women", 0.317,
  "LocalPilgrimage", "Men", 0.068,
  
  "Fasting", "Women", 0.725,
  "Fasting", "Men", 0.048,
  
  "Prostration", "Women", 0.692,
  "Prostration", "Men", 0.129,
  
  "AccountingBeads", "Women", 0.669,
  "AccountingBeads", "Men", 0.435,
  
  "Burning", "Women", 0.746,
  "Burning", "Men", 0.776,
) %>% 
  mutate(Gender = factor(Gender, levels = c("Women","Men")))

New_Daily_Prac_Gender$Practice <- factor(New_Daily_Prac_Gender$Practice,
                                         levels = c("LocalPilgrimage","Fasting","Prostration","AccountingBeads","Burning"),
                                         labels = c("Local Pilgrimage","Fasting","Prostration","Counting Beads","Burning"))  

ggplot(data = New_Daily_Prac_Gender,aes(x = Gender, y = Freq, fill = Gender ))+
  facet_wrap(~ Practice,scale="free_x") + geom_col(width = 0.4) +
  geom_text(aes(x = Gender, y = Freq, label = str_c(Freq*100, "%"),label = Freq),vjust = -0.3,size = 3)+
  theme(legend.position='none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white",colour = "grey"),
        strip.text = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(size = rel(0.8)))+ 
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1.00), linetype = "dotted", color = "grey",size = 0.3) +
  labs(x = "Gender",y = "Percent of Respondents") +
  scale_fill_manual(values = c("#FA6A96","#59E0AA"))
### This (Above) is the Figure 2 in Manuscript.


###  New_Reputation_Nomination 
Percentage_Nomination <- Suheri_demo_repu_reli[,c("ID", "Gender","Age","Age_cohort","Devout", "Generous",
                                                  "GoodCharacter","HardWorking")]



Percentage_Nomination <- within(Percentage_Nomination,{
  Devout_Perc <- NA
  Devout_Perc [Devout == 0] <- "0"
  Devout_Perc [Devout > 0 & Devout <= 3 ] <- "1-3"
  Devout_Perc [Devout >= 4] <- ">= 4" })

Percentage_Nomination <- within(Percentage_Nomination,{
  Generous_Perc <- NA
  Generous_Perc[Generous == 0] <- "0"
  Generous_Perc[Generous > 0 & Generous <= 3] <- "1-3"
  Generous_Perc[Generous >= 4] <- ">= 4"})

Percentage_Nomination <- within(Percentage_Nomination,{
  GoodChara_Perc <- NA
  GoodChara_Perc [GoodCharacter == 0] <- "0"
  GoodChara_Perc [GoodCharacter > 0 & GoodCharacter <= 3] <- "1-3"
  GoodChara_Perc [GoodCharacter >= 4] <- ">= 4"})

Percentage_Nomination <- within(Percentage_Nomination,{
  HardWorking_Perc <- NA
  HardWorking_Perc [HardWorking  == 0] <- "0"
  HardWorking_Perc [HardWorking > 0 & HardWorking <= 3] <- "1-3"
  HardWorking_Perc [HardWorking >= 4] <- ">= 4"})

str(Percentage_Nomination)
unique(Percentage_Nomination$HardWorking_Perc)
#--------------------------------------------------------------------------------------

Percentage_Nomination$Gender <- factor(Percentage_Nomination$Gender,
                                       levels = c("Female","Male"),
                                       labels = c("Women","Men"))


Percentage_Nomination$Devout_Perc <- factor(Percentage_Nomination$Devout_Perc,
                                            levels = c("0","1-3",">= 4"),
                                            labels = c("0","1-3",">=4"))

Percentage_Nomination$Generous_Perc <- factor(Percentage_Nomination$Generous_Perc,
                                              levels = c("0","1-3",">= 4"),
                                              labels = c("0","1-3",">=4"))

Percentage_Nomination$GoodChara_Perc <- factor(Percentage_Nomination$GoodChara_Perc,
                                               levels = c("0","1-3",">= 4"),
                                               labels = c("0","1-3",">=4"))

Percentage_Nomination$HardWorking_Perc <- factor(Percentage_Nomination$HardWorking_Perc,
                                                 levels = c("0","1-3",">= 4"),
                                                 labels = c("0","1-3",">=4")) 

# Plot

Perc_Nomi1 <- Percentage_Nomination %>% 
  filter(Age > 18) %>% 
  ggplot(data = ., aes(x = Devout_Perc,fill = Gender)) +
  geom_bar(aes(y = ..prop.., group = 1),width = 0.8)+facet_grid(~Gender)+
  geom_text(aes(label = scales::percent(..prop..),y = ..prop..,group = 1),stat = "count",vjust = -0.3,size = 3)+
  labs(x = "Devout", y = NULL)+ 
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.2,0.4,0.6,0.8,1.00), linetype = "dotted", color = "grey",size = 0.3)+
  theme(strip.background = element_rect(fill = "white",colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold",size = 8),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(face = "bold",size = 9))+
  scale_fill_manual(values = c("#FA6A96","#59E0AA"))+
  guides(fill = F)

Perc_Nomi2 <- Percentage_Nomination %>% 
  filter(Age > 18) %>% 
  ggplot(data = .,aes(x = Generous_Perc,fill = Gender))+
  geom_bar(aes(y = ..prop..,group = 1),width = 0.8)+facet_grid(~Gender)+
  geom_text(aes(label = scales::percent(..prop..),y = ..prop..,group = 1),stat = "count",vjust = -0.3,size = 3)+
  labs(x = "Generous", y = NULL)+ 
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.2,0.4,0.6,0.8,1.00), linetype = "dotted", color = "grey",size = 0.3)+
  theme(strip.background = element_rect(fill = "white",colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text = element_text(face = "bold",size = 8),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(face = "bold",size = 9))+
  scale_y_continuous(breaks = NULL)+
  scale_fill_manual(values = c("#FA6A96","#59E0AA"))+
  guides(fill = F)

Perc_Nomi3 <- Percentage_Nomination %>% 
  filter(Age > 18) %>% 
  ggplot(data = ., aes(x = GoodChara_Perc,fill = Gender)) +
  geom_bar(aes(y = ..prop.., group = 1),width = 0.8)+ facet_grid(~Gender) +
  geom_text(aes(label = scales::percent(..prop..),y = ..prop..,group = 1), stat = "count",  vjust = -0.3,size = 3)+
  labs(x = "Good Character", y = NULL)+ 
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.2,0.4,0.6,0.8,1.00), linetype = "dotted", color = "grey",size = 0.3)+
  theme(strip.background = element_rect(fill = "white",colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(face = "bold",size = 9))+
  scale_fill_manual(values = c("#FA6A96","#59E0AA"))+
  guides(fill = F)


Perc_Nomi4 <- Percentage_Nomination %>% 
  filter(Age > 18) %>% 
  ggplot(data = ., aes(x = HardWorking_Perc,fill = Gender))+
  geom_bar(aes(y = ..prop.., group = 1),width = 0.8)+ facet_grid(~Gender) +
  geom_text(aes(label = scales::percent(..prop..),y = ..prop..,group = 1), stat = "count",  vjust = -0.3,size = 3)+
  labs(x = "Hard Working", y = NULL)+ 
  scale_y_continuous(breaks = c(0,0.2,0.4,0.6,0.8,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.2,0.4,0.6,0.8,1.00), linetype = "dotted", color = "grey",size = 0.3)+
  theme(strip.background = element_rect(fill = "white",colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(face = "bold",size = 9))+
  scale_y_continuous(breaks = NULL)+
  scale_fill_manual(values = c("#FA6A96","#59E0AA"))+
  guides(fill = F)

Figure <- ggarrange(Perc_Nomi1,Perc_Nomi2,Perc_Nomi3,Perc_Nomi4)

annotate_figure(Figure,left = text_grob("Percent of Respondents",rot = 90,size = 11))

### This (Above) is the Figure S4 in Supplementary Material.

######## The correlation plot showing the relationship between different nominations.########

###   This is original code from Results_Descriptive_Statistics ###
CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(Totalscore) & !is.na(DailyRegularscore)) %>%
  select(14:19) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("Var1") %>% 
  pivot_longer(-Var1,names_to = "Var2", values_to = "CorValue") %>% 
  ggplot(aes(x=Var1,y=Var2,fill=CorValue,label=CorValue))+
  geom_tile(colour = "pink")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(6,"Set1")[2],"white",brewer.pal(6,"Set1")[1]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))


Cor <- CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(Totalscore) & !is.na(DailyRegularscore)) %>%
  select(14:19) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame() %>% 
  rownames_to_column("Var1") %>% 
  pivot_longer(-Var1,names_to = "Var2", values_to = "CorValue")

ggplot(data=Cor,aes(x= Var1 ,y=Var2,fill=CorValue,label=CorValue))+
  geom_tile(colour = "pink")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(6,"Set1")[2],"white",brewer.pal(6,"Set1")[1]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))


New_cor = tribble(
  ~V1,            ~V2,           ~CorValue,
  "Knowledgable", "Knowledgable",    1.00,
  "Knowledgable", "Influential",     0.86,
  "Knowledgable", "HardWorking",     0.32,
  "Knowledgable", "GoodCharacter",   0.87,
  "Knowledgable", "Generous",        0.81,
  "Knowledgable", "Devout",          0.20,
  
  "Influential", "Knowledgable",    0.86,
  "Influential", "Influential",     1.00,
  "Influential", "HardWorking",     0.22,
  "Influential", "GoodCharacter",   0.94,
  "Influential", "Generous",        0.82,
  "Influential", "Devout",          0.38,
  
  "HardWorking", "Knowledgable",    0.32,
  "HardWorking", "Influential",     0.22,
  "HardWorking", "HardWorking",     1.00,
  "HardWorking", "GoodCharacter",   0.34,
  "HardWorking", "Generous",        0.29,
  "HardWorking", "Devout",          0.04,
  
  "GoodCharacter", "Knowledgable",    0.87,
  "GoodCharacter", "Influential",     0.94,
  "GoodCharacter", "HardWorking",     0.34,
  "GoodCharacter", "GoodCharacter",   1.00,
  "GoodCharacter", "Generous",        0.88,
  "GoodCharacter", "Devout",          0.34,
  
  "Generous", "Knowledgable",    0.81,
  "Generous", "Influential",     0.82,
  "Generous", "HardWorking",     0.29,
  "Generous", "GoodCharacter",   0.88,
  "Generous", "Generous",        1.00,
  "Generous", "Devout",          0.07,
  
  "Devout", "Knowledgable",    0.20,
  "Devout", "Influential",     0.38,
  "Devout", "HardWorking",     0.04,
  "Devout", "GoodCharacter",   0.34,
  "Devout", "Generous",        0.07,
  "Devout", "Devout",          1.00,
) %>% 
  mutate(V2 = factor(V2,levels = c("Knowledgable","Influential","HardWorking","GoodCharacter","Generous","Devout")))

ggplot(data=New_cor,aes(x= V1 ,y=V2,fill=CorValue,label=CorValue))+
  geom_tile(colour = "pink")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(6,"Set1")[2],"white",brewer.pal(6,"Set1")[1]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))

####### The correlation plot showing the relationship between different nominations, except the "knowledgeable",
####### and "influential"

New_cor_1 = tribble(
  ~V11,            ~V22,           ~CorValue,
  "HardWorking", "HardWorking",     1.00,
  "HardWorking", "GoodCharacter",   0.34,
  "HardWorking", "Generous",        0.29,
  "HardWorking", "Devout",          0.04,
  
  "GoodCharacter", "HardWorking",     0.34,
  "GoodCharacter", "GoodCharacter",   1.00,
  "GoodCharacter", "Generous",        0.88,
  "GoodCharacter", "Devout",          0.34,
  
  "Generous", "HardWorking",     0.29,
  "Generous", "GoodCharacter",   0.88,
  "Generous", "Generous",        1.00,
  "Generous", "Devout",          0.07,
  
  "Devout", "HardWorking",     0.04,
  "Devout", "GoodCharacter",   0.34,
  "Devout", "Generous",        0.07,
  "Devout", "Devout",          1.00,
) %>% 
  mutate(V22 = factor(V22,levels = c("HardWorking","GoodCharacter","Generous","Devout")))

ggplot(data=New_cor_1,aes(x= V11 ,y=V22,fill=CorValue,label=CorValue))+
  geom_tile(colour = "pink")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(6,"Set1")[2],"white",brewer.pal(6,"Set1")[1]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1))

###-Corplot_New with upper triangle on major revision of MS

Cor_4types <- CB_demo_repu_reli %>% 
  filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
  filter(!is.na(Totalscore) & !is.na(DailyRegularscore)) %>%
  select(14:17) %>% 
  cor() %>% 
  round(2) %>% 
  as.data.frame()

ggplot(data=New_cor_1,aes(x= V11 ,y=V22,fill=CorValue,label=CorValue))+
  geom_tile(colour = "black")+
  geom_text(size=5,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(5,"Greys")[8],brewer.pal(5,"Greys")[4]))+
  xlab("Reputation Qualities")+
  ylab("Reputation Qualities")+
  labs(fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1),
        panel.grid.major = element_blank())

Cor_4types[lower.tri(Cor_4types)] <- NA #---retain the lower part of coefficients

Cor_4types

Cor_4types$variable <- rownames(Cor_4types) #---add one more column

Cor_4types

Cor_4types_new <-  pivot_longer(Cor_4types,cols = c(Devout,Generous,GoodCharacter,HardWorking),
                                values_drop_na = T)

Cor_4types_new

Cor_4types_new$name <- factor(Cor_4types_new$name,
                              levels = c("HardWorking","GoodCharacter","Generous","Devout"))

ggplot(Cor_4types_new,aes (x = variable, y = name, label = value))+
  geom_tile(aes(fill = value),colour = "black")+
  geom_text(size=4,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(4,"Greys")[8],brewer.pal(4,"Greys")[4]))+
  labs(x= NULL,y = NULL,fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1),
        panel.background =  element_blank())

###---Review letter 2, "there is no reason to include the self-correlations
###---of each variable", Delete 1 in Cor_4types_new.
str(Cor_4types_new)

Cor_4types_new <- Cor_4types_new[-c(1,5,8,10),] ####delete row number1,5,8,10
Cor_4types_new

###--------draw another figure with the original code.

ggplot(Cor_4types_new,aes (x = variable, y = name, label = value))+
  geom_tile(aes(fill = value),colour = "black")+
  geom_text(size=4,colour="black")+
  coord_equal()+
  scale_fill_gradientn( colours = c(brewer.pal(4,"Greys")[8],brewer.pal(4,"Greys")[4]))+
  labs(x= NULL,y = NULL,fill = "")+
  theme(axis.text.x = element_text(angle = 30,hjust = 1,vjust = 1),
        panel.background =  element_blank())     
### This (Above) is the Figure 3 in Manuscript.

#################Counting the number of people who did the daily religious practice
sum(Daily_score$Kowtow == 1 & Daily_score$Q16.Gender == 0)
length(which(Daily_score$Kowtow == 1 & Daily_score$Q16.Gender == 0))

sum(Daily_score$LocalPilgrimageRegOrUnreg == 1 & Daily_score$Q16.Gender == 0)
sum(Daily_score$LocalPilgrimageRegOrUnreg == 1 & Daily_score$Q16.Gender == 1)

sum(Daily_score$TurnBeads ==1 & Daily_score$Q16.Gender == 0)
sum(Daily_score$TurnBeads ==1 & Daily_score$Q16.Gender == 1)

sum(Daily_score$BurningLastMonth == 1 & Daily_score$Q16.Gender == 0)
sum(Daily_score$BurningLastMonth == 1 & Daily_score$Q16.Gender == 1)

length(which(Daily_score$FastingLY ==1 & Daily_score$Q16.Gender == 0))
length(which(Daily_score$FastingLY ==1 & Daily_score$Q16.Gender == 1))

sum(Daily_score$PerambulationLY ==1 & Daily_score$Q16.Gender == 0)
length(which(Daily_score$PerambulationLY ==1 & Daily_score$Q16.Gender ==1))

sum(Daily_score$DailyRegularscore == 0 & Daily_score$Q16.Gender ==0)
sum(Daily_score$DailyRegularscore == 0 & Daily_score$Q16.Gender ==1)
sum(Daily_score$DailyRegularscore == 1 & Daily_score$Q16.Gender ==0)
sum(Daily_score$DailyRegularscore == 1 & Daily_score$Q16.Gender ==1)
sum(Daily_score$DailyRegularscore == 2 & Daily_score$Q16.Gender ==0)

sum(Daily_score$DailyRegularscore == 2 & Daily_score$Q16.Gender ==1)
sum(Daily_score$DailyRegularscore == 3 & Daily_score$Q16.Gender ==0)
sum(Daily_score$DailyRegularscore == 3 & Daily_score$Q16.Gender ==1)

Daily_score1 <- Daily_score[,c("ID","LocalPilgrimageRegOrUnreg","Kowtow","TurnBeads","BurningLastMonth",
                               "FastingLY","PerambulationLY","DailyRegularscore","Age_cohort","Q16.Gender")]
names(Daily_score1)[10] <- "Gender"
Daily_score1

Daily_score2 <- rename(Daily_score1,c(LocalPilgrimageRegOrUnreg = "LocalPilgrimage",TurnBeads = "AccountingBeads",
                                      BurningLastMonth = "Burning", FastingLY = "Fasting",PerambulationLY = "Perambulation"))

Daily_score2$Gender <- factor(Daily_score1$Gender,
                              levels = c(0,1),
                              labels = c("Female","Male"))
Daily_score2$Age_cohort <- factor(Daily_score1$Age_cohort,
                                  levels = c("16-25","26-35","36-45","46-55",">55"),
                                  labels = c("16-25","26-35","36-45","46-55",">55"))
Daily_score2
str(Daily_score2)

Perambulation <- Daily_score2[,c("ID","Perambulation","Age_cohort","Gender")]
LocalPilgrimage <- Daily_score2[,c("ID","LocalPilgrimage","Age_cohort","Gender")]
Fasting <- Daily_score2[,c("ID","Fasting","Age_cohort","Gender")]
Kowtow <- Daily_score2[,c("ID","Kowtow","Age_cohort","Gender")]
AccountingBeads <- Daily_score2[,c("ID","AccountingBeads","Age_cohort","Gender")]
Burning <- Daily_score2[,c("ID","Burning","Age_cohort","Gender")]

Perambulation$Type <- "Perambulation"
LocalPilgrimage$Type <- "LocalPilgrimage"
Fasting$Type <- "Fasting"
Kowtow$Type <- "Kowtow"
AccountingBeads$Type <- "AccountingBeads"
Burning$Type <- "Burning"

names(Perambulation)[2] <- "Participation"
names(LocalPilgrimage)[2] <- "Participation"
names(Fasting)[2] <- "Participation"
names(Kowtow)[2] <- "Participation"
names(AccountingBeads)[2] <- "Participation"
names(Burning)[2] <- "Participation"


Daily_score3 <- rbind(Perambulation,LocalPilgrimage,Fasting,Kowtow,AccountingBeads,Burning)

Daily_score3$Participation <- factor(Daily_score3$Participation,
                                     levels = c(0,1),
                                     labels = c("No","Yes"))


Daily_score3$Type <- factor(Daily_score3$Type,
                            levels = c("Perambulation","LocalPilgrimage","Fasting","Kowtow","AccountingBeads","Burning"),
                            labels = c("Perambulation","Local Pilgrimage","Fasting","Kowtow","Accounting Beads","Burning"))


Daily_score3$Tookpart [Daily_score3$Participation=="No"] <- 0
Daily_score3$Tookpart [Daily_score3$Participation=="Yes"] <- 1

sum(Daily_score3$Tookpart == "1")

############ Percentage of people who participated in daily religious practices
Daily_Prac_Agecohort  = tribble(
  ~Practice,          ~Age,   ~Freq,
  "Perambulation",   "16-25", 0,
  "Perambulation",   "26-35", 0.024,
  "Perambulation",   "36-45", 0.034,
  "Perambulation",   "46-55", 0,
  "Perambulation",   ">55", 0.03,
  "LocalPilgrimage", "16-25", 0,
  "LocalPilgrimage", "26-35", 0.048,
  "LocalPilgrimage", "36-45", 0.092,
  "LocalPilgrimage", "46-55", 0.238,
  "LocalPilgrimage", ">55", 0.448,
  "Fasting", "16-25", 0.067,
  "Fasting", "26-35", 0.262,
  "Fasting", "36-45", 0.494,
  "Fasting", "46-55", 0.381,
  "Fasting", ">55", 0.448,
  "Kowtow", "16-25", 0.167,
  "Kowtow", "26-35", 0.214,
  "Kowtow", "36-45", 0.425,
  "Kowtow", "46-55", 0.413,
  "Kowtow", ">55", 0.388,
  "AccountingBeads", "16-25", 0.067,
  "AccountingBeads", "26-35", 0.214,
  "AccountingBeads", "36-45", 0.563,
  "AccountingBeads", "46-55", 0.587,
  "AccountingBeads", ">55", 0.925,
  "Burning", "16-25", 0.4,
  "Burning", "26-35", 0.571,
  "Burning", "36-45", 0.874,
  "Burning", "46-55", 0.794,
  "Burning", ">55", 0.866,
) %>% 
  mutate(Age = factor(Age, levels = c("16-25","26-35","36-45","46-55",">55")))

Daily_Prac_Agecohort$Practice <- factor(Daily_Prac_Agecohort$Practice,
                                        levels = c("Perambulation","LocalPilgrimage","Fasting","Kowtow","AccountingBeads","Burning"),
                                        labels = c("Perambulation","Local Pilgrimage","Fasting","Kowtow","Accounting Beads","Burning"))


ggplot(data = Daily_Prac_Agecohort,aes(x = Age, y = Freq, fill = Age ))+
  facet_wrap(~ Practice,scales = "free_x") + geom_col(width = 0.8) +
  geom_text(aes(x = Age,y = Freq, label = str_c(Freq*100, "%"),label = Freq),vjust = -0.3,size = 3)+
  theme(legend.position='none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white",colour = "grey"),
        strip.text = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(size = rel(0.8)))+ 
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1.00), linetype = "dotted", color = "grey",size = 0.3) +
  labs(x = "Age Cohort",y = NULL) +
  scale_fill_manual(values = c("#A8D2FF","#4DAAAC","#176199","#21943E","#947529"))
### This (Above) is the Figure S2 in Manuscript.

###Daily_Gender  

Daily_Prac_Gender  = tribble(
  ~Practice,          ~Gender,   ~Freq,
  "Perambulation",   "Female", 0.021,
  "Perambulation",   "Male", 0.02,
  
  "LocalPilgrimage", "Female", 0.317,
  "LocalPilgrimage", "Male", 0.068,
  
  "Fasting", "Female", 0.725,
  "Fasting", "Male", 0.048,
  
  "Kowtow", "Female", 0.692,
  "Kowtow", "Male", 0.129,
  
  "AccountingBeads", "Female", 0.669,
  "AccountingBeads", "Male", 0.435,
  
  "Burning", "Female", 0.746,
  "Burning", "Male", 0.776,
) %>% 
  mutate(Gender = factor(Gender, levels = c("Female","Male")))

Daily_Prac_Gender$Practice <- factor(Daily_Prac_Gender$Practice,
                                     levels = c("Perambulation","LocalPilgrimage","Fasting","Kowtow","AccountingBeads","Burning"),
                                     labels = c("Perambulation","Local Pilgrimage","Fasting","Kowtow","Accounting Beads","Burning"))  

ggplot(data = Daily_Prac_Gender,aes(x = Gender, y = Freq, fill = Gender ))+
  facet_wrap(~ Practice,scale="free_x") + geom_col(width = 0.4) +
  geom_text(aes(x = Gender, y = Freq, label = str_c(Freq*100, "%"),label = Freq),vjust = -0.3,size = 3)+
  theme(legend.position='none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "white",colour = "grey"),
        strip.text = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold"),
        axis.text = element_text(size = rel(0.8)))+ 
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1.00), labels = scales::percent)+
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1.00), linetype = "dotted", color = "grey",size = 0.3) +
  labs(x = "Gender",y = NULL) +
  scale_fill_manual(values = c("#A8D2FF","#4DAAAC"))
### This (Above) is the Figure 2 in Manuscript.

M_score1 <- M_score[,c("ID","Totalscore","After18score","Fiveyearscore","Tenyearscore","Gender","Age_cohort","Rank1")]
str(M_score1)
M_score.T <- M_score1[,c("ID","Totalscore","Rank1","Gender")]
M_score.18 <- M_score1[,c("ID","After18score","Rank1","Gender")]
M_score.5 <- M_score1[,c("ID","Fiveyearscore","Rank1","Gender")]
M_score.10 <- M_score1[,c("ID","Tenyearscore","Rank1","Gender")]

M_score.T$Score.name <- "Total"
M_score.18$Score.name <- "After18"
M_score.5$Score.name <- "Fiveyear"
M_score.10$Score.name <- "Tenyear"

names(M_score.T)[2] <- "Score.value"
names(M_score.18)[2] <- "Score.value"
names(M_score.5)[2] <- "Score.value"
names(M_score.10)[2] <- "Score.value"

M_score2 <- rbind(M_score.T,M_score.18,M_score.5,M_score.10)
M_score2$Gender <- factor(M_score2$Gender,
                          levels = c("Male","Female"),
                          labels = c("Men","Women"))

M_score2$Rank1 <- factor(M_score2$Rank1,
                         levels = c("L","M","H"),
                         labels = c("Low","Medium","High"))

M_score2$Score.name <- factor(M_score2$Score.name,
                              levels = c("Fiveyear","Tenyear","After18","Total"),
                              labels = c("Fiveyear","Tenyear","After18","Total"))

names(M_score2)[3] <- "EconomicRank"

#### Sample characteristics for distant pilgrimage, classified by economic rank

ggplot(data = M_score2, aes(x = Score.name, y = Score.value, fill = EconomicRank))+
  geom_boxplot()

EconomicRank_MEAN <- M_score2 %>%
  group_by(Score.name,EconomicRank)%>%
  summarise(mean.score = mean(Score.value))
EconomicRank_MEAN


EconomicRank_SD <-  M_score2 %>% 
  group_by(Score.name,EconomicRank) %>% 
  summarise(sd.score = sd(Score.value))
EconomicRank_SD

msd <- cbind(EconomicRank_MEAN,EconomicRank_SD)

Mean_SD_by_EconomicRank <- msd[,c("Score.name...1","EconomicRank...2","mean.score","sd.score")]

names(Mean_SD_by_EconomicRank)[1:4] <- c("Score.name","EconomicRank","Mean.Score","SD.Score")
str(Mean_SD_by_EconomicRank) 

### Sample characteristics for distant pilgrimage, classified by gender
ggplot(data = M_score2, aes(x = Score.name, y = Score.value, fill = Gender))+
  geom_boxplot()


Gender_MEAN <- M_score2%>%
  group_by(Gender,Score.name)%>%
  summarise(mean.score = mean(Score.value))
Gender_MEAN

Gender_SD <- M_score2%>%
  group_by(Gender,Score.name)%>%
  summarise(mean.score = mean(Score.value),
            sd.score = sd(Score.value))

names(Gender_SD)[3:4] <- c("Mean.Score","SD.Score")

Mean_SD_by_Gender <- Gender_SD

################

M_score1$Gender <- factor(M_score1$Gender,
                          levels = c("Female","Male"),
                          labels = c("Women","Men"))
M_score1$Age_cohort <- factor(M_score1$Age_cohort,
                              levels = c("16-25","26-35","36-45","46-55",">55"),
                              labels = c("16-25","26-35","36-45","46-55",">55"))
M_score1$Rank1 <- factor(M_score1$Rank1,
                         levels = c("L","M","H"),
                         labels = c("Low","Medium","High"))

names(M_score1)[8] <- "EconomicRank"

fit1 <- lm(Totalscore ~ Gender + Age_cohort + EconomicRank,data = M_score1)
summary(fit1)

ggeff.fit1.1 <- ggpredict(fit1,terms = c("Age_cohort","Gender"))


gf1.1 <- ggplot(ggeff.fit1.1,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.2,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = "Age Cohort", y = "Total Scores") +
  scale_colour_hue("Gender") +
  theme(axis.title.y = element_text(size = 8))


ggeff.fit1.1$group <- factor(ggeff.fit1.1$group,
                             levels = c("Low","Medium","High"),
                             labels = c("Low","Medium","High"))


ggeff.fit1.2 <- ggpredict(fit1,terms = c("Age_cohort","EconomicRank"))

gf1.2 <- ggplot(ggeff.fit1.2,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.2,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = "Age Cohort", y = "Total Score") +
  scale_colour_hue("Economic Rank")+
  theme(axis.title.y = element_text(size = 8))


fit2 <- lm(After18score ~ Gender + Age_cohort + EconomicRank, data = M_score1)
summary(fit2)
ggeff.fit2.1 <- ggpredict(fit2,terms = c("Age_cohort","Gender"))


gf2.1 <- ggplot(ggeff.fit2.1,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.2,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = "Age Cohort", y = "After 18 Scores") +
  scale_colour_hue("Gender")+
  theme(axis.title.y = element_text(size = 8))


ggeff.fit2.2 <- ggpredict(fit2,terms = c("Age_cohort","EconomicRank"))


gf2.2 <- ggplot(ggeff.fit2.2,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.2,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = "Age Cohort", y = "After 18 Scores") +
  scale_colour_hue("Economic Rank")+
  theme(axis.title.y = element_text(size = 8))


fit3 <- lm(Fiveyearscore ~ Gender + Age_cohort + EconomicRank, data = M_score1)
summary(fit3)
ggeff.fit3.1 <- ggpredict(fit3,terms = c("Age_cohort","Gender"))


gf3.1 <- ggplot(ggeff.fit3.1,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.8,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = "Age Cohort", y = "Distant Pilgrimage Scores") +
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "top")+
  scale_colour_manual(name = "Gender",values = c("#FA6A96","#59E0AA"))


ggeff.fit3.2 <- ggpredict(fit3,terms = c("Age_cohort","EconomicRank"))


gf3.2 <-ggplot(ggeff.fit3.2,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.8,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = "Age Cohort",y = "Distant Pilgrimage Scores") +
  scale_colour_hue("Wealth Rank")+
  theme(axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "top")


fit4 <- lm(Tenyearscore ~ Gender + Age_cohort + EconomicRank, data = M_score1)
summary(fit4)
ggeff.fit4.1 <- ggpredict(fit4,terms = c("Age_cohort","Gender"))

gf4.1 <- ggplot(ggeff.fit4.1,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.2,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = NULL, y = "10-Year Scores") +
  scale_colour_hue("Gender")+
  theme(axis.title.y = element_text(size = 8))+
  scale_y_continuous(breaks = round(seq(min(0), max(30), by=5)))


ggeff.fit4.2 <- ggpredict(fit4,terms = c("Age_cohort","EconomicRank"))


gf4.2 <- ggplot(ggeff.fit4.2,aes(x = x, y = predicted, group = group, colour = group)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),size = 0.2,width = 0.15,
                position = position_dodge(0.3)) + 
  geom_point(size = 1.2, position = position_dodge(0.3))+
  labs(x = NULL,y = "10-Year Scores") +
  scale_colour_hue("Economic Rank")+
  theme(axis.title.y = element_text(size = 8))


#######combine plots

ggarrange(gf3.1,gf4.1,gf2.1,gf1.1,ncol = 2,nrow = 2,
          common.legend = T,legend = "top")
### This (Above) is the Figure S1 in Supplementary Materail.


ggarrange(gf3.2,gf4.2,gf2.2,gf1.2,ncol = 2,nrow = 2,
          common.legend = T,legend = "top")
### This (Above) is the Figure 1 in Manuscript.

