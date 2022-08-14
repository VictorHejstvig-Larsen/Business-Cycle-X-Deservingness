pack<-c("car","sandwich","lmtest","RColorBrewer","mgcv","foreign","xtable"
        ,"AER","stargazer", "MASS", "tidyverse", "ggplot", "forcats", "rio")
lapply(pack, require, character.only=T)

setwd("C:/Users/45533/OneDrive - Aarhus universitet/Statskundskab/Business cycle + deservingness")
databcd<-import("databcd.dta")


#####laver dummy variable over tid til valg 0/1
databcd <- databcd %>% 
  mutate (valg24=case_when( dtelection <= 720 ~ 1,
                            dtelection >720 ~ 0)) %>% 
  mutate (valg21=case_when( dtelection <= 720-90 ~ 1,
                            dtelection >720-90 ~ 0)) %>% 
  mutate (valg18=case_when( dtelection <= 720-180 ~ 1,
                            dtelection >720-180 ~ 0)) %>% 
  mutate (valg15=case_when( dtelection <= 720-270 ~ 1,
                            dtelection >720-270 ~ 0)) %>% 
  mutate (valg12=case_when( dtelection <= 720-360 ~ 1,
                            dtelection >720-360 ~ 0)) %>% 
  mutate (valg9=case_when( dtelection <= 720-450 ~ 1,
                           dtelection >720-450 ~ 0)) %>% 
  mutate (valg6=case_when( dtelection <= 720-540 ~ 1,
                           dtelection >720-540 ~ 0)) %>% 
  mutate (valg3=case_when( dtelection <= 720-630 ~ 1,
                           dtelection >720-630 ~ 0))

#####Laver dem om til numeriske variable
databcd <- databcd %>% 
  mutate (valg24=as.numeric(valg24)) %>% 
  mutate (valg21=as.numeric(valg21)) %>% 
  mutate (valg18=as.numeric(valg18)) %>%
  mutate (valg15=as.numeric(valg15)) %>%
  mutate (valg12=as.numeric(valg12)) %>%
  mutate (valg9=as.numeric(valg9)) %>% 
  mutate (valg6=as.numeric(valg6)) %>% 
  mutate (valg3=as.numeric(valg3)) 

#####Fikser Deserving og Expansion
databcd$ddeserving<-0
databcd$ddeserving[databcd$Deserving=="1"]<-1


#####Model estimation
m24 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg24
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m21 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg21
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m18 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg18 
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m15 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg15
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m12 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg12
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m9 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg9
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m6 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg6
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m3 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg3 
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

rtest <- data.frame(c('m24', 'm21', 'm18', 'm15', 'm12', 'm9', 'm6', 'm3'))

##Modeller med robuste standardfejl ved hjælp af sandwhich pakken

m24<-coeftest(m24, vcov=vcovHC(m24, type="HC1"))

m21<-coeftest(m21, vcov=vcovHC(m21, type="HC1"))

m18<-coeftest(m18, vcov=vcovHC(m18, type="HC1"))

m15<-coeftest(m15, vcov=vcovHC(m15, type="HC1"))

m12<-coeftest(m12, vcov=vcovHC(m12, type="HC1"))

m9<-coeftest(m9, vcov=vcovHC(m9, type="HC1"))

m6<-coeftest(m6, vcov=vcovHC(m6, type="HC1"))

m3<-coeftest(m3, vcov=vcovHC(m3, type="HC1"))

#####Konfidensintervaller

cim24<-confint(m24, "ddeserving:valg24", 0.9)

cim21<-confint(m21, "ddeserving:valg21", 0.9)

cim18<-confint(m18, "ddeserving:valg18", 0.9)

cim15<-confint(m15, "ddeserving:valg15", 0.9)

cim12<-confint(m12, "ddeserving:valg12", 0.9)

cim9<-confint(m9, "ddeserving:valg9", 0.9)

cim6<-confint(m6, "ddeserving:valg6", 0.9)

cim3<-confint(m3, "ddeserving:valg3", 0.9)


##Sæt konfidensintervaller sammen med selve leddet

cim24 <- tibble(
  point = coef(m24),
  ci.hi = (cim24[1,2]),
  ci.lo = (cim24[1,1]),
  labs = names(coef(m24)))

cim21 <- tibble(
  point = coef(m21),
  ci.hi = (cim21[1,2]),
  ci.lo = (cim21[1,1]),
  labs = names(coef(m21)))

cim18 <- tibble(
  point = coef(m18),
  ci.hi = (cim18[1,2]),
  ci.lo = (cim18[1,1]),
  labs = names(coef(m18)))

cim15 <- tibble(
  point = coef(m15),
  ci.hi = (cim15[1,2]),
  ci.lo = (cim15[1,1]),
  labs = names(coef(m21)))

cim12 <- tibble(
  point = coef(m12),
  ci.hi = (cim12[1,2]),
  ci.lo = (cim12[1,1]),
  labs = names(coef(m12)))

cim9 <- tibble(
  point = coef(m9),
  ci.hi = (cim9[1,2]),
  ci.lo = (cim9[1,1]),
  labs = names(coef(m9)))

cim6 <- tibble(
  point = coef(m6),
  ci.hi = (cim6[1,2]),
  ci.lo = (cim6[1,1]),
  labs = names(coef(m6)))

cim3 <- tibble(
  point = coef(m3),
  ci.hi = (cim3[1,2]),
  ci.lo = (cim3[1,1]),
  labs = names(coef(m3)))

##### Fjerner alle rækker der ikke er interaktionen

cim24 <- slice (cim24[8,])

cim21 <- slice(cim21[8,])

cim18 <- slice(cim18[8,])

cim15 <- slice(cim15[8,])

cim12 <- slice(cim12[8,])

cim9 <- slice(cim9[8,])

cim6 <- slice(cim6[8,])

cim3 <- slice(cim3[8,])

##### Grupper dataen sammen
groupall <- cim24 %>% bind_rows(cim21) %>% bind_rows(cim18) %>% 
  bind_rows(cim15) %>% bind_rows(cim12) %>% bind_rows(cim9) %>% bind_rows(cim6) %>% 
  bind_rows(cim3) #%>% bind_rows(line0)


groupall <- groupall %>% 
  mutate(coef.names = rep(c("valg24", "valg21", "valg18", "valg15",
                            "valg12", "valg9", "valg6", "valg3")))

groupall$antal <- c(24, 21, 18, 15, 12, 9, 6, 3)

groupall %>% 
  mutate(coef.names=as.factor(coef.names))

line0 <- 0

##### Selve plottetgroupall %>% 
groupall %>% 
  ggplot(aes(antal, 
             ymin= ci.lo,
             y=point,
             ymax=ci.hi)) +
  geom_point(size= 2) +
  scale_y_continuous("") +
  geom_linerange(size=0.5, color="lightblue4", alpha=0.5) +
  #geom_smooth(colour="black", se = F, size=0.5) +
  geom_hline(linetype='dashed', yintercept=0) +
  theme_bw()+
  scale_x_reverse(breaks=c(24,21,18,15,12,9,6,3))+
  labs(title="Figur 3", x="måneder til valg", y="")


######Figur over P-værdier
pval<- c(0.75, 0.8685, 0.916, 0.8043, 0.2811, 0.0404, 0.096, 0.0528)
tid  <-c(24, 21, 18, 15, 12, 9, 6, 3)

datafig4 <- data.frame(pval, tid)

datafig4 %>% 
  ggplot(aes(tid, pval)) +
  geom_point(size=2) +
  geom_smooth(se=F, color="light grey") +
  theme_light() +
  scale_x_reverse(breaks=c(24,21,18,15,12,9,6,3))+
  scale_y_continuous(breaks=c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
                              0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1))+
  labs(title ="Figur 4: Ændring i p-værdier med tid til valg", x="Antal måneder til valg", y= NULL)

#####Deskriptive plots
databcd %>% 
  mutate(Udv =as.factor(Expansion)) %>% 
  mutate(modtager =as.factor(modtager)) %>% 
  mutate(Udv =fct_recode(Udv, "Generøs"="1", "Restriktiv"="0")) %>%
  group_by(modtager) %>% 
  count(modtager, Udv) %>% 
  mutate(prop = n / sum(n)) %>%
  mutate(mean_prop = mean(prop)) %>% 
  mutate(modtager = fct_reorder(modtager, mean_prop)) %>% 
  mutate(modtager = fct_recode(modtager, "Sundhed" = "Health",
                               "Senior" = "Senior",
                               "Arbejdsløshed" = "Unemployed",
                               "Immigration" = "Immigrant",
                               "Kriminel" = "Criminal")) %>% 
  mutate(modtager =factor(modtager, levels= c("Kriminel",
                                              "Immigration",
                                              "Arbejdsløshed",
                                              "Senior",
                                              "Sundhed"))) %>% 
  ggplot(aes(modtager, prop, fill=Udv))+
  geom_bar (stat='identity', position="fill", alpha=1)+  
  theme_light()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  coord_flip()+
  labs(title="Figur 1: Proportion generøs/restriktiv") +
  labs(x="")+
  labs(y="")+
  scale_fill_manual(values=c("grey90", "grey60"))


## Figur over total mængde generøs/restriktiv på deserving og undeserving opdelt på år

databcd %>% 
  select(year, Expansion, Deserving) %>% 
  mutate(Expansion =as.factor(Expansion)) %>% 
  ggplot(aes(year, fill =Expansion, alpha=1)) +
  geom_bar() +
  facet_wrap(~Deserving) +
  theme_light()

## Figur over total mængde, ikke opdelt på år  
databcd %>% 
  select(Expansion, Deserving) %>% 
  mutate(Expansion =as.factor(Expansion)) %>% 
  ggplot(aes(Deserving, fill =Expansion, alpha=1)) +
  geom_bar(width = 0.3) +
  theme_light()

##Figur over total mængde, opdelt i valgperioder 
databcd %>% 
  select(comingelection, Expansion, Deserving) %>% 
  mutate(Expansion =as.factor(Expansion)) %>% 
  ggplot(aes(comingelection, fill =Expansion)) +
  geom_bar() +
  theme_light() +
  facet_wrap(~Deserving) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_fill_manual(values=c("grey90", "grey60")) +
  guides(fill = "none")+
  labs(x ="Valgperioder 1975-2019", color= "Policygenerøsitet") +
  labs(title="Figur 2: Total mængde policy, fordelt på generøs(mørk)
       og restriktiv(lys)") 

##### Regression over dage til valg # 

r6<- databcd %>% 
  lm (Expansion ~ dtelection + laginflation + laggdpgr + lagunemp + lagdebt, data=.)
summary(r6)

r6 %>% 
  ggplot(aes(dtelection, Expansion))+
  scale_x_reverse()+
  geom_smooth(se=F, color="dark grey") +
  theme_light()+
  labs(title="FigurX: Ændring i sandsynligheden for generøs policy
       med dage til valg",
       x="Dage til valg", y=NULL)