pack<-c("car","sandwich","lmtest","RColorBrewer","mgcv","foreign","xtable"
        ,"AER","stargazer", "MASS", "tidyverse",
        "pathwork","ggplot2", "forcats", "rio", "Cairo", "ggpubr", "ggExtra")
lapply(pack, require, character.only=T)

setwd("")
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

#####Laver Deserving om til 1/0 dummy
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

##Modeller med robuste standardfejl

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

##### Fjerner alle rækker fra modelobjekterne der ikke er interaktionen

cim24 <- cim24[8,]

cim21 <- cim21[8,]

cim18 <- cim18[8,]

cim15 <- cim15[8,]

cim12 <- cim12[8,]

cim9 <- cim9[8,]

cim6 <- cim6[8,]

cim3 <- cim3[8,]

##### Grupper dataen sammen
groupall <- cim24 %>% bind_rows(cim21) %>% bind_rows(cim18) %>% 
  bind_rows(cim15) %>% bind_rows(cim12) %>% bind_rows(cim9) %>% bind_rows(cim6) %>% 
  bind_rows(cim3)

groupall$observations_treatment <- c(337,303,270,229,183,118,86,36)
groupall$observations_control <- 579 - groupall$observations_treatment
groupall$n <- 579

groupall <- groupall %>% 
  mutate(coef.names = rep(c("valg24", "valg21", "valg18", "valg15",
                            "valg12", "valg9", "valg6", "valg3")))

groupall$antal <- c(24, 21, 18, 15, 12, 9, 6, 3)

groupall %>% 
  mutate(coef.names=as.factor(coef.names))

##### Det endelige plot %>% 
CairoWin()
png(file="",
    height=15, width=25, units="cm", res=300) 

groupall$legend <- c("95% konfidensinterval")
fig3<- groupall %>% 
  ggplot(aes(antal, 
             ymin= ci.lo,
             y=point,
             ymax=ci.hi, fill=paste(legend, "n når valgdummy = 1", sep="\n\n"))) +
  geom_point(color="#0E86D4", alpha=1, size=3) +
  scale_y_continuous(breaks=seq(-0.5,0.6,0.1)) +
  geom_linerange(size=1, color="#0E86D4", alpha=0.4) +
  geom_hline(linetype='dashed', yintercept=0, alpha=0.8, color="#4f4e4e") +
  geom_segment(aes(x=24.5, y=0.435, xend=22.5, yend=0.435), color="#0E86D4", alpha=0.4)+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.line.x = element_line(color="black", linewidth=0.5),
        axis.title.x  = element_text(size=12),
        legend.text = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color="white"), 
       # panel.grid.minor = element_line(color="light gray"),
        axis.line = element_line(colour = "black"),
        legend.position=c(0.18,0.8),
        legend.box=NULL,
        legend.key.size=unit(0.0, 'cm'),
        panel.border = element_rect(fill=NA, color="black", size=0.5),
        legend.title=element_blank(),
        legend.key = element_rect(fill="#0E86D4", color="white")
        )+
  guides(color="none")+
  coord_cartesian(ylim = c(-0.5, 0.6)) + 
  scale_x_reverse(breaks=c(24,21,18,15,12,9,6,3))+
  labs(title="", y="", x="Måneder til valg")+
  guides(fill = guide_legend(override.aes = list(size=0, color="white")))
fig3
dev.off()


png(file="",
    height=3, width=25, units="cm", res=300) 
ggplot(groupall, aes(x = antal, y=observations_treatment)) + 
  geom_bar(stat="identity", width=0.8, color="black", fill="#0E86D4", alpha=0.6) + 
  scale_x_reverse()+
  geom_label(aes(label=paste(observations_treatment)), nudge_x=0, nudge_y=40, fill=NA,
             label.size=0)+
  theme_void()

dev.off()

png(file="",
    height=3, width=25, units="cm", res=300) 
ggplot(groupall, aes(x = antal, y=observations_control)) + 
  geom_bar(stat="identity", width=0.8, color="black", fill="gray60", alpha=0.6) + 
  scale_x_reverse()+
  geom_label(aes(label=paste(observations_control)), nudge_x=0, nudge_y=40, fill=NA,
             label.size=0)+
  theme_void()
dev.off()

 
  ######Figur over P-værdier
pval<- c(0.75, 0.8685, 0.916, 0.8043, 0.2811, 0.0404, 0.096, 0.0528)
tid  <-c(24, 21, 18, 15, 12, 9, 6, 3)

datafig4 <- data.frame(pval, tid)
alpha_vec <- c(0.0404, 0.096, 0.0528)
datafig4 %>% 
  ggplot(aes(tid, pval, label=round(pval, digits=3))) +
  geom_point(size=1.8, alpha=0.8) +
  geom_text(data=subset(datafig4, pval < 0.1), hjust=0.6, vjust=-0.5, size=2.5)+
  geom_hline(linetype='dashed', yintercept=0.1) +
  scale_x_reverse(breaks=c(24,21,18,15,12,9,6,3))+
  scale_y_continuous(breaks=c(0.1))+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6))+
    labs(title ="Figur 4: Ændring i p-værdier med tid til valg", x="Antal måneder til valg", y= NULL)

#####Deskriptive plots

png(file="",
    height=15, width=25, units="cm", res=300)

databcd %>% 
  filter(inflation < 5.395) %>% 
  mutate(policy =as.factor(Expansion)) %>% 
  mutate(modtager =as.factor(modtager)) %>% 
  mutate(policy =fct_recode(policy, "Generøs"="1", "Restriktiv"="0")) %>%
  group_by(modtager) %>% 
  count(modtager, policy) %>% 
  mutate(prop = n / sum(n)) %>%
  mutate(mean_prop = mean(prop)) %>% 
  mutate(modtager = fct_reorder(modtager, mean_prop)) %>% 
  mutate(modtager = fct_recode(modtager, "Sundhed" = "2",
                               "Senior" = "4",
                               "Arbejdsløshed" = "5",
                               "Immigration" = "3",
                               "Kriminel" = "1")) %>% 
  mutate(modtager =factor(modtager, levels= c("Kriminel",
                                              "Immigration",
                                              "Arbejdsløshed",
                                              "Senior",
                                              "Sundhed"))) %>% 
  ggplot(aes(modtager, prop, fill=policy))+
  geom_bar (stat='identity', position="fill", alpha=0.7, color="white")+  
  coord_flip()+
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=8),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6))+
  labs(title="") +
  labs(x="")+
  labs(y="")+
  scale_fill_manual(values=c("#0E86D4", "#FAC898"), breaks=c("Generøs", "Restriktiv"),
                    name="Policy") +
  guides(fill="none")
dev.off()

## Figur over total mængde generøs/restriktiv på deserving og undeserving opdelt på år
databcd %>% 
  select(year, Expansion, Deserving) %>% 
  mutate(Expansion =as.factor(Expansion)) %>% 
  ggplot(aes(year, fill =Expansion, alpha=1)) +
  geom_bar() +
  facet_wrap(~Deserving) +
  theme_light()

databcd %>% 
  ggplot(aes(dtelection, fill=sum(Expansion))) +
  geom_bar()

## Figur over total mængde, ikke opdelt på år  
databcd %>% 
  mutate(Expansion =as.factor(Expansion)) %>% 
  ggplot(aes(Deserving, fill =Expansion, alpha=1)) +
  geom_bar(width = 0.3) +
  theme_light()

##Figur over total mængde, opdelt i valgperioder 
facetlabels <- c("1"= "Deserving", "0" = "Undeserving")
databcd %>% 
  mutate(Expansion =as.factor(Expansion)) %>% 
  ggplot(aes(comingelection, fill =Expansion)) +
  geom_histogram()+
  theme(axis.text.y   = element_text(size=8),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        #axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6))+
  facet_wrap(~Deserving, labeller=labeller(Deserving=facetlabels)) +
  scale_fill_manual(values=c("grey80", "grey40")) +
  labs(x ="Valgperioder 1975-2019") +
  labs(y="")+
#scale_fill_manual(values=c("grey40", "grey80"), breaks=c("Generøs", "Restriktiv"),
               # name="Policy")
##### Model over dage til valg og sandsynlighed for generøs policy # 

databcd$ldeserving[databcd$Deserving==1] <- "Deserving"
databcd$ldeserving[databcd$Deserving==0] <- "Undeserving"

databcd <- databcd[databcd$inflation < 5.395,]
            
databcd$month <- as.numeric(format(databcd$Date, "%m"))

datadeserving <- databcd[databcd$Deserving==1,]
dataundeserving <- databcd[databcd$Deserving==0,]


expansion_per_month_deserving <- datadeserving %>%
  group_by(round(dtelection/30)) %>%
  summarise(
    expansion_ratio = sum(Expansion == 1)/n()
  )
expansion_per_month_undeserving <- dataundeserving %>%
  group_by(round(dtelection/30)) %>%
  summarise(
    expansion_ratio = sum(Expansion == 1)/n()
  )


expansion_per_month_deserving$ldeserving <- 1
expansion_per_month_undeserving$ldeserving <- 0
deserving_points<- rbind(expansion_per_month_deserving, expansion_per_month_undeserving)
deserving_points$dtelection <- deserving_points$`round(dtelection/30)`*30
  

# Merge the expansion_per_week data frame with the original databcd data frame based on the week column

png(file="",
    height=15, width=25, units="cm", res=300)  

fig2 <- ggplot(data=databcd, 
         aes(dtelection, Expansion, 
             group=ldeserving))+
  geom_line(stat="smooth", se=F, size=1, aes(color=ldeserving), alpha=1)+
  geom_point(alpha=0)+
  geom_point(data=deserving_points,
             aes(x=dtelection, y=expansion_ratio, color=as.factor(ldeserving)), alpha= 0.4)+
  scale_x_reverse()+
  guides(color="none")+
  scale_color_manual(values=c("#f0650e","#0E86D4", "#0E86D4","#f0650e"))+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color="white"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6,),
        legend.position=c(0.08,0.85),
        legend.box=NULL,
        legend.background= element_rect(fill="white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill="white", color="white"))+
  labs(title="",
      x="Dage til valg", y=NULL)
fig2
dev.off()

fig2 <- ggplot(data=databcd, 
               aes(dtelection, Expansion, 
                   group=ldeserving, color=factor(ldeserving)))+
  geom_line(stat="smooth", se=F, size=1, aes(color=ldeserving), alpha=1)+
  geom_point(alpha=0)+
  scale_x_reverse()+
  scale_color_manual(values=c("#0E86D4","#f0650e"))+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill="white"),
        panel.grid.major = element_line(color="white"), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6,),
        legend.position=c(0.08,0.85),
        legend.box=NULL,
        legend.background= element_rect(fill="white"),
        legend.title=element_blank(),
        legend.key = element_rect(fill="white", color="white"))+
  labs(title="",
       x="Dage til valg", y=NULL)

png(file="",
    height=15, width=25, units="cm", res=300)  

ggMarginal(fig2, type="histogram", margins="x", groupColor=TRUE, groupFill=TRUE,
           margin_inner=TRUE, size=3.5)

dev.off()

# Add marginal grob locations  
gt_main = gtable_add_grob(gt_main, 
                          marginal = gt_marg$grobs[[which(gt_marg$layout$name == "x")]], 
                          t = 3, l = 3)  

# Draw modified gtable  
grid.draw(gt_main)


dev.off()



databcd <- subset(databcd, !is.na(databcd$ddeserving))

databcd$ldeserving[databcd$ddeserving==1] <- "Deserving"
databcd$ldeserving[databcd$ddeserving==0] <- "Undeserving"

databcd %>% 
  filter(inflation < 5.395) %>% 
  ggplot(aes(dtelection, Expansion, 
             group=as.factor(ldeserving),
             linetype=as.factor(ldeserving)))+
  geom_point(alpha=0)+
  scale_x_reverse()+
  scale_y_continuous(breaks=seq(0,1,0.1))+
  geom_smooth(se=F, color="black", size=1)+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6,),
        legend.position=c(0.8,0.22),
        legend.box=NULL,
        legend.title=element_blank(),
        legend.key = element_rect(fill="white", color="white"))+
  labs(title="FigurX: Sammenhæng mellem
sandsynlighed for generøs policy
og antal dage til valg",
       x="Dage til valg", y=NULL)

  ##Regressionstabel

m24uk <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg24,
     data=.)

m3uk <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg3,
     data=.)

stargazer(m24uk, m24, m3uk, m3, align=T, type="text", out="starfull.html")

############Figur 3 for Undeserving

databcd$udeserving<-1
databcd$udeserving[databcd$Deserving=="1"]<-0

##

m24 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg24
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m21 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg21
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m18 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg18 
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m15 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg15
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m12 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg12
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m9 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg9
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m6 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg6
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

m3 <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ udeserving*valg3 
     +lagunemp + laginflation + laggdpgr + lagdebt,
     data=.)

rtest <- data.frame(c('m24', 'm21', 'm18', 'm15', 'm12', 'm9', 'm6', 'm3'))

##Modeller med robuste standardfejl

m24<-coeftest(m24, vcov=vcovHC(m24, type="HC1"))

m21<-coeftest(m21, vcov=vcovHC(m21, type="HC1"))

m18<-coeftest(m18, vcov=vcovHC(m18, type="HC1"))

m15<-coeftest(m15, vcov=vcovHC(m15, type="HC1"))

m12<-coeftest(m12, vcov=vcovHC(m12, type="HC1"))

m9<-coeftest(m9, vcov=vcovHC(m9, type="HC1"))

m6<-coeftest(m6, vcov=vcovHC(m6, type="HC1"))

m3<-coeftest(m3, vcov=vcovHC(m3, type="HC1"))

#####Konfidensintervaller

cim24<-confint(m24, "udeserving:valg24", 0.9)

cim21<-confint(m21, "udeserving:valg21", 0.9)

cim18<-confint(m18, "udeserving:valg18", 0.9)

cim15<-confint(m15, "udeserving:valg15", 0.9)

cim12<-confint(m12, "udeserving:valg12", 0.9)

cim9<-confint(m9, "udeserving:valg9", 0.9)

cim6<-confint(m6, "udeserving:valg6", 0.9)

cim3<-confint(m3, "udeserving:valg3", 0.9)


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

##### Fjerner alle rækker fra modelobjekterne der ikke er interaktionen

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
  bind_rows(cim3)


groupall <- groupall %>% 
  mutate(coef.names = rep(c("valg24", "valg21", "valg18", "valg15",
                            "valg12", "valg9", "valg6", "valg3")))

groupall$antal <- c(24, 21, 18, 15, 12, 9, 6, 3)

groupall %>% 
  mutate(coef.names=as.factor(coef.names))


##### Det endelige plot %>% 

png(file="",
    height=15, width=25, units="cm", res=300) 

groupall$legend <- c("95% ci")
groupall %>% 
  ggplot(aes(antal, 
             ymin= ci.lo,
             y=point,
             ymax=ci.hi, fill=legend)) +
  geom_point(size= 2, color="black") +
  scale_y_continuous(breaks=c(-0.2, -0.1, 0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6)) +
  geom_linerange(size=0.3, color="black", alpha=1) +
  geom_hline(linetype='dashed', yintercept=0) +
  geom_segment(aes(x=8.5, y=-0.1, xend=6, yend=-0.1))+
  theme(axis.text.y   = element_text(size=10),
        axis.text.x   = element_text(size=10),
        axis.title.y  = element_text(size=14),
        axis.title.x  = element_text(size=12),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.6),
        legend.position=c(0.85,0.14),
        legend.box=NULL,
        legend.key.size=unit(0.0, 'cm'),
        legend.title=element_blank(),
        legend.key = element_rect(fill="white", color="white")
  )+
  scale_x_reverse(breaks=c(24,21,18,15,12,9,6,3))+
  labs(title="Figur 3: Ændring i sandsynligheden for
 generøs policy tæt på valg
 for deserving grupper", x="måneder til valg", y="")+
  guides(fill = guide_legend(override.aes = list(size=0, color="white")))

dev.off()

#####

m24uk <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg24,
     data=.)
m24uk<-coeftest(m24uk, vcov=vcovHC(m24uk, type="HC1"))

m3uk <- databcd %>% 
  filter(inflation < 5.395)%>% 
  lm(Expansion ~ ddeserving*valg3,
     data=.)
m3uk<-coeftest(m3uk, vcov=vcovHC(m3uk, type="HC1"))

stargazer(m24uk, m24, m3uk, m3, align=T, title="Tabel 2", dep.var.labels="Udvidelse",
          dep.var.caption="",
          add.lines = list(c("Økonomiske Kontrol", "Nej","Ja","Nej","Ja"),
                           c("Konstant", "0,389***", "-0,056", "0,427***", "-0,013"),
                           c("Observationer", "579", "579", "579", "579"),
                           c("Justeret r2", "0,1361", "0,1717", "0,1374", "0,1728")),
        omit=c("lagunemp", "laginflation", "laggdpgr", "lagdebt", "Constant"),
          covariate.labels=c("Deserving", "24 måneder", "Deserving*24 måneder",
                             "3 måneder", "Deserving*3 måneder"), decimal.mark=",",
        star.cutoffs=c(.05, .01, .001),
        intercept.bottom=T,
          ci.level=0.95,
           type='text', out="tabel2.pdf")

dev.off()

##Tabel med alle otte
stargazer(m24, m21, m18, m15, m12, m9, m6, m3, align=T,
          add.lines = list(c("Økonomiske Kontrol", "Nej","Ja","Nej","Ja")),
          omit=c("lagunemp", "laginflation", "laggdpgr", "lagdebt", "Constant"),
          type='text', out="ot.html")

##

m3uk <- databcd %>% 
  filter(inflation < 5.395)

test <- subset(databcd, databcd$inflation < 5.395)
table(test$Expansion)
table(databcd$Expansion)

library(gt)
tabelet <-  c("Ændring i nominelt ydelsesniveau",
            "Ændring i aldersgrænse for at modtage ydelse",
             "Ændret strafferamme",
            "Ændrede rettigheder for fængselsindsatte, patienter, asylansøgere eller immigranter",
             "Ændrede krav til dokumentation for at modtage ydelser",
             "Ændrede krav til immigranter eksempelvis krav om danskundervisning",
             "Ændring i skattefradrag for pensionister",
             "Ændring i mulighed for frit sygehusvalg")
tabel <- "Policyændring"

tabelet <- data.frame(tabelet, tabel)

xtabs(tabelet)
