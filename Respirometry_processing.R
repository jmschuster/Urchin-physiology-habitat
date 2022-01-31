# Respirometry Data processing

devtools::install_github("januarharianto/respR")
library(respR) # load the library

setwd("/Users/jmschuster/Desktop/PhD/Fieldwork 2020/Data/Experiment data/Exp3_Urchin_Pop_Bauline_Sept2020/Exp3.2_Urchins_22deg_23092020/")
# Import data and Background resp data for experiment run:
# Urchin
Exp2.1_Urchin1_Barren<-read.csv("Exp2.1_Cha1_StrongyDroe_Barren_07092020.csv")

# Background
BG_Exp2.1<-read.csv("Exp2.1_Cha10_StrongyDroe_BLANK_07092020.csv")

# INSPECT DATA (function also plots data)
# specify which column time and oxygen vals are (default is time=2 and oxy=2)
# NOTE on time evenly spacing: warning is to make the user aware that if they use row numbers 
# for manual operations such as subsetting, the same width may not represent the same time period.
Urchin_Ch1Exp2.1<-inspect(Exp2.1_Urchin1_Barren,time=3,oxygen=7)

# Format date-time (e.g. corrupt file due to software crash messing up mins time); exp5
Urchin_1_Kelp_10deg_a <- format_time(Urchin_1_Kelp_10deg, format = "dmyHMS",time=2)

# PROCESS BACKGROUND RESPIRATION (applies for all channels of that experiment)
# limiting data points incl from 5-40 mins
bg_exp2.1 <- calc_rate.bg(BG_Exp2.1,time=3,oxygen=7)
print(bg_exp2.1) # average bg rate applies only if using several background respiration chambers (e.g. if using average of all my background runs)

# CALCULATE OXYGEN UPTAKE RATE using inspect object
# Analysis at this point is unitless! 
calc_rate(Urchin_Ch1Exp6) # uses entire df for lin regression when no additional arguments are called
# to determine rate over a subset of data, for a set time period, or within two O2 thresholds:
# subset data in one of four ways, based on 'from' and 'to' commands:
calc_rate(Urchin_Ch1Exp1,by='time', from=4,to=29) # What is the rate over a specific 25 minute period?
calc_rate(Urchin_Ch1Exp1,by='O2',from=95,to=85) # At what rate is O2 consumed btw saturation points 95% and 85%?
calc_rate(Urchin_Ch1Exp1,by='proportion') # What is the rate from 4/5ths (0.8) to halfway (0.5) along the data?
calc_rate(Urchin_Ch1Exp1,by='row',from=5,to=2000) # Determine rate between two rows (e.g. row 5 and 2000)

# store object
rate_Exp1Ch1<-calc_rate(Urchin_Ch1Exp1,by='time', from=4,to=29)
print(rate_Exp1Ch1)
summary(rate_Exp1Ch1)
plot(rate_Exp1Ch1)

# ADJUST FOR BACKGROUND RESPIRATION:
adj.rate_Exp1Ch1 <- adjust_rate(rate_Exp1Ch1, bg_exp1)

# CONVERT RESULTS 
# so far, haven't required units of time or O2, now convert to final units
# 1. VO2 - total change in O2 per unit time within chamber
# 2. MO2 - mass-specific rate of change in O2 per unit time of specimen
# conversion function requires units of original data (O2 and time units), body mass (kg) and chamber volume (L)
# NOTE volume = volume of water in chamber after adjusting for displacement by animal (i.e. Vol empty- Vol disp)

# Convert to VO2:
respR::convert_rate(adj.rate_Exp1Ch1, 
             o2.unit = "%", 
             time.unit = "m", 
             mass=0.0673,
             output.unit = "ml/h/g", S=27.7,
             t=Urchin_1_Barren_14deg$Temperature,
             volume = 0.6)

# Whole procedure as a pipe:
# CHANGE:
# DATA FILE, VOL, MASS, TEMP FILE, BACKGROUND FILE
setwd("/Users/jmschuster/Desktop/PhD/Fieldwork 2020/Data/Experiment data/Exp3_Urchin_Pop_Bauline_Sept2020/Exp3.8_Urchins_6deg_28092020/")

BG_Exp3.8<-read.csv("Exp3.8_Cha10_StrongyDroe_BLANK_28092020.csv") # background resp data
Exp3.8_Urchin7<-read.csv("Exp3.8_Cha7_StrongyDroe_Barren_28092020.csv") # urchin resp data

rate<-Exp3.8_Urchin9 %>%                               # With the data object,
  inspect(3, 7) %>%                                     # inspect, then
  calc_rate(from = 96, to = 85, by = "O2") %>%          # calculate rate, then
  print() %>%
  adjust_rate(
    calc_rate.bg(BG_Exp3.8,time=3,oxygen=7)) %>%
  print() %>%
  convert_rate(o2.unit = "%", time.unit = "m",     
               output.unit = "ml/h/g", volume = 0.61, mass = 0.044432,S=26.8, t=Exp3.8_Urchin9$Temperature) # convert units.

rate<-Exp1.3_Urchin9 %>%                               # With the data object,
  inspect(3, 7) %>%                                     # inspect, then
  calc_rate(from = 10, to = 80, by = "time")

summary(rate) # gives R2 (rsquared value); can also use plot(rate)

### MODIFIED FOR SCREWY EXP 5 (time mess)
Exp1.5_Urchin9 %>%                               # With the data object,
  inspect(3, 8) %>%                                     # inspect, then
  calc_rate(from = 4000, to = 6000, by = "time") %>%          # calculate rate, then
  print() %>%
  adjust_rate(
    calc_rate.bg(BG_Exp1.5,time=3,oxygen=8)) %>%
  print() %>%
  convert_rate(o2.unit = "%", time.unit = "s",     
               output.unit = "ml/h/g", volume = 0.6, mass =0.065702,S=26.8, t=Exp1.5_Urchin9$Temperature) # convert units.


###########################
####################
#########
### URCHIN RESP EXP 1-3 Metadata (FieldFresh) ACUTE

#### PLOT RESPIRATION RESULTS
setwd("/Users/jmschuster/Desktop/PhD/Fieldwork 2020/Data/Experiment data/")
Urchin_Resp_Metadata<-read.csv('Metadata_Series1_Urchins_FieldFresh.csv')
Urchin_Resp_MetadataR2<-read.csv('Metadata_Series1_Urchins_FieldFresh_RSquared.csv')

## SUPP FIG S9A
pdf(width = 11, useDingbats=TRUE,height = 5, bg="white", file="Urchin_MR_MO2_Sites")

ggplot(subset(Urchin_Resp_Metadata, Experiment!="2.2"), aes(x=as.factor(Temperature_C), y=VO2_AFDW_ml.h,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h)*''))+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.1,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+facet_wrap(~Location,labeller = labeller(Location = Exp.labs2))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="Absolute")+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26,28, 30),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28, 30),drop=FALSE)

dev.off()

## SUPP FIG S9B
pdf(width = 11, useDingbats=TRUE,height = 5, bg="white", file="Urchin_MR_WetWeight")

ggplot(subset(Urchin_Resp_Metadata, Experiment!="2.2"), aes(x=as.factor(Temperature_C), y=MR_WetWeight_ml.h.g,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''))+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.1,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+facet_wrap(~Location,labeller = labeller(Location = Exp.labs2))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="Wet Mass")+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26,28, 30),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28, 30),drop=FALSE)

dev.off()

### SUPP FIG S9C
pdf(width = 11, useDingbats=TRUE,height = 5, bg="white", file="Urchin_MR_AFDM")

ggplot(subset(Urchin_Resp_Metadata, Experiment!="2.2"), aes(x=as.factor(Temperature_C), y=MR_AFDW_ml.h.g,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''))+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.1,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+facet_wrap(~Location,labeller = labeller(Location = Exp.labs2))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="AFDM")+
  scale_x_discrete(breaks=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28, 30),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28, 30),drop=FALSE)

dev.off()



# GAM
Urchin_Resp_Metadata$Habitat<-as.factor(Urchin_Resp_Metadata$Habitat)
Model_DF_Resp_Acute<-Urchin_Resp_Metadata %>% filter(Habitat !='Blank') %>% droplevels()
UM3c<-gam(MR_AFDW_ml.h.g~s(Temperature_C,by=Habitat,k=3),data=subset(Model_DF_Resp_Acute,Location=='Tors Cove'),na.action=na.omit)
UM4<-gam(VO2_AFDW_ml.h~s(Temperature_C,by=Habitat,k=3),data=subset(Model_DF_Resp_Acute,Location=='Biscayan Cove'),na.action=na.omit)

GAM3c<-plot_smooths(
  model = UM3c,
  series = Temperature_C,
  comparison = Habitat)+labs(title="AFDW",tag="A")+ylab('Metabolic Rate (mlO2/h/g)')+xlab("Temperature (°C)")+scale_x_continuous(limits=c(4,30),breaks=c(4,6,8,10,12,14,16,18,20,22,24,26,28,30))+
  theme(legend.position = "bottom")

GAM3

# PLOT URCHIN WEIGHTS

Urchin_Resp_Metadata$Dry_Weight_g<-(Urchin_Resp_Metadata$Dry.weight.w.boat-Urchin_Resp_Metadata$Empty.weigh.boat)
Urchin_Resp_TPCs$Dry_Weight_g<-(Urchin_Resp_TPCs$Dry.weight.w.boat-Urchin_Resp_TPCs$Empty.weigh.boat)

### ALL WEIGHTS

### SUPP FIG S3A
pdf(width = 11, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Overall_WetWeights")

Urchin_Resp_Metadata %>% filter(Habitat!= 'Blank') %>% ggplot(aes(x=Habitat, y=Wet_weight_g,fill=Habitat))+geom_boxplot(outlier.shape=NA)+xlab('Habitat')+ylab('Wet Mass (g)')+theme_bw(base_size=15)+ylim(c(0,120))+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Location,labeller = labeller(Location = Exp.labs3),scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))

dev.off()

### SUPP FIG S3B
Urchin_Resp_Metadata$Dry_weight_g<-Urchin_Resp_Metadata$Dry.weight.w.boat-Urchin_Resp_Metadata$Empty.weigh.boat
Urchin_Resp_Metadata$Ash_weight_g<-Urchin_Resp_Metadata$Ashed.weight.w.boat-Urchin_Resp_Metadata$Empty.weigh.boat

pdf(width = 11, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Overall_DryWeights")

Urchin_Resp_Metadata %>% filter(Habitat!= 'Blank') %>% ggplot(aes(x=Habitat, y=Dry_weight_g,fill=Habitat))+geom_boxplot(outlier.shape=NA)+xlab('Habitat')+ylab('Dry Mass (g)')+theme_bw(base_size=15)+ylim(c(0,40))+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Location,labeller = labeller(Location = Exp.labs4),scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

dev.off()

### SUPP FIG S3C
pdf(width = 11, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Overall_AFDWWeights")

Urchin_Resp_Metadata %>% filter(Habitat!= 'Blank') %>% ggplot(aes(x=Habitat, y=AFDW_g,fill=Habitat))+geom_boxplot(outlier.shape=NA)+xlab('Habitat')+ylab('AFDM (g)')+theme_bw(base_size=15)+ylim(c(0,10))+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Location,labeller = labeller(Location = Exp.labs5),scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))

dev.off()

### SUPP FIG S3D
pdf(width = 11, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Overall_InorganicWeights")

Urchin_Resp_Metadata %>% filter(Habitat!= 'Blank') %>% ggplot(aes(x=Habitat, y=Ash_weight_g,fill=Habitat))+geom_boxplot(outlier.shape=NA)+xlab('Habitat')+ylab('Inorganic Mass (g)')+theme_bw(base_size=15)+ylim(c(0,40))+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Location,labeller = labeller(Location = Exp.labs6),scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)))

dev.off()


# PLOT ratio of wet:AFD weight vs temp

### FIG 4 Weight Ratios
pdf(width = 5, useDingbats=TRUE,height = 4.5, bg="white", file="Weights Ratios")

ggplot(subset(Urchin_Resp_Metadata),aes(x=as.factor(Temperature_C), y=(AFDW_g/Wet_weight_g),fill=Habitat))+geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+ylab('AFDM:Wet mass ratio')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28, 30),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28, 30),drop=FALSE)+ylim(c(0,0.2))

dev.off()

### SUPP FIG S4 Weight Ratios without temp
library(ggsignif)
pdf(width = 11, useDingbats=TRUE,height = 4.5, bg="white", file="Supp_FigS4")

ggplot(subset(Urchin_Resp_Metadata, Habitat!='Blank'),aes(x=Habitat, y=(AFDW_g/Wet_weight_g),fill=Habitat))+geom_boxplot(outlier.shape=NA)+xlab('Habitat')+ylab('AFDM:Wet mass ratio')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.1,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Location,labeller = labeller(Location = Exp.labs2),scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

dev.off()

#########################
######################
###################
###########
# TPC analysis

# Whole procedure as a pipe:
# CHANGE:
# DATA FILE, VOL, MASS, TEMP FILE, BACKGROUND FILE
setwd("/Users/jmschuster/Desktop/PhD/Fieldwork 2020/Data/Experiment data/Exp 4 TPC Oct 2020/Exp4.3_Urchin_TPC")

BG_Exp4.3<-read.csv("Exp4.3_Ch10_BLANK_TPC_09112020.csv") # background resp data
Exp4.3_Urchin9<-read.csv("Exp4.3_Ch9_Urchin_TPC_09112020.csv") # urchin resp data

BG_Exp4.3s9<-subset(BG_Exp4.3, Delta.T..min. >= 514 & Delta.T..min. <= 528) # subset background resp for each temp
Exp4.3_Urchin9_s9<-subset(Exp4.3_Urchin9, Delta.T..min. >= 514 & Delta.T..min. <= 528) # subset background resp for each temp

Exp4.3_Urchin9_s9 %>%                               # With the data object,
  inspect(3, 7) %>%                                     # inspect, then
  calc_rate(from = 514, to = 528, by = "time") %>%          # calculate rate, then
  print() %>%
  adjust_rate(
    calc_rate.bg(BG_Exp4.3s9,time=3,oxygen=7)) %>%
  print() %>%
  convert_rate(o2.unit = "%", time.unit = "m",     
               output.unit = "ml/h/g", volume = 0.55, mass = 0.098632,S=28, t=Exp4.3_Urchin9_s9$Temperature) # convert units.

rate<-Exp4.3_Urchin9_s9 %>%                               # With the data object,
  inspect(3, 7) %>%                                     # inspect, then
  calc_rate(from = 100, to = 85, by = "O2") 

summary(rate) # gives R squared value of regression
### Volumetric O2 consumption (disregarding organism weight)

Exp2.1_Urchin9 %>%                               # With the data object,
  inspect(3, 7) %>%                                     # inspect, then
  calc_rate(from = 10, to = 80, by = "time") %>%          # calculate rate, then
  print() %>%
  adjust_rate(
    calc_rate.bg(BG_Exp2.1,time=3,oxygen=7)) %>%
  print() %>%
  convert_rate(o2.unit = "%", time.unit = "m",     
               output.unit = "ml/h", volume = 0.61,S=26.8, t=Exp2.1_Urchin9$Temperature) # convert units.


#### PLOT TPCs

#### PLOT RESPIRATION TPC RESULTS
setwd("/Users/jmschuster/Desktop/PhD/Fieldwork 2020/Data/Experiment data/")
Urchin_Resp_TPCs<-read.csv('Metadata_Series2_Urchins_TPCs.csv')
Urchin_Resp_TPCsOG<-read.csv('Metadata_Series2_Urchins_TPCs_Original.csv')

# Creating Facet labels
Exp.labs <- c("Bauline - acclimated", "Biscayan Cove - acclimated", 'Biscayan Cove - realized')
names(Exp.labs) <- c("4.1", "4.2", '4.3')

Exp.labs2 <- c("A. Bauline", "B. Biscayan Cove", 'C. Tors Cove')
names(Exp.labs2) <- c("Bauline", "Biscayan Cove", 'Tors Cove')

Exp.labs3 <- c("A", "B", 'C')
names(Exp.labs3) <- c("Bauline", "Biscayan Cove", 'Tors Cove')
Exp.labs4 <- c("D", "E", 'F')
names(Exp.labs4) <- c("Bauline", "Biscayan Cove", 'Tors Cove')
Exp.labs5 <- c("G", "H", 'I')
names(Exp.labs5) <- c("Bauline", "Biscayan Cove", 'Tors Cove')
Exp.labs6 <- c("J", "K", 'L')
names(Exp.labs6) <- c("Bauline", "Biscayan Cove", 'Tors Cove')


### FIG 3 TPC MO2
VO2_TPC<-ggplot(subset(Urchin_Resp_TPCs,Experiment=='4.3'), aes(x=as.factor(Temp), y=VO2_AFDW_ml.h, fill=Habitat))+ geom_rect(xmin=0, xmax=6, ymin=0, ymax=Inf, fill='lightgrey') + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+labs(title="A")+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h)*''))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28,30,32),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28,30,32),drop=FALSE)


VO2_TPCi<-ggplot(TPC_BIS, aes(x=Temp, y=VO2_AFDW_ml.h)) + 
  geom_line(aes(group=Replicate_Chamber,colour=Habitat,linetype=Habitat),size=1)+geom_point(aes(shape=Habitat,fill=Habitat),size=3)+xlab('Temperature (°C)')+theme_bw(base_size=15)+
  theme(legend.position='none',legend.title=element_text(size=16,face='bold'), legend.text=element_text(size=14),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_colour_manual(values=c("goldenrod2", "#669933"))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+labs(title="B")+labs(fill="Habitat", colour="Urchin")+scale_fill_manual(values=c('white','black','grey'))+
  scale_x_continuous(limits=c(3, 33),breaks=c(4, 6,8, 10,12, 14,16, 18,20, 22,24, 26,28, 30, 32),labels=c(4, 6,8, 10,12, 14,16, 18, 20,22,24, 26, 28,30, 32))+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h)*''))

pdf(width = 9, useDingbats=TRUE,height = 5, bg="white", file="Urchin_VO2_TPC")

ggarrange(VO2_TPC,NULL,VO2_TPCi, 
          nrow = 1, widths = c(1, 0.03, 1),align = "v",common.legend = TRUE,legend='bottom')

dev.off()
### FIG 2 ACUTE MO2 per Site

VO2_Acute<-ggplot(subset(Urchin_Resp_MetadataR2,rsquared>0.98), aes(x=as.factor(Temperature_C), y=VO2_AFDW_ml.h, fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+theme_bw(base_size=15)+facet_wrap(~Location,labeller = labeller(Location = Exp.labs2),scale='free_y')+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.07,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28,30,32),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28,30,32),drop=FALSE)+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h)*''))

# labs(title=bquote(''*MO[2]*' Acute Temperature Exposure'))+
pdf(width = 12, useDingbats=TRUE,height = 4.5, bg="white", file="Urchin_VO2_Acute") # size for site plot
pdf(width = 5, useDingbats=TRUE,height = 4.5, bg="white", file="Urchin_VO2_Acute") # size for overall plot

VO2_Acute
dev.off()
### FIG 2 ACUTE MO2 Overall

VO2_Acute<-ggplot(subset(Urchin_Resp_MetadataR2,rsquared>0.98), aes(x=as.factor(Temperature_C), y=VO2_AFDW_ml.h, fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.25,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+labs(title="A")+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28,30,32),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28,30,32),drop=FALSE)+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h)*''))

### FIG 2 ACUTE MO2 Overall MASS-INDEPENDENT

VO2_MI_Acute<-ggplot(Acute_sub, aes(x=as.factor(Temperature_C), y=res.VO2.WW, fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.25,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+labs(title="B")+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28,30,32),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28,30,32),drop=FALSE)+labs(y=bquote(' .\nM'*O[2]*~(standardized)*''))

pdf(width = 9, useDingbats=TRUE,height = 5, bg="white", file="Urchin_VO2_Acute_NEW")

ggarrange(VO2_Acute,NULL,VO2_MI_Acute, 
          nrow = 1, widths = c(1, 0.03, 1),align = "v",common.legend = TRUE,legend='bottom')

dev.off()


### INDIVIDUAL TPCs

color_Hab <- c("white","grey",'black')
TPC_BIS<-subset(Urchin_Resp_TPCsOG,Experiment=='4.3') # use original Urchin_Resp_TPCs file (without added continuous temps)
TPC_BIS <- TPC_BIS %>% filter(Habitat != 'Blank') %>% droplevels()
TPC_BIS$Replicate_Chamber<-as.factor(TPC_BIS$Replicate_Chamber)

### COMPOSITE PLOT FOR BISCAYAN COVE FIELD_FRESH TPCs ONLY
### SUPP FIG S10
Biscayan_FF_WW<-ggplot(subset(Urchin_Resp_TPCs,Experiment=='4.3'), aes(x=as.factor(Temp), y=MR_WetWeight_ml.h.g, fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''))+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="TRC - Wet Mass",tag='A')

WW_TPCi<-ggplot(TPC_BIS, aes(x=Temp, y=MR_WetWeight_ml.h.g)) + 
  geom_line(aes(group=Replicate_Chamber,colour=Habitat,linetype=Habitat),size=1)+geom_point(aes(shape=Habitat,fill=Habitat),size=3)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''))+
  theme_bw(base_size=15)+theme(legend.position='none',legend.title=element_text(size=16,face='bold'),
                               legend.text=element_text(size=14),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="",tag='B')+labs(fill="Habitat", colour="Urchin")+scale_colour_manual(values=c("goldenrod2", "#669933"))+scale_fill_manual(values=c('white','black','grey'))+
  scale_x_continuous(limits=c(3, 33),breaks=c(4, 6,8, 10,12, 14,16, 18,20, 22,24, 26,28, 30, 32),labels=c(4, 6,8, 10,12, 14,16, 18, 20,22,24, 26, 28,30, 32))

AFDW_TPCi<-ggplot(TPC_BIS, aes(x=Temp, y=MR_AFDW_ml.h.g)) + 
  geom_line(aes(group=Replicate_Chamber,colour=Habitat,linetype=Habitat),size=1)+geom_point(aes(shape=Habitat,fill=Habitat),size=3)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''))+
  theme_bw(base_size=15)+theme(legend.position='none',legend.title=element_text(size=16,face='bold'),
                  legend.text=element_text(size=14),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="",tag='D')+labs(fill="Habitat", colour="Urchin")+scale_colour_manual(values=c("goldenrod2", "#669933"))+scale_fill_manual(values=c('white','black','grey'))+
  scale_x_continuous(limits=c(3, 33),breaks=c(4, 6,8, 10,12, 14,16, 18,20, 22,24, 26,28, 30, 32),labels=c(4, 6,8, 10,12, 14,16, 18, 20,22,24, 26, 28,30, 32))


Biscayan_FF_AFDW<-ggplot(subset(Urchin_Resp_TPCs,Experiment=='4.3'), aes(x=as.factor(Temp), y=MR_AFDW_ml.h.g, fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''))+theme_bw(base_size=15)+
  scale_fill_manual(values=c("goldenrod2", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="TRC - AFDM",tag='C')

pdf(width = 9, useDingbats=TRUE,height = 5, bg="white", file="Biscayan_TPC_Composite_WetMass")

ggarrange(Biscayan_FF_WW, WW_TPCi, 
          nrow = 1, ncol = 2, align = "v",common.legend = TRUE,legend='bottom')

dev.off()
# AFDW
pdf(width = 9, useDingbats=TRUE,height = 5, bg="white", file="Biscayan_TPC_Composite_AFDW")

ggarrange(Biscayan_FF_AFDW, AFDW_TPCi, 
          nrow = 1, ncol = 2, align = "v",common.legend = TRUE,legend='bottom')

dev.off()

# GAM 
Model_DF_Resp_TPC<-Urchin_Resp_TPCsOG %>% filter(Habitat !='Blank') %>% droplevels()
Model_DF_Resp_TPC$Habitat<-as.factor(Model_DF_Resp_TPC$Habitat)
UM5<-gam(MR_AFDW_ml.h.g~s(Temp,by=Habitat,k=5),data=subset(Model_DF_Resp_TPC,Experiment=='4.3'),na.action=na.omit)
UM6<-gam(VO2_AFDW_ml.h~s(Temp,by=Habitat,k=7),data=subset(Model_DF_Resp_TPC,Experiment=='4.3'),na.action=na.omit)

GAM6<-plot_smooths(
  model = UM6,
  series = Temp,
  comparison = Habitat)+labs(title="VO2",tag="A")+ylab('Metabolic Rate (mlO2/h)')+xlab("Temperature (°C)")+scale_x_continuous(limits=c(4,32),breaks=c(4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))+
  theme(legend.position = "bottom")

GAM6

### COMPARING METABOLIC RATES BETWEEN RAMP/ACUTE
TPC_vs_Acute<-read.csv('TPC_vs_Acute.csv')
TPC_vs_Acute$Replicate<-as.factor(TPC_vs_Acute$Replicate)
TPC_vs_Acute$Rep_2<-as.factor(TPC_vs_Acute$Rep_2)


Rate_Comp_AFDW<-ggplot(subset(TPC_vs_Acute,Temp<30), aes(x=as.factor(Temp), y=MR_AFDW,fill=Treatment)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h / g)*''))+theme_bw(base_size=15)+
  theme(legend.position=c(0.1,0.7),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="AFDM",tag="C")+scale_fill_manual(labels = c("Acute", "Ramping"), values = c("coral", "darkturquoise"))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28),drop=FALSE)

Rate_Comp_WW<-ggplot(subset(TPC_vs_Acute,Temp<30), aes(x=as.factor(Temp), y=MR_WW,fill=Treatment)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h / g)*''))+theme_bw(base_size=15)+
  theme(legend.position=c(0.1,0.7),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title="Wet Mass",tag="B")+scale_fill_manual(labels = c("Acute", "Ramping"), values = c("coral", "darkturquoise"))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28),drop=FALSE)

Rate_Comp_VO2<-ggplot(subset(TPC_vs_Acute,Temp<30), aes(x=as.factor(Temp), y=VO2,fill=Treatment)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]*~(mL~O[2] / h)*''))+theme_bw(base_size=15)+
  theme(legend.position=c(0.1,0.7),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title='Absolute',tag='A')+scale_fill_manual(labels = c("Acute", "Ramping"), values = c("coral", "darkturquoise"))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28),drop=FALSE)

Rate_Comp_MassIndep<-ggplot(subset(TPC_vs_Acute,Temp<30), aes(x=as.factor(Temp), y=MR_MassIndep,fill=Treatment)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Temperature (°C)')+labs(y=bquote(' .\nM'*O[2]*~(standardized)*''))+theme_bw(base_size=15)+
  theme(legend.position=c(0.1,0.7),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"),axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))+
  labs(title='Mass-independent',tag='B')+scale_fill_manual(labels = c("Acute", "Ramping"), values = c("coral", "darkturquoise"))+
  scale_x_discrete(breaks=c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24,26,28),labels=c(4, 6,8, 10, 12, 14, 16, 18,20, 22, 24,26,28),drop=FALSE)

### FIG 6
pdf(width = 8.5, useDingbats=TRUE,height = 4.5, bg="white", file="Acute_vs_TPC")

ggarrange(Rate_Comp_VO2, Rate_Comp_MassIndep,
          nrow = 1, ncol = 2, align = "v",common.legend = TRUE,legend='bottom')

dev.off()

### Fitting a curve for each treatment
TPC_vs_Acute$Treatment<-as.factor(TPC_vs_Acute$Treatment)
UM6<-gamm(MR_AFDW~s(Temp,by=Treatment,k=5),random=list(Rep_2=~1),data=subset(TPC_vs_Acute,Temp<30),na.action=na.omit)
UM6pois<-gamm(MR_AFDW~s(Temp,by=Treatment,k=5),random=list(Rep_2=~1),data=subset(TPC_vs_Acute,Temp<30),na.action=na.omit,family='poisson')
UM7<-gamm(MR_MassIndep~s(Temp,by=Treatment,k=5),random=list(Treatment=~1,Rep_2=~1),data=subset(TPC_vs_Acute,Temp<30),na.action=na.omit)
UM8<-gamm(MR_WW~s(Temp,by=Treatment,k=5),random=list(Treatment=~1,Rep_2=~1),data=subset(TPC_vs_Acute,Temp<30),na.action=na.omit)
UM9<-gamm(VO2~s(Temp,by=Treatment,k=5),random=list(Treatment=~1,Rep_2=~1),data=subset(TPC_vs_Acute,Temp<30),na.action=na.omit)

UM4lm<-lm(VO2~Temp*Treatment,data=subset(TPC_vs_Acute,Temp<30),na.action=na.omit)

library(tidymv)

theme_set(theme_bw(base_size=15))


#### SUPP FIG S8 TPC vs Acute gamms
pdf(width = 11, useDingbats=TRUE,height = 10, bg="white", file="TPC_vs_Acute_gam")

par(mfrow=c(2,2))
par(mar=c(4,5.5,3,0.5),oma=c(1,1,1,1))

plot(UM9$gam,select=1,shade=TRUE,shade.col=rgb(255,0,0,60,maxColorValue=255),col="coral",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]~(mL ~ O[2] / h)*''),cex.main=1.6,rug=FALSE,ylim=c(-1.5,1.5))
axis(side=2,at=c(-2,-1,0,1,2),cex.axis=1.5)
par(new=TRUE)
plot(UM9$gam,shade=TRUE,select=2,shade.col=rgb(0,100,255,60,maxColorValue=255),col="darkcyan",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-1.5,1.5))
text(5,1.21,"A",cex=1.8)
#title(sub="A", adj=0, line=-15, font=2,cex.sub=1.8)
legend("bottomright", c("Acute","Ramping"), lwd=3,col=c("coral", "darkcyan"), bty="n", cex=1.5)
mtext('Absolute', side=3, line=0.5, font=1,cex=1.5,adj=0)

plot(UM7$gam,select=1,shade=TRUE,shade.col=rgb(255,0,0,60,maxColorValue=255),col="coral",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]*~(standardized)*''),cex.main=1.6,rug=FALSE,ylim=c(-1,1))
axis(side=2,at=c(-1,0,1),cex.axis=1.5)
par(new=TRUE)
plot(UM7$gam,shade=TRUE,select=2,shade.col=rgb(0,100,255,60,maxColorValue=255),col="darkcyan",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-1,1))
text(5,0.85,"B",cex=1.8)
legend("bottomright", c("Acute","Ramping"), lwd=3,col=c("coral", "darkcyan"), bty="n", cex=1.5)
mtext('Mass-independent', side=3, line=0.5, font=1,cex=1.5,adj=0)

plot(UM8$gam,select=1,shade=TRUE,shade.col=rgb(255,0,0,60,maxColorValue=255),col="coral",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''),cex.main=1.6,rug=FALSE,ylim=c(-0.03,0.03))
axis(side=2,at=c(-0.02,0,0.02),cex.axis=1.5)
par(new=TRUE)
plot(UM8$gam,shade=TRUE,select=2,shade.col=rgb(0,100,255,60,maxColorValue=255),col="darkcyan",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-0.03,0.03))
text(5,0.026,"C",cex=1.8)
legend("bottomright", c("Acute","Ramping"), lwd=3,col=c("coral", "darkcyan"), bty="n", cex=1.5)
mtext('Wet Mass', side=3, line=0.5, font=1,cex=1.5,adj=0)

plot(UM6$gam,select=1,shade=TRUE,shade.col=rgb(255,0,0,60,maxColorValue=255),col="coral",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]~(mL ~ O[2] / h / g)*''),cex.main=1.6,rug=FALSE,ylim=c(-0.5,0.5))
axis(side=2,at=c(-0.3,0,0.3),cex.axis=1.5)
par(new=TRUE)
plot(UM6$gam,shade=TRUE,select=2,shade.col=rgb(0,100,255,60,maxColorValue=255),col="darkcyan",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-0.5,0.5))
text(5,0.4,"D",cex=1.8)
legend("bottomright", c("Acute","Ramping"), lwd=3,col=c("coral", "darkcyan"), bty="n", cex=1.5)
mtext('AFDM', side=3, line=0.5, font=1,cex=1.5,adj=0)

dev.off()

### FIG 9
pdf(width = 11, useDingbats=TRUE,height = 4, bg="white", file="Acute_vs_TPC_GAM")

ggarrange(GAM2, GAM3, GAM1,
          nrow = 1, ncol = 3,
          align = "v",common.legend = TRUE,legend='bottom')

dev.off()

#### Supplementary Figure Rsquared vs MO2
pdf(width = 5, useDingbats=TRUE,height = 4, bg="white", file="Rsquared")

ggplot(Urchin_Resp_MetadataR2,aes(x=VO2_AFDW_ml.h,y=rsquared))+geom_point()+theme_bw(base_size=15)+
  geom_hline(yintercept=0.98,colour='red',linetype='dashed')+xlab(bquote(' .\nM'*O[2]~(mL~O[2] / h)*''))+ylab(expression(italic("r"^{"2"})))+
  theme(axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)))

dev.off()

ggplot(Urchin_Resp_MetadataR2,aes(x=rsquared))+geom_histogram(stat='count',bins=30)+theme_bw(base_size=15)+
  xlab(bquote(''*MO[2]*' (mlO2/h)'))+geom_vline(xintercept=0.98,colour='red',linetype='dashed')+xlim(c(0.75,1.01))

#### Q10 values
Q10s<-read.csv('Q10_Urchins.csv')
Q10sAcute<-read.csv('Q10_Acute_Urchins.csv')
Q10s$Habitat<-as.factor(Q10s$Habitat)
Q10sAcute$Habitat<-as.factor(Q10sAcute$Habitat)

# Q10s for TPC ramping
Q10plot<-ggplot(Q10s, aes(x=Habitat, y=Q10_4_14,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*Q[10]*' (4-14°C)'))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*Q[10]*' Ramping Protocol'),tag="C")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

Q10plot2<-ggplot(Q10s, aes(x=Habitat, y=Q10_14_26,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*Q[10]*' (14-26°C)'))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*Q[10]*' Ramping Protocol'),tag="D")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

# Q10s for Acute
Q10AcuteC<-ggplot(Q10sAcute, aes(x=Habitat, y=Q10_4_14,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*Q[10]*' (4-14°C)'))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*Q[10]*' Acute Protocol'),tag="A")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

Q10AcuteH<-ggplot(Q10sAcute, aes(x=Habitat, y=Q10_14_26,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*Q[10]*' (14-26°C)'))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*Q[10]*' Acute Protocol'),tag="B")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))


### MAX MR & TAS for TPCs
MaxMR<-ggplot(Q10s, aes(x=Habitat, y=Max_MR,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*(mL ~ O[2] / h)*''))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*MMR[T]*''),tag="E")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

TAS<-ggplot(Q10s, aes(x=Habitat, y=TAS,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*(mL ~ O[2] / h)*''))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*AS[T]*''),tag="F")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

### MAX MR & TAS for ACUTE (TAS is average rate at 26deg - average rate at 4deg for each location/habitat)
MaxMRAcute<-ggplot(Q10sAcute, aes(x=Habitat, y=Max_MR,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*(mL ~ O[2] / h)*''))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*MMR[T]*''),tag="C")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

TASAcute<-ggplot(Q10sAcute, aes(x=Habitat, y=TAS,fill=Habitat)) + 
  geom_boxplot(outlier.shape=NA)+xlab('Habitat')+labs(y=bquote(''*(mL ~ O[2] / h)*''))+theme_bw(base_size=14)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=15),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(strip.text.x = element_text(size = 14, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title=bquote(''*AS[T]*''),tag="D")+
  scale_fill_manual(values=c("goldenrod2", "#669933"))

pdf(width = 7, useDingbats=TRUE,height = 7, bg="white", file="Q10_TPC_Urchins")
pdf(width = 7, useDingbats=TRUE,height = 10, bg="white", file="Q10_ALL_Urchins")


# with acute Q10s (ALL)
ggarrange(Q10AcuteC, Q10AcuteH,Q10plot, Q10plot2,MaxMR, TAS, 
          nrow = 3, ncol = 2,
          align = "v",common.legend = TRUE,legend='bottom')
dev.off()

ggarrange(Q10plot, Q10plot2,Q10AcuteC, Q10AcuteH, 
          nrow = 2, ncol = 2,
          align = "v",common.legend = TRUE,legend='bottom')

# Suppl fig for MMRT and AST per Wet Weight and AFDW
pdf(width = 7, useDingbats=TRUE,height = 7, bg="white", file="Q10_TPC_WW_AFDW")
ggarrange(MaxMR_WW, MaxMR_AFDW,TAS_WW, TAS_AFDW, 
          nrow = 2, ncol = 2,
          align = "v",common.legend = TRUE,legend='bottom')
dev.off()
##### MAP OF SITES
library(ggplot2)
library('sf')
library('rnaturalearth')
library('rnaturalearthdata')
devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")

library('rnaturalearthhires')
library(rgeos)
library(ggspatial)
library(ggrepel)

X=c('canada')
world <- ne_states(country='canada',returnclass = 'sf')
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

NL_map<-ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-61.00, -52.00), ylim = c(46.00, 52.00), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.3,text_cex=1)+
  annotate("text", x = -56, y = 48.5, label = "NL",size=5)+
  annotate('text', x= -59.7, y = 51.5, label= 'Mainland\nCanada', size=3) +
  theme_bw(base_size=15)+theme(axis.text=element_blank(),axis.title=element_blank(),
                   axis.ticks=element_blank())

Lat<-c(47.803947,47.212302,47.722456)
Long<-c(-52.757087,-52.844915,-52.835011)
Site<-c("Biscayan Cove", 'Tors Cove', 'Bauline')
SiteLab<-c("B", 'C', 'A')
Locs<-tibble(Lat,Long,Site,SiteLab)

Avalon<-ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(-54.50, -52.50), ylim = c(46.50, 48.30), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5,text_cex=1) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering)+
  theme_bw(base_size=15)+geom_point(data=Locs,aes(x=Long,y=Lat),
                              colour='darkcyan',size=4,shape=25,fill='darkcyan')+
  geom_text_repel(data=Locs,aes(x=Long,y=Lat,label=Site),size=4)

pdf(width = 7, useDingbats=TRUE,height = 7, bg="white", file="Map")

Avalon + annotation_custom(ggplotGrob(NL_map), xmin = -54.45, xmax = -53.4, 
                       ymin = 47.3, ymax = 48.5)

dev.off()

# END OF PAPER FIGURES #

###################################
# ANOVAs
library(lmer)
# One-Way ANOVA for Biscayan Cove TPC urchins Q10 etc.
A1<-aov(Q10_4_14 ~ Habitat, data = subset(Q10s,Q10_4_14< 2.69))
A1<-aov(Q10_4_14 ~ Habitat, data = Q10sAcute) # unbalanced design !!! and normality violation with 9th sample
summary(A1)

# Checking model assumptions
par(mfrow = c(1,2))  # This code put two plots in the same window
hist(A1$residuals)   # Makes histogram of residuals  
plot(A1, which = 2)   # Makes Q-Q plot, check normality

# Shapiro-Wilko test for normality
aov_residuals <- residuals(object = A1)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) # p < 0.05, normality is violated

# Q10 Cold range violates normality when all samples are included (outlier)
# Grubb's Outlier test:
library(outliers)
test <- grubbs.test(Q10s$Q10_4_14)
test # confirms highest kelp value as an outlier! Ok to remove

# Non-parametric Mann-Whitney U test:
wilcox.test(Q10_4_14 ~ Habitat, data=Q10s)
kruskal.test(Q10_4_14 ~ Habitat, data=Q10s)


plot(A1, which = 1)   # Plot residual vs fitted values; checks homogeneity of variance
# Lavene's test to test for homogeneity of variance
car::leveneTest(TAS ~ Habitat, data = Q10s) # p> 0.05 i.e. homogeneity of variance NOT violated

group_by(Q10s, Habitat) %>%
  dplyr::summarize(
    count = n(),
    mean = mean(TAS, na.rm = TRUE),
    sd = sd(TAS, na.rm = TRUE)
  )

# Tukeys post hoc test:
TukeyHSD(A1)

# Two-Way ANOVA for overall weights at each location
A3<-aov(Dry_weight_g~Habitat*Location,data=subset(Urchin_Resp_Metadata, Habitat!='Blank' & AFDW_g >0))
summary(A3)

# Two-Way ANOVA for weights at each location + habitat
Urchin_Resp_Metadata$Weight_Ratio<-Urchin_Resp_Metadata$AFDW_g/Urchin_Resp_Metadata$Wet_weight_g
A4<-aov(Weight_Ratio~Habitat*Location,data=subset(Urchin_Resp_Metadata, Habitat!='Blank' & Weight_Ratio>0)) # exclude two negative weight-ratio samples (negative bc missing AFDW sample)
summary(A4)

# Which location is different?
TukeyHSD(A4, which = "Location")
TukeyHSD(A4, which = "Habitat")

## GAMs Acute Data
theme_set(theme_bw(base_size=15))
Urchin_Resp_MetadataR2<-Urchin_Resp_MetadataR2 %>% filter(Habitat !='Blank') %>% droplevels()
Urchin_Resp_MetadataR2$Habitat<-as.factor(Urchin_Resp_MetadataR2$Habitat)

# Acute approach data (Location random effect)
UM4gamm<-gamm(scale(VO2_AFDW_ml.h)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)
UM3gamm<-gamm(scale(MR_AFDW_ml.h.g)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)
UM2gamm<-gamm(scale(MR_WetWeight_ml.h.g)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)
UM1gamm<-gamm(scale(MR_MetabTiss_ml.h.g)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)

### FOLLOWING REVIEWER COMMENTS:
# Acute approach data (Location random effect with MASS COVARIATE)
UM4Wgamm<-gamm(scale(VO2_AFDW_ml.h)~s(Temperature_C,by=Habitat,k=4)+Wet_weight_g,random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)
UM3Wgamm<-gamm(scale(MR_AFDW_ml.h.g)~s(Temperature_C,by=Habitat,k=4)+Wet_weight_g,random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)
UM2Wgamm<-gamm(scale(MR_WetWeight_ml.h.g)~s(Temperature_C,by=Habitat,k=4)+Wet_weight_g,random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)
UM1Wgamm<-gamm(scale(MR_MetabTiss_ml.h.g)~s(Temperature_C,by=Habitat,k=4)+Wet_weight_g,random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)

UM4WgammA<-gamm(scale(VO2_AFDW_ml.h)~s(Temperature_C,by=Habitat,k=4)+AFDW_g,random=list(Location=~1),data=subset(Urchin_Resp_MetadataR2,rsquared>0.98),na.action=na.omit)

# POLYNOMIAL LME MODEL for comparison
UM4lme<-lme(scale(VO2_AFDW_ml.h)~Habitat*poly(Temperature_C,3)+Wet_weight_g,random=~1|Location,data=Urchin_Resp_Metadata,na.action=na.omit)
plot(residuals(UM4lme))

# pattern and significance agree with gamm results
ggplot(Urchin_Resp_Metadata, aes(x=Temperature_C, y=VO2_AFDW_ml.h,colour=Habitat)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE))+ylab('O2 ml/h')+xlab('Temperature (°C)')

Urchin_Resp_Metadata<-Urchin_Resp_Metadata %>% filter(Habitat !='Blank') %>% droplevels() 
col_test2<-c('goldenrod2', '#669933')[as.numeric(Urchin_Resp_Metadata$Habitat)]
col_transp2<-adjustcolor(col_test2, alpha.f = 0.3)

plot(UM3gamm$gam,select=1,shade=TRUE,shade.col=rgb(10,0,255,40,maxColorValue=255),col="#330066",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="",ylab=('ml/h/g'),cex.main=1.6,rug=FALSE,ylim=c(-2,2))
axis(side=2,at=c(-1,0,1),cex.axis=1.5)
par(new=TRUE)
plot(UM3gamm$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,40,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-2,2))
text(5,1.5,"a",cex=1.8)
legend("bottomright", c("Barren","Kelp"), lwd=3,col=c("#330066", "#669933"), bty="n", cex=1.5)
mtext('AFDW MR', side=3, line=0.5, font=2,cex=1.5)

plot(UM2gamm$gam,select=1,shade=TRUE,shade.col=rgb(10,0,255,40,maxColorValue=255),col="#330066",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="",ylab=('ml/h/g'),cex.main=1.6,rug=FALSE,ylim=c(-2,2))
axis(side=2,at=c(-1,0,1),cex.axis=1.5)
par(new=TRUE)
plot(UM2gamm$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,40,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-2,2))
text(5,1.5,"a",cex=1.8)
mtext('Wet Weight MR', side=3, line=0.5, font=2,cex=1.5)

plot(UM1gamm$gam,select=1,shade=TRUE,shade.col=rgb(10,0,255,40,maxColorValue=255),col="#330066",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="",ylab=('ml/h/g'),cex.main=1.6,rug=FALSE,ylim=c(-2,2))
axis(side=2,at=c(-1,0,1),cex.axis=1.5)
par(new=TRUE)
plot(UM1gamm$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,40,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-2,2))
text(5,1.5,"a",cex=1.8)
mtext('Metabolizing Tissue MR', side=3, line=0.5, font=2,cex=1.5)

## GAMs TPC data
Urchin_Resp_TPCs<-Urchin_Resp_TPCs %>% filter(Habitat !='Blank') %>% droplevels()
Urchin_Resp_TPCs$Habitat<-as.factor(Urchin_Resp_TPCs$Habitat)
TPC_sub<-Urchin_Resp_TPCs %>% filter(Experiment=='4.3' & Habitat!="") %>% droplevels()

UM5gamm<-gamm(scale(VO2_AFDW_ml.h)~s(Temp,by=Habitat,k=4),random=list(Replicate_Chamber=~1),data=TPC_sub,na.action=na.omit)
UM5lme<-lme(scale(VO2_AFDW_ml.h)~Habitat*poly(Temp,3),random=~1|Replicate_Chamber,data=TPC_sub,na.action=na.omit)

# pattern and significance confirm gamm results
ggplot(TPC_sub, aes(x=Temp, y=VO2_AFDW_ml.h,colour=Habitat)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE))+ylab('O2 ml/h')+xlab('Temperature (°C)')

# TPC data (with MASS COVARIATE)
UM5Wgamm<-gamm(scale(VO2_AFDW_ml.h)~s(Temp,by=Habitat,k=4)+Wet_weight_g,random=list(Replicate_Chamber=~1),data=TPC_sub,na.action=na.omit)

### SUPP FIG S7 MO2 gamms Acute & TPC (with mass-covariate (A,B); mass-independent (C,D))
pdf(width = 10, useDingbats=TRUE,height = 9, bg="white", file="VO2_gamms_WeightCovar_MassIndep")

par(mfrow=c(2,2))
par(mar=c(4,5.5,1,0),oma=c(1,0.5,1,1))

# Acute Absolute MO2 with mass-covariate
plot(scale(VO2_AFDW_ml.h)~Temperature_C,data=Urchin_Resp_Metadata,yaxt='n',xaxt='n',ylab='',xlab='',col=col_transp2,pch=16,ylim=c(-3,4))
par(new=TRUE)
plot(UM4WgammA$gam,select=1,shade=TRUE,shade.col=rgb(255,180,102,60,maxColorValue=255),col="goldenrod2",lty=1,lwd=4,cex=1.8,yaxt='n',xaxt='n',cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]~(mL ~ O[2] / h)*''),cex.main=1.6,rug=FALSE,ylim=c(-3,4))
axis(side=2,at=c(-2,0,2,4),cex.axis=1.5)
axis(side=1,at=c(5,15,25),cex.axis=1.5)
par(new=TRUE)
plot(UM4WgammA$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,60,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-3,4))
text(5,3.3,"A",cex=1.8)
legend("bottomright", c("Barrens","Kelp"), lwd=3,col=c("goldenrod2", "#669933"), bty="n", cex=1.2)
mtext('Acute Protocol', side=3, line=0.5, font=1,cex=1.8,adj=0)

# Ramped (TPC) Absolute MO2 with mass-covariate
plot(scale(VO2_AFDW_ml.h)~Temp,data=subset(Urchin_Resp_TPCs,Experiment=='4.3'),yaxt='n',xaxt='n',ylab='',xlab='',col=col_transp2,pch=16,ylim=c(-2,2))
par(new=TRUE)
plot(UM5Wgamm$gam,select=1,shade=TRUE,shade.col=rgb(255,180,102,60,maxColorValue=255),col="goldenrod2",lty=1,lwd=4,cex=1.8,yaxt='n',xaxt='n', cex.axis=1.5, cex.lab=1.6,xlim=c(4,32),ylab='',xlab=('Temperature (°C)'),cex.main=1.6,rug=FALSE,ylim=c(-2,2))
axis(side=2,at=c(-2,-1,-0,1,2),cex.axis=1.5)
axis(side=1,at=c(5,15,25),cex.axis=1.5)
par(new=TRUE)
plot(UM5Wgamm$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,60,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-2,2))
text(5,1.5,"B",cex=1.8)
legend("bottomright", c("Barrens","Kelp"), lwd=3,col=c("goldenrod2", "#669933"), bty="n", cex=1.2)
mtext('Ramping Protocol', side=3, line=0.5, font=1,adj=0,cex=1.8)

# Acute MASS-INDEPENDENT MO2
plot(scale(VO2_AFDW_ml.h)~Temperature_C,data=Urchin_Resp_Metadata,yaxt='n',xaxt='n',ylab='',xlab='',col=col_transp2,pch=16,ylim=c(-3,4))
par(new=TRUE)
plot(UM4Rgam$gam,select=1,shade=TRUE,shade.col=rgb(255,180,102,60,maxColorValue=255),col="goldenrod2",lty=1,lwd=4,cex=1.8,yaxt='n',xaxt='n',cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]*~(standardized)*''),cex.main=1.6,rug=FALSE,ylim=c(-3,4))
axis(side=2,at=c(-2,0,2,4),cex.axis=1.5)
axis(side=1,at=c(5,15,25),cex.axis=1.5)
par(new=TRUE)
plot(UM4Rgam$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,60,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-3,4))
text(5,3.3,"C",cex=1.8)
legend("bottomright", c("Barrens","Kelp"), lwd=3,col=c("goldenrod2", "#669933"), bty="n", cex=1.2)
#mtext('Acute Protocol', side=3, line=0.5, font=1,cex=1.8,adj=0)

# Ramped (TPC) MASS-INDEPENDENT MO2
plot(scale(VO2_AFDW_ml.h)~Temp,data=subset(Urchin_Resp_TPCs,Experiment=='4.3'),yaxt='n',xaxt='n',ylab='',xlab='',col=col_transp2,pch=16,ylim=c(-2,2))
par(new=TRUE)
plot(UM5Rgamm$gam,select=1,shade=TRUE,shade.col=rgb(255,180,102,60,maxColorValue=255),col="goldenrod2",lty=1,lwd=4,cex=1.8,yaxt='n',xaxt='n', cex.axis=1.5, cex.lab=1.6,xlim=c(4,32),ylab='',xlab=('Temperature (°C)'),cex.main=1.6,rug=FALSE,ylim=c(-2,2))
axis(side=2,at=c(-2,-1,-0,1,2),cex.axis=1.5)
axis(side=1,at=c(5,15,25),cex.axis=1.5)
par(new=TRUE)
plot(UM5Rgamm$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,60,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-2,2))
text(5,1.5,"D",cex=1.8)
legend("bottomright", c("Barrens","Kelp"), lwd=3,col=c("goldenrod2", "#669933"), bty="n", cex=1.2)
#mtext('Ramping Protocol', side=3, line=0.5, font=1,adj=0,cex=1.8)

dev.off()

######
# Residual model using MR~weight residuals ACUTE
Acute_sub<-Urchin_Resp_MetadataR2 %>% filter(rsquared>0.98 & Replicate_Chamber!="") %>% droplevels()
Mres1<-lme(VO2_AFDW_ml.h~Wet_weight_g,random=~1|Location,data=Acute_sub,na.action=na.omit)
Mres2<-gamm(VO2_AFDW_ml.h~s(Wet_weight_g),random=list(Location=~1),data=Acute_sub,na.action=na.omit)
res.VO2.WW=as.data.frame(residuals(Mres1));colnames(res.VO2.WW)=c("res.VO2.WW")
res2.VO2.WW<-as.data.frame(residuals(Mres2$gam,type='response'));colnames(res2.VO2.WW)=c('res2.VO2.WW') # response residuals (= observed - fitted values)
Acute_sub=cbind(Acute_sub,res.VO2.WW,res2.VO2.WW)
extra_temp<-data.frame(c(6,8,12,16,20,24,28));colnames(extra_temp)=c("Temperature_C") #adding temp steps that weren't measured to df
Acute_sub=dplyr::bind_rows(Acute_sub,extra_temp)

# run model with residuals (weight effect removed) 
UM4Rlme<-gamm(scale(res.VO2.WW)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=Acute_sub,na.action=na.omit) # using residuals from lme
UM4Rgam<-gamm(scale(res2.VO2.WW)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=Acute_sub,na.action=na.omit) # using residuals from gamm

# looks like weight-effect is negligible 

# Residual model using MR~weight residuals TPC
Mres3<-lme(VO2_AFDW_ml.h~Wet_weight_g,random=~1|Replicate_Chamber,data=TPC_sub,na.action=na.omit)
Mres4<-gamm(VO2_AFDW_ml.h~s(Wet_weight_g),random=list(Replicate_Chamber=~1),data=TPC_sub,na.action=na.omit)
res.VO2.WW=as.data.frame(residuals(Mres3));colnames(res.VO2.WW)=c("res.VO2.WW")
res2.VO2.WW<-as.data.frame(residuals(Mres4$gam,type='response'));colnames(res2.VO2.WW)=c('res2.VO2.WW') # response residuals (= observed - fitted values)
TPC_sub=cbind(TPC_sub,res.VO2.WW)

# run model with residuals (weight effect removed)
UM5Rgamm<-gamm(scale(res.VO2.WW)~s(Temp,by=Habitat,k=4),random=list(Replicate_Chamber=~1),data=TPC_sub,na.action=na.omit)
# looks like weight-effect is negligible 

### Residual model as a power function:
library(drc)
library(devtools)
install_github("onofriandreapg/aomisc")
library(aomisc)
# For power function of the form: MR = a × M^b (MR=metabolic rate, a=intercept, M=mass and b=scaling exponent)
D(expression(a * X^b), "X")

powermodel <- drm(VO2_AFDW_ml.h~Wet_weight_g, fct = DRC.powerCurve(),
             data = Acute_sub)

powermodel2 <- nls(VO2_AFDW_ml.h ~ NLS.powerCurve(Wet_weight_g, a, b),
             data = Acute_sub)

powermodel3 <-nls(VO2_AFDW_ml.h~b*Wet_weight_g^z,start = list(b = 0.1, z = 1),data=Acute_sub)
powermodel4 <-nls(VO2_AFDW_ml.h~b*Wet_weight_g^z,start = list(b = 0.1, z = 0.75),data=Acute_sub)
# these models all give the ~same coefficients (M2, M3 and M4 are exactly the same)
summary(powermodel2)
plot(powermodel2, log="", main = "Power curve (b = )")
# comparing residual model based on power-function for MR~Mass
res.VO2.WWpow=as.data.frame(residuals(powermodel4));colnames(res.VO2.WWpow)=c("res.VO2.WWpow")
test=cbind(Acute_sub,res.VO2.WWpow)
PowerResgam<-gamm(scale(res.VO2.WWpow)~s(Temperature_C,by=Habitat,k=4),random=list(Location=~1),data=test,na.action=na.omit) # using residuals from gamm

# Plot
plot(scale(VO2_AFDW_ml.h)~Temperature_C,data=Urchin_Resp_Metadata,yaxt='n',xaxt='n',ylab='',xlab='',col=col_transp2,pch=16,ylim=c(-3,4))
par(new=TRUE)
plot(PowerResgam$gam,select=1,shade=TRUE,shade.col=rgb(255,180,102,60,maxColorValue=255),col="goldenrod2",lty=1,lwd=4,cex=1.8,yaxt='n',xaxt='n',cex.axis=1.5, cex.lab=1.6, xlab="Temperature (°C)",ylab=expression(' .\nM'*O[2]*~(standardized)*''),cex.main=1.6,rug=FALSE,ylim=c(-3,4))
axis(side=2,at=c(-2,0,2,4),cex.axis=1.5)
axis(side=1,at=c(5,15,25),cex.axis=1.5)
par(new=TRUE)
plot(PowerResgam$gam,shade=TRUE,select=2,shade.col=rgb(100,255,0,60,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.8,rug=FALSE,ylim=c(-3,4))
text(5,3.3,"C",cex=1.8)
legend("bottomright", c("Barrens","Kelp"), lwd=3,col=c("goldenrod2", "#669933"), bty="n", cex=1.2)
#mtext('Acute Protocol', side=3, line=0.5, font=1,cex=1.8,adj=0)


###################################
#### Strongylocentrotus droebachiensis abundance-distribution (RLS data)
#setwd("/Users/jmschuster/Desktop/PhD/Thesis writing/Chapter 2")
Strongy_AB<-read.csv('Reef_Life_Survey_(RLS)_StrongylocentrotusDroe_surveys.csv')

Strongy_AB<-aggregate(Total~SurveyID+SiteCode+Ecoregion+Realm+Country+SiteLat+SiteLong+SurveyDate+Depth+geom,data=Strongy_AB,FUN=sum)

# export to manually add 2020 RLS surveys from NL
write.xlsx(Strongy_AB,file="Strongy_AB_RLS.xlsx",col.names=TRUE)
Strongy_AB<-read.csv("Strongy_AB_RLSplus.csv")

ggplot(Strongy_AB,aes(x=SiteLat,y=Total))+geom_point(colour='#330066',size=2,alpha=0.5)+
  theme_bw(base_size=15)+xlab("Latitude °")+
  ylab('Abundance (# per survey)')+xlim(0,90)

ggplot(Strongy_AB,aes(x=Survey.T.Match,y=Total))+geom_point(colour='#330066',size=2,alpha=0.5)+
  theme_bw(base_size=15)+xlab("Temperature °C")+
  ylab('Abundance (# per survey)')+xlim(-1,32)

################ SEM FOR URCHINS
Urchins<-na.omit(Urchin_Resp_TPCs)

Urchin_Resp_TPCs <- Urchin_Resp_TPCs %>% filter (Habitat != 'Blank') %>% droplevels

# No direct habitat effect
rate.mass <- lm(MR_AFDW_ml.h.g ~ AFDW_g, data=Urchin_Resp_TPCs, na.action=na.omit)
hist(residuals(rate.mass))

mass.hab <- lm(AFDW_g ~ Habitat, data = subset(Urchin_Resp_TPCs, Habitat != "Blank"), na.action=na.omit)
urchin_sem<-psem(rate.mass, mass.hab)
summary(urchin_sem)

# Direct habitat effect
rate.full <- lm(MR_AFDW_ml.h.g ~ AFDW_g+Habitat, data=subset(Urchin_Resp_TPCs, Temp=='32'), na.action=na.omit)
full.sem<-psem(rate.full)
summary(full.sem)

## NOT ENOUGH DATA TO DO THIS.... 

##################################
######### UNUSED FIGURES

#### WEIGHTS ACROSS TEMPS
# Wet Weights
pdf(width = 9, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Wet_Weights_Exp1-3")

ggplot(Urchin_Resp_Metadata,aes(x=as.factor(Temperature_C), y=Wet_weight_g,fill=Habitat))+geom_boxplot()+xlab('Temperature (°C)')+ylab('Wet Weight (g)')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("#330066", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12))+facet_wrap(~Location,scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))

dev.off()

# Dry Weights
pdf(width = 9, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Dry_Weights_Exp1-3")

ggplot(Urchin_Resp_Metadata,aes(x=as.factor(Temperature_C), y=Dry_Weight_g,fill=Habitat))+geom_boxplot()+xlab('Temperature (°C)')+ylab('Dry Weight (g)')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("#330066", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12))+facet_wrap(~Location,scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))

dev.off()

# Ashed Weights
pdf(width = 9, useDingbats=TRUE,height = 4, bg="white", file="Urchin_Ashed_Weights_Exp1-3")

ggplot(subset(Urchin_Resp_Metadata,AFDW_g>0),aes(x=as.factor(Temperature_C), y=AFDW_g,fill=Habitat))+geom_boxplot()+xlab('Temperature (°C)')+ylab('Ash-free Dry Weight (g)')+theme_bw(base_size=15)+
  scale_fill_manual(values=c("#330066", "#669933"))+theme(legend.position='none',legend.title=element_text(size=14,face='bold'),legend.text=element_text(size=12))+facet_wrap(~Location,scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))

dev.off()

# PLAYING
ggplot(subset(Urchin_Resp_Metadata,AFDW_g>0), aes(x=Temperature_C, y=MR_AFDW_ml.h.g, colour=Habitat)) + 
  geom_line()+xlab('Temperature (°C)')+ylab('Metabolic Rate (mlO2/h/g)')+theme_bw(base_size=15)+
  scale_colour_manual(values=c("#330066", "#669933"))+theme(legend.position=c(0.1,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+facet_wrap(~Location,scale='free')+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title="AFDW Weight")

Urchin_Resp_Metadata$temp<-as.factor(Urchin_Resp_Metadata$Temperature_C)

resp_m1<-gamm(MR_AFDW_ml.h.g~s(Temperature_C,by=Habitat,k=6)+Habitat,random=list(Location=~1),data=Urchin_Resp_Metadata,na.action=na.omit)
plot(resp_m1)

plot(resp_m1$gam,select=1,shade=TRUE,shade.col=rgb(100,255,0,40,maxColorValue=255),col="#330066",lty=1,lwd=4,cex=1.8,yaxt='n', cex.axis=1.5, cex.lab=1.6, xlab="",ylab=expression(paste(~Delta,"Metabolic Rate")),cex.main=1.6,rug=FALSE,ylim=c(-1,1))
axis(side=2,at=c(-05,-0.5,0.5),cex.axis=1.5)
par(new=TRUE)
plot(resp_m1$gam,shade=TRUE,select=2,shade.col=rgb(10,0,255,40,maxColorValue=255),col="#669933",lty=1,lwd=4,cex=1.8, cex.axis=1.5, cex.lab=1.6, xlab="",ylab="",yaxt="n", xaxt="n",cex.main=1.6,rug=FALSE,ylim=c(-1,1))

#### TPC plots
WW_TPC<-ggplot(Urchin_Resp_TPCs, aes(x=as.factor(Temp), y=MR_WetWeight_ml.h.g, fill=Habitat)) + 
  geom_boxplot()+xlab('Temperature (°C)')+ylab('Metabolic Rate (mlO2/h/g)')+theme_bw(base_size=15)+facet_wrap(~Experiment,labeller = labeller(Experiment = Exp.labs))+
  scale_fill_manual(values=c("#330066", "#669933"))+theme(legend.position=c(0.08,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title="Wet Weight")

dev.off()

AFDW_TPC<-ggplot(Urchin_Resp_TPCs, aes(x=as.factor(Temp), y=MR_AFDW_ml.h.g, fill=Habitat)) + 
  geom_boxplot()+xlab('Temperature (°C)')+ylab('Metabolic Rate (mlO2/h/g)')+theme_bw(base_size=15)+facet_wrap(~Experiment,labeller = labeller(Experiment = Exp.labs))+
  scale_fill_manual(values=c("#330066", "#669933"))+theme(legend.position=c(0.2,0.8),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title="AFDW")

ggarrange(AFDW_TPC, WW_TPC, 
          nrow = 1, ncol = 2,
          align = "h")

#### Individual TPCs
WW_TPCi<-ggplot(TPC_BIS, aes(x=Temp, y=MR_WetWeight_ml.h.g)) + 
  geom_line(aes(colour=Replicate_Chamber,linetype=Habitat),size=1)+geom_point(aes(shape=Habitat,fill=Habitat),size=3)+xlab('Temperature (°C)')+ylab('Metabolic Rate (mlO2/h/g)')+theme_bw(base_size=15)+theme(legend.position='right',legend.title=element_text(size=16,face='bold'),legend.text=element_text(size=14))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title="Wet Weight")+scale_color_viridis_d()+labs(fill="Habitat", colour="Urchin")+scale_fill_manual(values=c('white','black','grey'))+
  scale_x_continuous( limits=c(3, 33),breaks=c(4, 6.5, 10, 14, 18, 22, 26, 30, 32),labels=c(4, 6.5, 10, 14, 18, 22, 26, 30, 32))


WW_TPCi<-WW_TPCi+theme(axis.title.y=element_blank())

ggarrange(AFDW_TPCi, WW_TPCi, 
          nrow = 1, ncol = 2,
          align = "v",common.legend = TRUE,legend='bottom')

### Comparing Acute vs Ramp
Rate_Comp<-ggplot(subset(TPC_vs_Acute,Temp<30), aes(x=as.factor(Temp), y=MR_AFDW,fill=Treatment)) + 
  geom_boxplot()+xlab('Temperature (°C)')+ylab('Metabolic Rate (mlO2/h/g)')+theme_bw(base_size=15)+theme(legend.position=c(0.1,0.7),legend.title=element_text(size=18,face='bold'),legend.text=element_text(size=15))+
  theme(strip.text.x = element_text(size = 16, color = "white", face = "bold"),strip.background = element_rect(fill="black"))+labs(title="AFDW")+facet_wrap(~Habitat)+
  scale_x_discrete(breaks=c(4, 6.5, 10, 14, 18, 22, 26, 30, 32),labels=c(4, 6.5, 10, 14, 18, 22, 26, 30, 32))


### Percentage difference between kelp & barren
# Acute
Perc_diff_Acute<-Acute_sub %>% group_by(Temperature_C,Habitat,Location) %>% 
  dplyr::summarize(mean=mean(VO2_AFDW_ml.h),sd=sd(VO2_AFDW_ml.h)) %>%
  ungroup() %>% tidyr::drop_na()

Perc_diff_Acute<-Perc_diff_Acute %>%
  group_by(Temperature_C,Location) %>%
  mutate(Diff = mean - lag(mean, default = mean[1]))

Perc_diff_Acute[Perc_diff_Acute == 0] <- NA 

Perc_diff_Acute<-Perc_diff_Acute %>%
  group_by(Temperature_C,Location) %>%
  mutate(Mean_tot = mean(mean, default = mean[1]))

Perc_diff_Acute$Diff_by_Avr<-Perc_diff_Acute$Diff/Perc_diff_Acute$Mean_tot
Perc_diff_Acute$Perc<-Perc_diff_Acute$Diff_by_Avr*100

# TPC
Perc_diff_TPC<-TPC_sub %>% group_by(Temp,Habitat,Location) %>% 
  dplyr::summarize(mean=mean(VO2_AFDW_ml.h),sd=sd(VO2_AFDW_ml.h)) %>%
  ungroup() %>% tidyr::drop_na()

Perc_diff_TPC<-Perc_diff_TPC %>%
  group_by(Temp,Location) %>%
  mutate(Diff = mean - lag(mean, default = mean[1]))

Perc_diff_TPC[Perc_diff_TPC == 0] <- NA 

Perc_diff_TPC<-Perc_diff_TPC %>%
  group_by(Temp,Location) %>%
  mutate(Mean_tot = mean(mean, default = mean[1]))

Perc_diff_TPC$Diff_by_Avr<-Perc_diff_TPC$Diff/Perc_diff_TPC$Mean_tot
Perc_diff_TPC$Perc<-Perc_diff_TPC$Diff_by_Avr*100
