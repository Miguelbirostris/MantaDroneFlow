
# Disclaimer --------------------------------------------------------------

# Scripts and code for an on-going publication. Drone footage unveils the effects of diving with manta rays during feeding events. Gómez-García,Miguel de Jesús 1; O’Brien, Amanda 2 ; Pate, Jessica*2. 1 Quantitative Marine Ecology Lab, University of New Hampshire, Durham, New Hampshire, USA. *Correspondence author jessica.pate@marinemegafauna.org. 2 Marine Megafauna Foundation, Truckee, CA, 96161, USA
#
# 
#
# Please note errors and inconsistencies can exist. Scripts are subject to change.
#



# Libraries and data ------------------------------------------------------


library(tidyverse)
library(parallel)
library(ggplot2)
library(markovchain)
library(SMM)
library(rstan)
library(diagram)
library(bayestestR)
library(viridis)
library(scales) # for labels


dat<-read.csv("data.csv",header = TRUE,row.names=1) ###Data must be distributed in equal intervals
head(dat)



# Data analysis -----------------------------------------------------------


datsum<-dat %>% group_by(MantaID)%>%                 #Group by Ethogram
  summarise(Length=length(Time_seconds),       #Ethogram length
            Divers=max(Divers),                #Divers
            Behaviors=length(unique(Behavior)) #Unique behaviors
  )

#Summary by Ethogram
print(datsum)

#unique individuals
length(unique(na.omit(dat$Individual)))

#Na mantas
unique(dat$MantaID[is.na(dat$Individual)])

#Summary by Individuals


datsum_ind<-dat %>% group_by(Individual,Divers)%>%                 #Group by Ethogram
  summarise(Ethograms=sum(length(unique(MantaID))),
            Length=length(Time_seconds),       #Ethogram length
            Behaviors=length(unique(Behavior)) #Unique behaviors
  )

#Summary by Individual
print(datsum_ind)

mean(na.omit(datsum_ind$Ethograms))
sd(na.omit(datsum_ind$Ethograms))

##Subsample Observer bias -------------------------------------------------

#Import analyzed subsample
subs_analyzed<- read.csv("subsample.csv")

#check IDs
unique(subs_analyzed$MantaID)

#Extract subsample from original dataset
subs_original<-dat[dat$MantaID %in% subs_analyzed$MantaID, ]


#check lengths
subs_original%>%group_by(MantaID)%>%
  summarise(length=n())

subs_analyzed%>%group_by(MantaID)%>%
  summarise(length=n())

#check spelling

unique(subs_original$Behavior)
unique(subs_analyzed$Behavior)

unique(subs_original$Behavior)== unique(subs_analyzed$Behavior)


#Percent of matched observations
length(subs_original$Behavior [subs_original$Behavior== subs_analyzed$Behavior]) / length(subs_original$Behavior)


#By State

length(subs_analyzed$States[subs_original$States == subs_analyzed$States]) / length(subs_original$Behavior)



# Percentage time at state/behavior ------------------------------------------------



##Create proportions by adding 0s ----------------------------------------

#Add second length by ethogram per behaviors and per states

dat_states<-dat %>% group_by(MantaID)%>%           #Group by states
  mutate(Length_seconds = max(Time_seconds))       #Add clip length


dat_behavior<-dat %>% group_by(MantaID)%>%           #Group by states
  mutate(Length_seconds = max(Time_seconds))       #Add clip length


#Adding 0s to states

dat_states<-dat_states%>% 
  group_by(MantaID, States)%>%                     #Group by Ethogram-state
  summarise(Individual=first(Individual),          #Keep Indiciduals
            Length_state=length(Time_seconds),     #Ethogram-behavior length
            Divers=max(Divers),                    #Unique behaviors
            Length_seconds=max(Length_seconds),    #keep total clip length
            Per_state=Length_state/Length_seconds  #Percentage duration state
  )

#add 0 values to unnobserved states
dat_states<-dat_states%>%ungroup() %>%                    #ungroup
  complete(                                               #complete function
    MantaID, States = c("Avoidance", "Feeding", "Neutral"), #by ethogram and state
    fill = list(Per_state = 0))%>%
  group_by(MantaID)%>%                                    #add ethogram metadata to filled lines        
  mutate(                                                    
    Length_seconds = first(Length_seconds,na_rm = T),     # Fill missing Length_seconds
    Individual = first(Individual,na_rm=T),                       # Fill Individual
    Divers = first(Divers,na_rm=T))    

#Adding 0s to behaviors

dat_behavior<-dat_behavior%>% 
  group_by(MantaID, Behavior)%>%                     #Group by Ethogram-behavior
  summarise(Individual=first(Individual),          #Keep Indiciduals
            Length_behavior=length(Time_seconds),     #Ethogram-behavior length
            Divers=max(Divers),                    #Unique behaviors
            Length_seconds=max(Length_seconds),    #keep total clip length
            Per_behavior=Length_behavior/Length_seconds  #Percentage duration behavior
  )



#add 0 values to unnobserved behaviors
dat_behavior<-dat_behavior%>%ungroup() %>%                    #ungroup
  complete(                                               #complete function
    MantaID, Behavior , #by ethogram and state
    fill = list(Per_behavior = 0))%>%
  group_by(MantaID)%>%                                    #add ethogram metadata to filled lines        
  mutate(                                                    
    Length_seconds = first(Length_seconds,na_rm = T),     # Fill missing Length_seconds
    Individual = first(Individual,na_rm=T),                       # Fill Individual
    Divers = first(Divers,na_rm=T))                       # Fill Divers



## Percentage at each state ------------------------------------------------



average_state <- dat_states #Create new dataframe

average_state$Divers[average_state$Divers>1] <-1 #some ethograms had 2 divers, standarize to presence/absence instead of cathegorical

average_state <- average_state %>%
  group_by(States,Divers) %>%
  summarise(Average_Percentage = mean(Per_state, na.rm = TRUE),
            SD_Percentage = sd(Per_state, na.rm = TRUE))


# Create the pie chart
Pie_State<-ggplot(average_state, aes(x = "", y = Average_Percentage, fill = States)) +
  geom_bar(stat = "identity", width = 1, color = "white") +  # Add white borders for separation
  coord_polar("y", start = 0) +  # Convert to pie chart
  scale_fill_viridis_d(option = "mako") +   # Custom colors
  labs(
    title = "Average State Distribution",
    fill = "States"
  ) +
  theme_void() +  # Clean layout by removing background elements
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and style the title
    legend.position = "right",  # Position the legend
    legend.title = element_text(face = "bold")  # Bold legend title
  ) +
  facet_wrap(~ Divers, scales = "free_y", ncol = 2,labeller = as_labeller(c(`0` = "Absence", `1` = "Presence")))  # Facet by Divers

print(Pie_State)



##Significant difference tests -------------------------------------------

##**For States**

dat_states_aov <- dat_states %>%           #Take base DF   
  mutate(Divers = if_else(Divers > 1, 1, Divers)) %>% #Standarize diver presence/absence
  filter(Divers == 0, States != "Avoidance")          #Keep only absence, remove avoidance 0s

#Number of ethograms with IDd mantas
length(unique(dat_states$MantaID[!is.na(dat_states$Individual)]))

#Anova
aov_states_Nodiver <- aov(Per_state ~ States, data = dat_states_aov)
summary(aov_states_Nodiver )

#Tukey
TukeyHSD(aov_states_Nodiver,"States")



# Significance with diver

dat_states_aov <- dat_states %>%           #Take base DF   
  mutate(Divers = if_else(Divers > 1, 1, Divers)) %>% #Standarize diver presence/absence
  filter(Divers == 1)          #Keep only presence


#Anova
aov_states_Diver <- aov(Per_state ~ States, data = dat_states_aov)
summary(aov_states_Diver )

#Tukey
TukeyHSD(aov_states_Diver,"States")


##**For Behaviors**

dat_behavior_aov <- dat_behavior %>%           #Take base DF   
  mutate(Divers = if_else(Divers > 1, 1, Divers)) %>% #Standarize diver presence/absence
  filter(Divers == 0)          #Keep only absence, remove avoidance 0s


#Anova
aov_behavior_Nodiver <- aov(Per_behavior ~ Behavior, data = dat_behavior_aov)
summary(aov_behavior_Nodiver )

#Tukey
TukeyHSD(aov_behavior_Nodiver,"Behavior")



# Significance with diver

dat_behavior_aov <- dat_behavior %>%           #Take base DF   
  mutate(Divers = if_else(Divers > 1, 1, Divers)) %>% #Standarize diver presence/absence
  filter(Divers == 1)          #Keep only presence


#Anova
aov_behavior_Diver <- aov(Per_behavior ~ Behavior, data = dat_behavior_aov)
summary(aov_behavior_Diver )

#Tukey
TukeyHSD(aov_behavior_Diver,"Behavior")




# Individual manta analysis -----------------------------------------------


#Create dataframe for individual analysis
dat_divers_box<-dat_states[dat_states$Divers>0,]  #With divers only
#dat_divers_box<-dat_states[dat_states$Divers<1,] #this gives you the results without divers


#Remove unidd clips
dat_divers_box<-dat_divers_box[!is.na(dat_divers_box$Individual),]

#Create individual table 

IndivEthoTable_d<-dat[dat$Divers>0,]%>%
  group_by(Individual)%>%                          #Group by individual
  summarise(Ethograms=length(unique(MantaID)))%>%  #Get unique ethograms
  print(n = 30)                                    #visualize whole table



dat_divers_box<-dat_divers_box %>%  #Append number of ethograms by individual
  left_join(IndivEthoTable_d, by = "Individual")

#Keep only mantas with more than 2 clips
dat_divers_box<-dat_divers_box[dat_divers_box$Ethograms>2,]

#Add meand and sd values per state
dat_divers_box<-dat_divers_box%>%
  group_by(States) %>%
  mutate(
    MeanState = mean(Per_state, na.rm = TRUE),
    SDState   = sd(Per_state, na.rm = TRUE)
  )

# Prepare one row of means per States (for the lines)
state_summary <- dat_divers_box %>%
  distinct(States, MeanState, SDState)

dat_divers_box$MeanState<- mean(dat_divers_box$Per_state[dat_divers_box$States=="Avoidance"])

#Transform Individuals into factors

dat_divers_box$Individual<-as.factor(dat_divers_box$Individual)

#test signiffice Avoidance
summary(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Avoidance",]))

TukeyHSD(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Avoidance",]))

Tukey_ind_Diver_avoidance<-as.data.frame(TukeyHSD(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Avoidance",]))$Individual,header=T)

#specific mantas with different proportion at avoidance
Tukey_ind_Diver_avoidance[Tukey_ind_Diver_avoidance$`p adj`<0.05,]


#Create separate dataframe for avoidant mantas
Ind_avoidance_df <- dat_divers_box %>%
  filter(States == "Avoidance") %>%                      #select avoidance only
  mutate(
    Nature = case_when(
      Individual %in% c(168, 185) ~ "Higher avoidance", #higher
      Individual %in% c(174, 186, 188, 198) ~ "Lower avoidance",  #lower
      TRUE ~ "Average avoidance"                         #average
    )
  )



Ind_avoidance_df%>%group_by(Individual)%>%
  summarise(MeanAvoidance=mean(Per_state))

#Tag individual as more avoidant or less avoidant



Indv_Divers_avoidance<-ggplot(Ind_avoidance_df, aes(x = Individual, y = Per_state, fill = Nature)) +
  geom_boxplot() +
  labs(title = "Percentage of Time Spent at Each State per Individual Manta",
       x = "Manta ID",
       y = "Proportion of Time in Avoidance"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "mako", begin=0 ) +
  theme(text=element_text(size=20),
        axis.text.x = element_text(angle = 0, hjust = 1,size=15),
        legend.position = "bottom"# Position the legend)
  )+
  geom_hline( aes(yintercept = MeanState), linetype = "dashed", color = "black", linewidth = 0.9) #Mean dashed line

print(Indv_Divers_avoidance)

#Save plot

ggsave("Individual_Manta_Avoidance_Divers_Boxplot_v2.jpg", plot = Indv_Divers_avoidance, width = 12, height = 6, dpi = 300)



#Other states

#test signiffice Feeding
summary(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Feeding",]))
TukeyHSD(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Feeding",]))
Tukey_ind_Diver_Feeding<-as.data.frame(TukeyHSD(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Feeding",]))$Individual,header=T)

Tukey_ind_Diver_Feeding[Tukey_ind_Diver_Feeding$`p adj`<0.05,]


#test signiffice Neutral
summary(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Neutral",]))
TukeyHSD(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Neutral",]))

Tukey_ind_Diver_Neutral<-as.data.frame(TukeyHSD(aov(Per_state ~ Individual, data = dat_divers_box[dat_divers_box$States=="Neutral",]))$Individual,header=T)

Tukey_ind_Diver_Neutral[Tukey_ind_Diver_Neutral$`p adj`<0.05,]


#Cephalic fins -----------------------------------------------------------



##Cephalic By state -------------------------------------------------------



#Cephalic fins by States


dat_lobes_s<-dat%>% group_by(MantaID)%>%           #Group by states
  mutate(Length_seconds = max(Time_seconds))



dat_lobes_s<-dat_lobes_s%>% group_by(MantaID,Divers,C.fins,States)%>%
  summarise(sec_lobe=n(),per_lobe=(sec_lobe/max(Length_seconds)))%>%
  ungroup() %>%                                          #ungroup
  group_by(MantaID)%>%                                    #add ethogram metadata to filled lines        
  mutate(                                                    
    Divers = first(Divers,na_rm=T)) 

dat_lobes_s<-dat_lobes_s[dat_lobes_s$C.fins!="Unknown",]
dat_lobes_s$Divers[dat_lobes_s$Divers>1]<-1

dat_lobes_sdivers<-dat_lobes_s[dat_lobes_s$Divers>0,]

#Proportion data

dat_prop_states <- dat_lobes_sdivers %>%
  group_by(States, C.fins) %>%
  summarize(total = sum(per_lobe, na.rm=TRUE)) %>%
  group_by(States) %>%
  mutate(prop = total / sum(total))


aov_lobes_sdivers_furled <- aov(per_lobe ~ States, data = dat_lobes_sdivers[dat_lobes_sdivers$C.fins=="Furled",])
summary(aov_lobes_sdivers_furled)

TukeyHSD(aov_lobes_sdivers_furled,"States")
Tukey_lobesf_Diver_states<-as.data.frame(TukeyHSD(aov_lobes_sdivers_furled,"States")$States, header=T)

#Export
write.csv(Tukey_lobesf_Diver_states,"TukeyLobesfDivers_States.csv",row.names = TRUE)

aov_lobes_sdivers_unfurled <- aov(per_lobe ~ States, data = dat_lobes_sdivers[dat_lobes_sdivers$C.fins=="Unfurled",])
summary(aov_lobes_sdivers_unfurled)


TukeyHSD(aov_lobes_sdivers_unfurled,"States")


Tukey_lobesu_Diver<-as.data.frame(TukeyHSD(aov_lobes_sdivers_unfurled,"States")$States, header=T)

#Export
write.csv(Tukey_lobesu_Diver,"TukeyLobesuDivers_states.csv",row.names = TRUE)

#No divers

dat_lobes_snodivers<-dat_lobes_s[dat_lobes_s$Divers<1,]


dat_prop_nd_states <- dat_lobes_snodivers %>%
  group_by(States, C.fins) %>%
  summarize(total = sum(per_lobe, na.rm=TRUE)) %>%
  group_by(States) %>%
  mutate(prop = total / sum(total))

#no tukey, only furled when neutral

aov_lobes_snodivers_unfurled <- aov(per_lobe ~ States, data = dat_lobes_snodivers[dat_lobes_snodivers$C.fins=="Unfurled",])
summary(aov_lobes_snodivers_unfurled)


TukeyHSD(aov_lobes_snodivers_unfurled,"States")

Tukey_lobesu_Nodiver_states<-as.data.frame(TukeyHSD(aov_lobes_snodivers_unfurled,"States")$States, header=T)

#signifficant

#Export
write.csv(Tukey_lobesu_Nodiver_states,"TukeyLobesuNodivers_states.csv",row.names = TRUE)


#No Feeding

dat_lobes_nf<-dat[dat$States!="Feeding",] 


dat_lobes_nf<-dat_lobes_nf%>% group_by(MantaID)%>%           #Group by Ethogram
  mutate(Length_seconds = max(Time_seconds))

dat_lobes_nf<-dat_lobes_nf%>% group_by(MantaID,Divers,C.fins)%>%
  summarise(sec_lobe=n(),per_lobe=(sec_lobe/max(Length_seconds)))%>%
  ungroup() %>%                                          #ungroup
  complete(                                               #complete function
    MantaID, C.fins ,                     #by groups
    fill = list(per_lobe = 0))%>%
  group_by(MantaID)%>%                                    #add ethogram metadata to filled lines        
  mutate(                                                    
    Divers = first(Divers,na_rm=T)) 


dat_lobes_nf<-dat_lobes_nf[dat_lobes_nf$C.fins!="Unknown",]
dat_lobes_nf$Divers[dat_lobes_nf$Divers>1]<-1

aov_cephalic_nofeed <- aov(per_lobe ~ C.fins, data = dat_lobes_nf[dat_lobes_nf$Divers>0,])
summary(aov_cephalic_nofeed)


TukeyHSD(aov_cephalic_nofeed,"C.fins")


##Cephalic By Behavior ----------------------------------------------------



dat_lobes_b<-dat%>% group_by(MantaID)%>%           #Group 
  mutate(Length_seconds = max(Time_seconds))

dat_lobes_b<-dat_lobes_b%>% group_by(MantaID,Divers,C.fins,Behavior)%>%
  summarise(sec_lobe=n(),per_lobe=(sec_lobe/max(Length_seconds)))%>%
  ungroup() %>%                                          #ungroup
  complete(                                               #complete function
    MantaID, Behavior,C.fins ,                     #by groups
    fill = list(per_lobe = 0))%>%
  group_by(MantaID)%>%                                    #add ethogram metadata to filled lines        
  mutate(                                                    
    Divers = first(Divers,na_rm=T)) 

dat_lobes_b<-dat_lobes_b[dat_lobes_b$C.fins!="Unknown",]
dat_lobes_b$Divers[dat_lobes_b$Divers>1]<-1

dat_lobes_bdivers<-dat_lobes_b[dat_lobes_b$Divers>0,]


#Proportional data

dat_prop <- dat_lobes_bdivers %>%
  group_by(Behavior, C.fins) %>%
  summarize(total = sum(per_lobe, na.rm=TRUE)) %>%
  group_by(Behavior) %>%
  mutate(prop = total / sum(total))



aov_lobes_bdivers_furled <- aov(per_lobe ~ Behavior, data = dat_lobes_bdivers[dat_lobes_bdivers$C.fins=="Furled",])
summary(aov_lobes_bdivers_furled)

TukeyHSD(aov_lobes_bdivers_furled,"Behavior")
Tukey_lobesf_Diver<-as.data.frame(TukeyHSD(aov_lobes_bdivers_furled,"Behavior")$Behavior, header=T)

#Export
write.csv(Tukey_lobesf_Diver,"TukeyLobesfDivers_behavior.csv",row.names = TRUE)



aov_lobes_bdivers_unfurled <- aov(per_lobe ~ Behavior, data = dat_lobes_bdivers[dat_lobes_bdivers$C.fins=="Unfurled",])
summary(aov_lobes_bdivers_unfurled)


TukeyHSD(aov_lobes_bdivers_unfurled,"Behavior")

Tukey_lobesu_Diver<-as.data.frame(TukeyHSD(aov_lobes_bdivers_unfurled,"Behavior")$Behavior, header=T)

#Export
write.csv(Tukey_lobesu_Diver,"TukeyLobesuDivers_behavior.csv",row.names = TRUE)

#No Divers
dat_lobes_bnodivers<-dat_lobes_b[dat_lobes_b$Divers<1,]

#Proportional data
dat_prop_nd <- dat_lobes_bnodivers %>%
  group_by(Behavior, C.fins) %>%
  summarize(total = sum(per_lobe, na.rm=TRUE)) %>%
  group_by(Behavior) %>%
  mutate(prop = total / sum(total))

aov_lobes_bnodivers_furled <- aov(per_lobe ~ Behavior, data = dat_lobes_bnodivers[dat_lobes_bnodivers$C.fins=="Furled",])
summary(aov_lobes_bnodivers_furled)


TukeyHSD(aov_lobes_bnodivers_furled,"Behavior")

Tukey_lobesf_Nodiver<-as.data.frame(TukeyHSD(aov_lobes_bnodivers_furled,"Behavior")$Behavior, header=T)

#Export
write.csv(Tukey_lobesf_Nodiver,"TukeyLobesfNodivers_behavior.csv",row.names = TRUE)


aov_lobes_bnodivers_unfurled <- aov(per_lobe ~ Behavior, data = dat_lobes_bnodivers[dat_lobes_bnodivers$C.fins=="Unfurled",])
summary(aov_lobes_bnodivers_unfurled)


TukeyHSD(aov_lobes_bnodivers_unfurled,"Behavior")

Tukey_lobesu_Nodiver<-as.data.frame(TukeyHSD(aov_lobes_bnodivers_unfurled,"Behavior")$Behavior, header=T)

#Export
write.csv(Tukey_lobesu_Nodiver,"TukeyLobesuNodivers_behavior.csv",row.names = TRUE)


##Cephalic Fins Plots -------------------------------------------------------------


#Merged Barplot Behaviors:

dat_prop_div<-dat_prop
dat_prop_div$Divers <- "Divers"

dat_prop_nd$Divers<- "No Divers"

dat_prop_cephalic<-rbind(dat_prop_div, dat_prop_nd)

#ggplot

BarCephalic_merged<-ggplot(dat_prop_cephalic, aes(x = Behavior, y = prop, fill = C.fins)) +
  geom_bar(stat = "identity", position = "fill", color="black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Lobes Time Spent at Each Behavior without Divers",
    x = "Behavior",
    y = "Proportion of Time ",
    fill = "Cephalic Fin Position"
  ) +
  scale_fill_viridis_d(option = "mako", direction=1) +
  theme_minimal() +
  theme(
    text=element_text(size=15),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.position = "top"
  )+
  facet_wrap(~Divers, ncol=1)


BarCephalic_merged  

ggsave("c_fins_Merged_Boxplot.jpg", plot = BarCephalic_merged  , width = 8, height = 10, dpi = 600)


#Merged Barplot States:

dat_prop_div_states<-dat_prop_states
dat_prop_div_states$Divers <- "Divers"

dat_prop_nd_states$Divers<- "No Divers"

dat_prop_cephalic_states<-rbind(dat_prop_div_states, dat_prop_nd_states)

#ggplot

BarCephalic_merged_states<-ggplot(dat_prop_cephalic_states, aes(x = States, y = prop, fill = C.fins)) +
  geom_bar(stat = "identity", position = "fill", color="black") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Lobes Time Spent at Each Behavior State without Divers",
    x = "States",
    y = "Proportion of Time ",
    fill = "Cephalic Fin Position"
  ) +
  scale_fill_viridis_d(option = "mako", direction=1) +
  theme_minimal() +
  theme(
    text=element_text(size=15),
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
    legend.position = "top"
  )+
  facet_wrap(~Divers, ncol=1)


BarCephalic_merged_states  

ggsave("c_fins_Merged_Boxplot_States.jpg", plot = BarCephalic_merged_states  , width = 8, height = 10, dpi = 600)


# Wingbeats ---------------------------------------------------------------

#Separate DF for wingbeats
dat_bx<-dat 
dat_bx$Divers[dat_bx$Divers>1] <-1

#Get mean + sd

summary_df_wing_s <- dat_bx%>%group_by(Divers,States) %>%
  summarise( mean= mean(Wingflaps.second),
             sd = sd(Wingflaps.second))

# Export to CSV (base R)
write.csv(summary_df_wing_s, "wingflap_summary_s.csv", row.names = FALSE)

summary_df_wing <- dat_bx %>%
  group_by(Divers, Behavior) %>%
  summarise(
    mean = mean(Wingflaps.second, na.rm = TRUE),
    sd = sd(Wingflaps.second, na.rm = TRUE)
  )

# Export to CSV (base R)
write.csv(summary_df_wing, "wingflap_summary.csv", row.names = FALSE)



##Signifficance tests Wingbeat --------------------------------------------



#Simple anova, ungruping. Signifficant difference
x<-aov(Wingflaps.second ~ Divers, data = dat_bx)
summary(x)

#All data t--test = Signifficant difference
t.test( Wingflaps.second ~ Divers, data=dat_bx)


# Individual, paired t tests

dat_individual_wingbeat<-dat_bx %>% 
  group_by(Individual, MantaID,Divers) %>% #group
  summarise( Mean_hz = mean(Wingflaps.second)) #get mean hz per individual


dat_individual_wingbeat<-dat_individual_wingbeat %>% 
  group_by(Individual, Divers)%>%
  summarise (Mean_hz = mean(Mean_hz),
             sample=n())

#remove na
dat_individual_wingbeat<- dat_individual_wingbeat[!is.na(dat_individual_wingbeat$Individual),]

#Keep mantas with diver presense/absence only

Indiv_prese_abs<-dat_individual_wingbeat%>%group_by(Individual)%>%
  summarise(Identifier=n())

dat_individual_wingbeat<-left_join(dat_individual_wingbeat,Indiv_prese_abs, by="Individual")

#Final filter
dat_individual_wingbeat<-dat_individual_wingbeat[dat_individual_wingbeat$Identifier>1,]


#T test

t.test(Mean_hz ~ Divers, data=dat_individual_wingbeat)

t.test(dat_individual_wingbeat$Mean_hz[dat_individual_wingbeat$Divers==0] ,dat_individual_wingbeat$Mean_hz[dat_individual_wingbeat$Divers==1],paired=T, alternative = "two.sided")

# Per state aov

#Visualize means
dat_bx%>%group_by(Divers, States)%>%
  summarize(MeanHz=mean(Wingflaps.second))

aov_wing_divers <- aov(Wingflaps.second ~ States, data = dat_bx[dat_bx$Divers>0,])
summary(aov_wing_divers)

TukeyHSD(aov_wing_divers,"States")

aov_wing_nodivers <- aov(Wingflaps.second ~ States, data = dat_bx[dat_bx$Divers==0,])
summary(aov_wing_nodivers)

TukeyHSD(aov_wing_nodivers,"States")


# Per behavior aov

#With divers
aov_wing_bdivers <- aov(Wingflaps.second ~ Behavior, data = dat_bx[dat_bx$Divers>0,])
summary(aov_wing_bdivers)


TukeyHSD(aov_wing_bdivers,"Behavior")



TukeyWingbeatDivers<-as.data.frame(
  TukeyHSD(aov_wing_bdivers,"Behavior")$Behavior, header=T)
write.csv(TukeyWingbeatDivers, "TukeyWingbeatPairedDiv.csv")

#No divers
aov_wing_bnodivers <- aov(Wingflaps.second ~ Behavior, data = dat_bx[dat_bx$Divers==0,])
summary(aov_wing_bnodivers)


TukeyHSD(aov_wing_bnodivers,"Behavior")

TukeyWingbeat<-as.data.frame(
  TukeyHSD(aov_wing_bnodivers,"Behavior")$Behavior, header=T)

write.csv(TukeyWingbeat, "TukeyWingbeatPairedNodiv.csv")


##Wingbeat plots ----------------------------------------------------------

wingbeat_states<-ggplot(dat_bx, aes(x = States, y = Wingflaps.second, fill = States)) +
  geom_boxplot() +
  labs(
    title = "Wingbeat per behavior state",
    x = "Behavior State",
    y = "Mean Wingbeat Per Second",
    fill = "State"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "mako") +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    legend.position = "none"
  ) +
  facet_wrap(
    ~ Divers,
    scales = "fixed",  # <- fixed y axis for all facets!
    ncol = 1,
    labeller = as_labeller(c(`0` = "Without Divers", `1` = "With Divers"))
  ) +
  coord_cartesian(ylim = c(0, 0.7))  # <- set y axis limits

print(wingbeat_states)

ggsave("wingbeat_states.jpg", plot = wingbeat_states, width = 8, height = 6, dpi = 600)



#Boxplot Wingflap Behavior

wingbeat_behav<-ggplot(dat_bx, aes(x = Behavior, y = Wingflaps.second, fill = Behavior)) +
  geom_boxplot() +  # Add boxplot with custom styling
  labs(
    title = "Wingbeat Per Behavior State",
    x = "Behavior",
    y = "Mean Wingbeat Per Second",
    fill = "Behavior"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "mako") +  # Use the same color palette
  theme(
    text=element_text(size=20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size=10),  # Rotate x-axis labels
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Style the title
    legend.position = "none"  # Remove redundant legend
  ) +
  facet_wrap(~ Divers, scales = "fixed", ncol = 1,labeller = as_labeller(c(`0` = "Without Divers", `1` = "With Divers")))+  # Facet by Divers
  coord_cartesian(ylim = c(0, 0.7))  # <- set y axis limits

print(wingbeat_behav)

ggsave("wingbeat_behav.jpg", plot = wingbeat_behav, width = 8, height = 6, dpi = 300)




# Markov model scripts ----------------------------------------------------



##Transition Matrix Formating ---------------------------------------------

#Create sorted vector 
E <- sort(unique(dat$States))  
S = length(E)   

k<-1  #Markov chain order

#Create data list grouping by behaviors

d<-aggregate(dat$States, list(dat$MantaID), FUN=list) 

#Extract grouped behaviors
dd<-d$x 


#Create Transition probability matrix
res3 = estimMk(seq = dd, E = E, k = 1)


#Extract transition probability vector
g<-res3$Ptrans

#Round and name for visualization

g<-round(g,3)
colnames(g)<-E
rownames(g)<-E

#Plot


states<-E

transitionMatrix<-g
colnames(transitionMatrix)<-states
rownames(transitionMatrix)<-states


#simple plot visualization of data TPMs
plotmat(transitionMatrix,
        relsize=0.85,box.size = 0.03,box.prop=.5, arr.type = "simple",
        arr.col = "white",arr.width=0.1,arr.lwd=5 ,arr.length = 0.5, 
        shadow.col = NULL,cex.txt = 0.9, dr = 0.01,dtext = 1,
        self.cex = 1, self.shifty=0.04,self.shiftx=-0.0
)




##Markov Model formulation ------------------------------------------------------------

#Define number of states
N <- 3 

#Extract initial distribution calculated from tpm
initdist <- res3$init  

#Separated ID
index1<-as.integer(as.factor(dat$MantaID))


#Behavior states vector
states.vec1<-as.factor(dat$States)
summary(states.vec1)

#Transform to numeric
states.vec1<-as.numeric(states.vec1)



mat<-dat$Divers  #Covariates... Divers or no divers?
summary(mat)

#Transform to matrix
mat<-as.numeric(mat)
mat[mat>1]<-1  #Just presence absence

length(mat)

#Create covariate matrix. Coefficient will be multiplyed by 0 if divers are absent
xmat<-matrix(NA, nrow=length(mat), ncol=2)

xmat[,1]<-rep.int(1,length(mat))
xmat[,2]<-mat
head(xmat)


## Fit the model------

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan.data <- list(behaviors= states.vec1, 
                  index = index1, 
                  N=3, 
                  T = length(mat),
                  ncov=1,
                  xmat= xmat)


#Note that the model specification is contained in the .stan file. It must exist within your working environment

fit <- stan(file="MantaMarkovChain_Covariates.stan", data=stan.data)

#Explore model results
fit

##Extract marix for plotting----

#transform output
fitm<-as.matrix(fit)

#Custom logit link function for 3 behavioral states

s3solver<-function(matrix=NA) {  
  output <- array(NA, c(3, 3, nrow(matrix)))
  for (j in 1:nrow(matrix)){
    mat <-
      matrix (c( (exp(matrix[j,1])/(exp(matrix[j,1])+ 
                                      exp(matrix[j,7])+ exp(matrix[j,13]))),
                 
                 (exp(matrix[j,7])/(exp(matrix[j,1])+ 
                                      exp(matrix[j,7])+ exp(matrix[j,13]))),
                 
                 (exp(matrix[j,13])/(exp(matrix[j,1])+ 
                                       exp(matrix[j,7])+ exp(matrix[j,13]))),
                 
                 
                 (exp(matrix[j,2])/(exp(matrix[j,2])+
                                      exp(matrix[j,8])+exp(matrix[j,14]))),
                 
                 (exp(matrix[j,8])/(exp(matrix[j,2])+
                                      exp(matrix[j,8])+exp(matrix[j,14]))),
                 
                 (exp(matrix[j,14])/(exp(matrix[j,2])+
                                       exp(matrix[j,8])+exp(matrix[j,14]))),
                 
                 
                 (exp(matrix[j,3])/(exp(matrix[j,3])+
                                      exp(matrix[j,9])+exp(matrix[j,15]))),
                 
                 (exp(matrix[j,9])/(exp(matrix[j,3])+
                                      exp(matrix[j,9])+exp(matrix[j,15]))),
                 
                 (exp(matrix[j,15])/(exp(matrix[j,3])+
                                       exp(matrix[j,9])+exp(matrix[j,15]))))
              
              ,byrow=T, ncol=3)
    
    output[,,j] <- mat      }
  return(output) }

#Custom function for 3 behavioral states, including diver covariate
s3solver1<-function(matrix=NA){    
  output <- array(NA, c(3, 3, nrow(matrix)))
  for (j in 1:nrow(matrix)){
    mat <-
      matrix(c( (exp(matrix[j,1]+matrix[j,4])/(exp(matrix[j,1]+matrix[j,4])+exp(matrix[j,7]+matrix[j,10])+
                                                 exp(matrix[j,13]+matrix[j,16]))),
                
                (exp(matrix[j,7]+matrix[j,10])/(exp(matrix[j,1]+matrix[j,4])+exp(matrix[j,7]+matrix[j,10])+
                                                  exp(matrix[j,13]+matrix[j,16]))),
                
                (exp(matrix[j,13]+matrix[j,16])/(exp(matrix[j,1]+matrix[j,4])+exp(matrix[j,7]+matrix[j,10])+
                                                   exp(matrix[j,13]+matrix[j,16]))),
                
                
                (exp(matrix[j,2]+matrix[j,5])/(exp(matrix[j,2]+matrix[j,5])+exp(matrix[j,8]+matrix[j,11])+
                                                 exp(matrix[j,14]+matrix[j,17]))),
                
                (exp(matrix[j,8]+matrix[j,11])/(exp(matrix[j,2]+matrix[j,5])+exp(matrix[j,8]+matrix[j,11])+
                                                  exp(matrix[j,14]+matrix[j,17]))),
                
                (exp(matrix[j,14]+matrix[j,17])/(exp(matrix[j,2]+matrix[j,5])+exp(matrix[j,8]+matrix[j,11])+
                                                   exp(matrix[j,14]+matrix[j,17]))),
                
                
                
                (exp(matrix[j,3]+matrix[j,6])/(exp(matrix[j,3]+matrix[j,6])+exp(matrix[j,9]+matrix[j,12])+
                                                 exp(matrix[j,15]+matrix[j,18]))),
                
                (exp(matrix[j,9]+matrix[j,12])/(exp(matrix[j,3]+matrix[j,6])+exp(matrix[j,9]+matrix[j,12])+
                                                  exp(matrix[j,15]+matrix[j,18]))),
                
                (exp(matrix[j,15]+matrix[j,18])/(exp(matrix[j,3]+matrix[j,6])+exp(matrix[j,9]+matrix[j,12])+
                                                   exp(matrix[j,15]+matrix[j,18])))) 
             ,byrow=T, ncol=3)
    
    output[,,j] <- mat  
  }
  
  return(output)
}


#Apply function for non diver and diver data (1) 
outBBsn<-s3solver(matrix=fitm)
outBBsn1<-s3solver1(matrix=fitm)

#Neutral 3 to avoidance 1

hist(outBBsn[1,1,])
hist(outBBsn1[1,1,])


#Mean matrices. Single TPM per scenario
mean_TPM_sn <- apply(outBBsn, c(1, 2), mean)
mean_TPM_sn1 <- apply(outBBsn1, c(1, 2), mean)



## Summary Tables ----------------------------------------------------------

#Create dataframes from model
fit_out<-as.data.frame(summary(fit)$summary)
fit_out<-fit_out%>%rownames_to_column(var = "Beta")

#export table as csv
write_csv(fit_out, "ModelOutputs.csv") 


sumfit <- summary(fit)$summary %>%         #Extract summary from 4k iterations
  as.data.frame() %>%                      #Make data frame
  rownames_to_column(var = "Beta") %>%     # Preserve row names as a column
  filter(grepl("^beta", Beta)) %>%         #Keep Beta values only
  select(Beta, mean, `2.5%`, `97.5%`) %>%  #Select and rename upper and lower CIs
  rename(Mean = mean, Lower = `2.5%`, Upper = `97.5%`)


#write table
write_csv(sumfit, "BetaSummaryMantaDrone.csv")

#Summary of signifficant values only

sumfit_sig <- sumfit[with(sumfit, Lower * Upper > 0), ]
write_csv(sumfit_sig, "BetaSignifficativeMantaDrone.csv")



# Ploting Makov results--------------------------------------------------------------------


# TPM without divers
tpmP <- matrix(c(mean(outBBsn[1,1,]), mean(outBBsn[1,2,]), mean(outBBsn[1,3,]),
                 mean(outBBsn[2,1,]), mean(outBBsn[2,2,]), mean(outBBsn[2,3,]),
                 mean(outBBsn[3,1,]), mean(outBBsn[3,2,]), mean(outBBsn[3,3,])), 
               byrow = T, ncol = 3)
tpmPs <- round(tpmP, 3)

# TPM with divers
tpmA <- matrix(c(mean(outBBsn1[1,1,]), mean(outBBsn1[1,2,]), mean(outBBsn1[1,3,]),
                 mean(outBBsn1[2,1,]), mean(outBBsn1[2,2,]), mean(outBBsn1[2,3,]), 
                 mean(outBBsn1[3,1,]), mean(outBBsn1[3,2,]), mean(outBBsn1[3,3,])), 
               byrow = T, ncol = 3)
tpmAs <- round(tpmA, 3)


# Index to check diver effect
indexBBsn <- outBBsn - outBBsn1

# Check distribution and quantiles
hist(indexBBsn[3,3,], breaks = 100)
quantile(indexBBsn[3,3,], probs = c(0.025, 0.975))
quantile(indexBBsn[3,3,], probs = c(0.05))


# Plotting TPM Without divers
plotmat(round(tpmP, 3),
        relsize = 1, box.size = 0.04, box.prop = 0.5, arr.type = "simple",
        arr.col = "white", arr.width = 0.1, arr.lwd = 5, arr.length = 0.5, 
        shadow.col = NULL, cex.txt = 0.9, dr = 0.01, dtext = 1,
        self.cex = 1, self.shifty = 0.04, self.shiftx = -0.0,
        main = "TPM Without Snorkelers")

# Plotting TPM With divers
plotmat(round(tpmA, 3),
        relsize = 1, box.size = 0.04, box.prop = 0.5, arr.type = "simple",
        arr.col = "white", arr.width = 0.1, arr.lwd = 5, arr.length = 0.5, 
        shadow.col = NULL, cex.txt = 0.9, dr = 0.01, dtext = 1,
        self.cex = 1, self.shifty = 0.04, self.shiftx = -0.0,
        main = "TPM With Snorkelers")

# ### Boxplot-----

# Prepare the data

#Extract TPM entries directly from posterior index matrix
x <- c(indexBBsn[1,1,], indexBBsn[1,2,], indexBBsn[1,3,],
       indexBBsn[2,1,], indexBBsn[2,2,], indexBBsn[2,3,],
       indexBBsn[3,1,], indexBBsn[3,2,], indexBBsn[3,3,])


#Set transition names
gp <- c(rep("11", 4000), rep("12", 4000), rep("13", 4000),
        rep("21", 4000), rep("22", 4000), rep("23", 4000),
        rep("31", 4000), rep("32", 4000), rep("33", 4000))

#Merge to data frame
df <- data.frame(value = x, group = gp)

summary(df)

head(df)
unique(df$group)



# Create histogram faceted by 'group'
diffhist<-ggplot(df, aes(x = value)) +
  geom_histogram(binwidth = 0.01, fill = "#357BA2", color = "black", alpha = 0.7) +
  facet_wrap(~ group, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Difference Transition matrix",
       x = "Difference Index",
       y = "Frequency") +
  theme(strip.text = element_text(size = 12, face = "bold"))

#Save histograms
ggsave("Diff Hist.jpg", plot = diffhist, width = 12, height = 8, dpi = 300)


#Create Boxplot
boxplot<-ggplot(df, aes(x = group, y = value, fill = group)) +
  geom_boxplot(outlier.shape = 20, outlier.size = 0) +
  labs(x = "Transitioned State", y = "Diver Effect on Transition",
       title = "Manta Ray Response to Diver Presence") +
  scale_fill_manual(values = c("#357BA2FF", "#DEF5E5FF", "#3E356BFF", 
                               "#357BA2FF", "#DEF5E5FF", "green", 
                               "#357BA2FF", "#3E356BFF", "#DEF5E5FF")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Horizontal labels
    legend.position = "none"  # Remove legend
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = c(3.5, 6.5), linetype = "solid", color = "black") +  # Section dividers
  scale_x_discrete(labels = c("11" = "Avoiding", "12" = "Feeding", "13" = "Neutral",
                              "21" = "Avoiding", "22" = "Feeding", "23" = "Neutral",
                              "31" = "Avoiding", "32" = "Feeding", "33" = "Neutral")) +
  annotate("text", x = 2, y = max(df$value) * 0.9, label = "Avoiding", size = 5) +
  annotate("text", x = 5, y = max(df$value) * 0.9, label = "Feeding", size = 5) +
  annotate("text", x = 8, y = max(df$value) * 0.9, label = "Neutral", size = 5) +
  annotate("text", x = 0.7, y = max(df$value) * 0.4, label = "Higher diver absence effect", angle = 90, size = 4) +
  annotate("text", x = 0.7, y = min(df$value) * 0.7, label = "Higher diver presence effect", angle = 90, size = 4)


#visualize
print(boxplot)


#save
ggsave("MarkovBoxplot_V1.jpg", plot = boxplot, width = 12, height = 8, dpi = 300)

# Dot plots and significant posteriors.  -----------------------------------------------------------------

# Extract the beta estimates from models
beta_summary <- summary(fit, pars = "beta")$summary
fitm


# Convert to a data frame
beta_df <- as.data.frame(beta_summary) %>%
  mutate(param = rownames(beta_summary)) %>%
  filter(grepl("beta", param)) %>%
  separate(param, into = c("param", "from", "diver", "to"), sep = "\\[|,|\\]", extra = "drop") %>%
  mutate(across(c(from, to, diver), as.numeric))

# Rename columns
colnames(beta_df) <- c("mean", "se_mean", "sd", "lower_95ci", "q25", "median", "q75", "upper_95ci", "n_eff", "Rhat", "param", "from", "diver", "to")

# Define correct transition labels
state_labels <- c("Avoidance", "Feeding", "Neutral")
diver_labels <- c("no diver", "with diver")

# Generate correct transition names
beta_df <- beta_df %>%
  mutate(
    transition = paste0(state_labels[from], " → ", state_labels[to], " (", diver_labels[diver], ")"),
    significant = (lower_95ci > 0) | (upper_95ci < 0)  # Significant if 95% CI does not include 0
  )

# Plot
dotplot <- ggplot(beta_df, aes(x = transition, y = mean, color = significant)) +
  geom_point(size = 3) +  # Dot plot instead of bar plot
  geom_errorbar(aes(ymin = lower_95ci, ymax = upper_95ci), width = 0.2) +
  geom_vline(xintercept = c(6.5, 12.5), linetype = "solid", color = "black") +
  scale_color_manual(values = c("#3E356BFF", "#357BA2FF")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    legend.position = "top"
  ) +
  labs(title = "Transition Probabilities with Significant Transitions",
       x = "Transition Type", y = "Beta Value") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  coord_flip()


#Visualize plot
print(dotplot)


#Export figure
ggsave("MarkovDotplot_V1.jpg", plot = dotplot, width = 12, height = 8, dpi = 300)


## As Probabilities-----

# Convert beta estimates to probabilities using the logistic function
beta_df_sig <- beta_df %>%
  mutate(probability = 1 / (1 + exp(-mean)))%>%
  filter(significant == TRUE)  # Keep only significant results

# Plot
dotplot_prob_significant <- ggplot(beta_df_sig, aes(x = transition, y = probability, color = significant)) +
  geom_point(size = 3) +  # Dot plot instead of bar plot
  geom_errorbar(aes(ymin = 1 / (1 + exp(-upper_95ci)), ymax = 1 / (1 + exp(-lower_95ci))), width = 0.2) +
  geom_vline(xintercept = c(4.5), linetype = "solid", color = "black") +
  scale_color_manual(values = c( "#357BA2FF")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 0.5),
    legend.position = "none"
  ) +
  labs(title = "Significant Transitions",
       x = "Transition Type", y = "Transition Probability") +
  # geom_hline(yintercept = 0.5, linetype = "dashed", color = "black") +
  coord_flip()


#Visualize
print(dotplot_prob_significant )


#Export figure
ggsave("MarkovDotplot_Probabilities_V1.jpg", plot = dotplot_prob_significant, width = 12, height = 8, dpi = 300)

# Stationary state plot ---------------------------------------------------

#This does not make a lot of sense for the objectives we have currently. Stationary state plots are nice for movement ecology and movement behavior HMMs. But for us, it results in single probabilities that are not taking as much in consideration as the logit link transformations we applied earlier.

#Leaving it here in case you want to explore it, can be remove later

library(expm) # For matrix power calculations

# Extract beta estimates
beta_summary <- summary(fit, pars = "beta")$summary

# Convert to a data frame
beta_df <- as.data.frame(beta_summary) %>%
  mutate(param = rownames(beta_summary)) %>%
  filter(grepl("beta", param)) %>%
  separate(param, into = c("param", "from", "diver", "to"), sep = "\\[|,|\\]", extra = "drop") %>%
  mutate(across(c(from, to, diver), as.numeric))

# Apply softmax function to convert betas to probabilities
softmax <- function(x) exp(x) / sum(exp(x))

# Compute transition matrices for each diver condition
trans_matrices <- beta_df %>%
  group_by(diver, from) %>%
  summarise(prob = list(softmax(mean)), .groups = "drop") %>%
  pivot_wider(names_from = from, values_from = prob) %>%
  mutate(matrix = pmap(list(`1`, `2`, `3`), ~ rbind(..1, ..2, ..3))) %>%
  select(diver, matrix)


# Compute stationary distribution
compute_stationary <- function(mat) {
  eigen_res <- eigen(t(mat))  # Get left eigenvector
  stat_dist <- Re(eigen_res$vectors[,1]) / sum(Re(eigen_res$vectors[,1]))  # Normalize
  return(stat_dist)
}

stationary_df <- trans_matrices %>%
  mutate(stationary = map(matrix, compute_stationary)) %>%
  unnest_wider(stationary, names_sep = "_") %>%
  pivot_longer(cols = starts_with("stationary_"), names_to = "state", values_to = "probability") %>%
  mutate(state = factor(state, labels = c("Avoidance", "Feeding", "Neutral")))

# Plot stationary probabilities
ggplot(stationary_df, aes(x = state, y = probability, fill = factor(diver))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gray", "blue")) +
  theme_minimal() +
  labs(title = "Stationary State Probabilities",
       x = "Behavioral State", y = "Probability",
       fill = "Diver Presence")



# Error bars --------------------------------------------------------------


# Extract posterior draws for beta
beta_posterior <- rstan::extract(fit, pars = "beta")$beta  # Extract full posterior samples

# Convert to transition probabilities using softmax for each sample
softmax <- function(x) exp(x) / sum(exp(x))

# Compute transition matrices for each sample
compute_transition_matrices <- function(beta_sample) {
  # Get dimensions of beta_sample
  dim_names <- dim(beta_sample)
  
  # Convert array to a tibble with explicit indices
  beta_df <- expand.grid(
    from = seq_len(dim_names[1]),
    diver = seq_len(dim_names[2]),
    to = seq_len(dim_names[3])
  ) %>%
    mutate(value = as.vector(beta_sample))  # Flatten array values into vector
  
  # Compute transition probabilities
  beta_df %>%
    group_by(diver, from) %>%
    summarise(prob = list(softmax(value)), .groups = "drop") %>%
    pivot_wider(names_from = from, values_from = prob) %>%
    mutate(matrix = pmap(list(`1`, `2`, `3`), ~ rbind(..1, ..2, ..3))) %>%
    select(diver, matrix)
}

# Compute stationary distributions for each sample
compute_stationary <- function(mat) {
  eigen_res <- eigen(t(mat))
  stat_dist <- Re(eigen_res$vectors[,1]) / sum(Re(eigen_res$vectors[,1]))  # Normalize
  return(stat_dist)
}

# Iterate over posterior draws
stationary_samples <- map(1:dim(beta_posterior)[1], function(i) {
  trans_matrices <- compute_transition_matrices(beta_posterior[i,,,])
  trans_matrices %>%
    mutate(stationary = map(matrix, compute_stationary))
}) %>% bind_rows()

# Unnest stationary probabilities
stationary_ci_df <- stationary_samples %>%
  unnest_wider(stationary, names_sep = "_") %>%
  pivot_longer(cols = starts_with("stationary_"), names_to = "state", values_to = "probability") %>%
  mutate(state = factor(state, labels = c("Avoidance", "Feeding", "Neutral")))

# Compute 95% credible intervals
stationary_ci <- stationary_ci_df %>%
  group_by(diver, state) %>%
  summarise(
    mean = mean(probability),
    lower = quantile(probability, 0.025),
    upper = quantile(probability, 0.975),
    .groups = "drop"
  )

# Plot with error bars
ggplot(stationary_ci, aes(x = state, y = mean, fill = factor(diver))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("gray", "blue")) +
  theme_minimal() +
  labs(title = "Stationary State Probabilities with 95% CIs",
       x = "Behavioral State", y = "Probability",
       fill = "Diver Presence")

