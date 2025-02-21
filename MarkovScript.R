
# Disclaimer --------------------------------------------------------------

# Scripts and code for an on-going publication. Drone footage unveils the effects of diving with manta rays during feeding events. Gómez-García,Miguel de Jesús *1; Pate, Jessica2. 1 Quantitative Marine Ecology Lab, University of New Hampshire, Durham, New Hampshire, USA. *Correspondence author midejgoga@gmail.com. 2 Marine Megafauna Foundation, Truckee, CA, 96161, USA
# 
# Please note errors and inconsistencies can exist. Scripts are subject to change.




# Libraries and data ------------------------------------------------------


library(tidyverse)
library(parallel)
library(ggplot2)
library(markovchain)
library(SMM)
library(rstan)
library(diagram)
library(bayestestR)


dat<-read.csv("EthogramMantaDroneFootage.csv",header = TRUE) ###Data must be distributed in equal intervals

tag<- read.csv("Metadata.csv",header = TRUE)

#Un IDd videos
length(unique(tag$MantaID[is.na(tag$Individual)]))

#Unique individuals
length(unique(na.exclude(tag$Individual)))



head(dat)
head(tag)

summary(dat)
summary(tag)


# Rename columns to ensure consistency
tag <- tag %>%
  rename(MantaID = `Manta.ID`)


# Merge metadata
dat <- dat %>%
  left_join(tag, by = "MantaID", relationship = "many-to-many")


#Individual metrics 

#Seconds of footage per individual
dat%>%
  group_by(Individual)%>%   #Group by individual
  summarise(seconds=n())    #Seconds recorded

#Number of ethograms per individual

dat%>%
  group_by(Individual)%>%                          #Group by individual
  summarise(Ethograms=length(unique(MantaID)))%>%  #Get unique ethograms
  print(n = 30)                                    #visualize whole table

dat_check<-dat #duplicate for safe editing

#Just keepdiver presence
dat_check$Divers[dat_check$Divers>0]<-1

#Number of ethograms with/without divers

dat_check%>%
  group_by(Individual,Divers)%>%                    #group by individual>Diver presence
  summarise(Ethograms=length(unique(MantaID)))%>%   #Get number of unique ethograms
  print(n = 60)

dat_checkDF<-dat_check%>%                          #create object for export
  data.frame()%>%                                  #as data frame
  group_by(Individual,Divers)%>%                   #group by individual > Diver presence
  summarise(Ethograms=length(unique(MantaID)))%>%  #Unique ethograms
  print(n = 60)


#Write table
write.csv(dat_checkDF, "IndividualMantaEthogramTable.csv")   

#check for errors
dat[dat$Divers==0& dat$States=="Avoidance",]


datsum_uncorrected<-dat %>%               #create object  from data
  group_by(MantaID,Behavior)%>%           #Group by ethogram and behavior
  summarise(Length=length(Time_seconds),  #Length in each behavior
            Divers=max(Divers),           #Max number of divers
            Behaviors=length(unique(Behavior)) #Number of unique behaviors
            )
#you can check individual behaviors like this
length(datsum_uncorrected$Behavior[datsum_uncorrected$Behavior=="Static"])


# Define transition Probability matrices ----------------------------------


#Define behavior states 
unique(dat$Behavior) #check names in file

#Mutate to rename grouped behavior states
dat <- dat %>%
  mutate(States = recode(Behavior, 
                           "Directional Swimming" = "Traveling",
                           "Static" = "Traveling",
                           "CourseChange" = "Traveling",
                           "Feeding?" = "Feeding",
                           "Feeding" = "Feeding",
                           "Acceleration" = "Avoidance",
                           "Avoidance" = "Avoidance"))
#Data Exploration

#Unique Ethograms
length(unique(dat$MantaID))

datsum<-dat %>% group_by(MantaID)%>%                 #Group by Ethogram
        summarise(Length=length(Time_seconds),       #Ethogram length
                  Divers=max(Divers),                #Divers
                  Behaviors=length(unique(Behavior)) #Unique behaviors
        )

#Visualize
print(datsum)


length(datsum$Divers[datsum$Divers==0]) #Number of ethograms without divers

length(datsum$Divers[datsum$Divers>0]) #Ethograms with divers

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




# Markov Model ------------------------------------------------------------

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

#Traveling 3 to avoidance 1

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



# plot --------------------------------------------------------------------


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

# CI plot -----------------------------------------------------------------

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
  geom_vline(xintercept = c(1.5,5.5), linetype = "solid", color = "black") +
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

