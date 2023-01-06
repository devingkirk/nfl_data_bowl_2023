# Load required packages #
library(tidyverse)
library(lme4)
library(gmodels)
library(scales)
library(nflplotR)

# Clean environment #
rm(list=ls())

# Load in different data sets #
players <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/players.csv")

games <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/games.csv")

# pff <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/pffScoutingData.csv")

plays <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/plays.csv")

# week1 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week1.csv")
# week2 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week2.csv")
# week3 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week3.csv")
# week4 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week4.csv")
# week5 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week5.csv")
# week6 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week6.csv")
# week7 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week7.csv")
# week8 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week8.csv")

# Combine weekly next gen data into one large matrix #
#all_tracking <- rbind(week1,week2,week3,week4,week5,week6,week7,week8)


# Create new feature: actual game time for each play #

# First convert gameClock to decimal in new column #
plays <- plays %>%
  mutate(decimal_clock = sapply(strsplit(plays$gameClock,":"),
                                function(x) {
                                  x <- as.numeric(x)
                                  x[1]+x[2]/60
                                }
  ))

# Now convert into 0 - 60 #
plays <- plays %>%
  mutate(true_time = case_when(quarter == 1 ~ c(15-decimal_clock),
                               quarter == 2 ~ c(30-decimal_clock),
                               quarter == 3 ~ c(45-decimal_clock),
                               quarter == 4 ~ c(60-decimal_clock)))



# new column with binary data for whether a sack occurred 
plays <- plays %>%
  mutate(sack_occurred = case_when(passResult == "S" ~ 1,
                                   passResult != "S" ~ 0))



# Index of the nfl teams #
team_index <- unique(plays$possessionTeam)

# Create dataframe to save model output for each team #
OL_team_model_output <- data.frame(
  nfl_team = rep(NA,32),
  oline_perf_int = rep(NA,32),
  oline_perf_slope = rep(NA,32),
  oline_perf_p = rep(NA,32),
  oline_total_sacks = rep(NA,32)
)


# Run the model separately for each team #

for(i in 1:length(team_index)){
  
  # Subset the plays for the team in question #
  final_data <- plays %>%
    filter(possessionTeam  %in% team_index[i])
  
 
  # Fit logistic model 
  oline_perf_glm <- glm(sack_occurred ~ true_time, family = binomial(link = "logit"), data=final_data)
  
  
  # Save model output #
  OL_team_model_output$nfl_team[[i]] <- unique(final_data$possessionTeam)
  OL_team_model_output$oline_perf_int[[i]] <- summary(oline_perf_glm)$coefficients[[1]]  # Intercept coefficient 
  OL_team_model_output$oline_perf_slope[[i]] <- summary(oline_perf_glm)$coefficients[[2]] # Slope coefficient (for time)
  OL_team_model_output$oline_perf_p[[i]] <- summary(oline_perf_glm)$coefficients[[8]] # p-value for slope 
  OL_team_model_output$oline_total_sacks[[i]] <- sum(final_data$sack_occurred) # p-value for slope 
  
}

# End of loop #
# Create new column with rank order #
OL_team_model_output <- OL_team_model_output %>%
  mutate(rank_order = rank(oline_total_sacks, ties.method = "first"))



OL_team_model_output



# Repeat for defensive line #
# Instead of the team in possession, it is the defensiveTeam #

# Create dataframe to save model output for each team #
DL_team_model_output <- data.frame(
  nfl_team = rep(NA,32),
  dline_perf_int = rep(NA,32),
  dline_perf_slope = rep(NA,32),
  dline_perf_p = rep(NA,32),
  dline_total_sacks = rep(NA,32)
)


# Run the model separately for each team #

for(i in 1:length(team_index)){
  
  # Subset the plays for the team in question #
  final_data <- plays %>%
    filter(defensiveTeam  %in% team_index[i])
  
  
  # Fit logistic model 
  dline_perf_glm <- glm(sack_occurred ~ true_time, family = binomial(link = "logit"), data=final_data)
  
  
  # Save model output #
  DL_team_model_output$nfl_team[[i]] <- unique(final_data$defensiveTeam)
  DL_team_model_output$dline_perf_int[[i]] <- summary(dline_perf_glm)$coefficients[[1]]  # Intercept coefficient 
  DL_team_model_output$dline_perf_slope[[i]] <- summary(dline_perf_glm)$coefficients[[2]] # Slope coefficient (for time)
  DL_team_model_output$dline_perf_p[[i]] <- summary(dline_perf_glm)$coefficients[[8]] # p-value for slope 
  DL_team_model_output$dline_total_sacks[[i]] <- sum(final_data$sack_occurred) # p-value for slope 
  
}

# End of loop #

# Create new column with rank order #
DL_team_model_output <- DL_team_model_output %>%
  mutate(rank_order = rank(-dline_total_sacks, ties.method = "first"))

DL_team_model_output



# Make plots #

DL_plot <- ggplot(DL_team_model_output, aes(x=factor(rank_order),exp(dline_perf_slope))) +
  geom_segment( aes(x=factor(rank_order), xend=factor(rank_order),y=1, yend=exp(dline_perf_slope)), color="black", size=1) +
 # geom_point(color="black", size=4) + #ylim(0.8,1.2) +
  theme_classic() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title= element_text(size=18, face="bold")) +
  geom_hline(yintercept = 1) +
  labs(title = "Team defensive line: Change in odds of getting a sack per additional minute of game play") +
  ylab("Change in odds of getting a sack per minute") +
  xlab("Rank in team sack rate") +
  geom_nfl_logos(aes(x=rank_order,exp(dline_perf_slope),team_abbr=nfl_team),
                 width=0.075,height=0.075)


DL_plot


ggsave(filename = "~/Desktop/DL_unit.png", plot = DL_plot,
       height=8,width=12, units="in")





OL_plot <- ggplot(OL_team_model_output, aes(x=factor(rank_order),exp(oline_perf_slope))) +
  geom_segment( aes(x=factor(rank_order), xend=factor(rank_order),y=1, yend=exp(oline_perf_slope)), color="black", size=1) +
  # geom_point(color="black", size=4) + #ylim(0.8,1.2) +
  theme_classic() + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title= element_text(size=18, face="bold")) +
  geom_hline(yintercept = 1) +
  labs(title = "Team offensive line: Change in odds of giving up a sack per additional minute of game play") +
  ylab("Change in odds of giving up a sack per minute") +
  xlab("Rank in team sack rate") +
  geom_nfl_logos(aes(x=rank_order,exp(oline_perf_slope),team_abbr=nfl_team),
                 width=0.075,height=0.075)


OL_plot


ggsave(filename = "~/Desktop/OL_unit.png", plot = OL_plot,
       height=8,width=12, units="in")


