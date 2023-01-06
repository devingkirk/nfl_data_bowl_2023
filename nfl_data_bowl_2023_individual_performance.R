# Load required packages #
library(tidyverse)
library(lme4)
library(gmodels)
library(scales)

# Clean environment #
rm(list=ls())

# Load in different data sets #
players <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/players.csv")

games <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/games.csv")

pff <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/pffScoutingData.csv")

plays <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/plays.csv")

week1 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week1.csv")
week2 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week2.csv")
week3 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week3.csv")
week4 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week4.csv")
week5 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week5.csv")
week6 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week6.csv")
week7 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week7.csv")
week8 <- read.csv("~/Desktop/NFL 2023 Data Bowl/data/week8.csv")

 # Combine weekly next gen data into one large matrix #
all_tracking <- rbind(week1,week2,week3,week4,week5,week6,week7,week8)


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

# Filter players for linemen #
players <- players %>%
  filter(officialPosition %in% c("T","C","G","DE","NT","DT"))


# Create an index of player ID (nflId), which we can then use this to filter the other data sets #
players_index <- players$nflId

# Create dataframe to save model output for every player #
model_output <- data.frame(
  nflID = rep(NA,length(players_index)),
  name = rep(NA,length(players_index)),
  position = rep(NA, length(players_index)),
  total_plays = rep(NA,length(players_index)),
  max_acc_int = rep(NA,length(players_index)),
  max_acc_slope = rep(NA,length(players_index)),
  max_acc_p = rep(NA,length(players_index))
)


# This will loop through each player separately #
for(i in 1:length(players_index)){
    
# Subset the tracking data for player i #
tracking_sub <- all_tracking %>%
  filter(nflId %in% players_index[[i]])

# Subset the PFF data for player i #
pff_sub <- pff %>%
  filter(nflId == players_index[[i]])

# Get the player name for player i #
players_sub <- players %>%
  filter(nflId == players_index[[i]]) 
# 
# player_name <- players_sub$displayName
# player_pos <- players_sub$officialPosition


# For each play in the dataset, calculate player i's max acceleration #
options(dplyr.summarise.inform = FALSE)
movement_data <- tracking_sub %>%
  group_by(gameId, playId) %>%
  summarise(max_speed = max(s),
            max_acc = max(a),
            mean_speed = mean(s),
            mean_acc = mean(a))

# Bind the true time data to the movement data to the PFF data #
grouped_data <- movement_data %>%
  left_join(plays, by = c("gameId","playId")) %>%
  left_join(pff_sub, by = c("gameId","playId"))


# Just select relevant features #
# Note some of these are relevant for some positions but not others #
grouped_data <- grouped_data %>%
  dplyr::select(c("nflId","gameId","playId","true_time",
                  "max_speed","max_acc","mean_speed","mean_acc",
                  "pff_hit","pff_hurry","pff_sack",
                  "pff_beatenByDefender","pff_hitAllowed","pff_hurryAllowed","pff_sackAllowed"))


# Ungroup data now for next steps #
final_data <- ungroup(grouped_data)


# For players who played at least 50 plays over the 8 games, fit linear models to the max acceleration data #
if(dim(final_data)[[1]] >= 50){
max_acc_mod <- lm(max_acc ~ true_time, data=final_data)
}

# Save model output #
model_output$nflID[[i]] <-  unique(final_data$nflId)
model_output$name[[i]] <- players_sub$displayName
model_output$position[[i]] <- players_sub$officialPosition
model_output$total_plays[[i]] <- dim(final_data)[[1]]

# If they played 50 or greater plays save the model output, else save as NA for that player #
ifelse((dim(final_data)[[1]] >= 50),
       c(model_output$max_acc_int[[i]] <- summary(max_acc_mod)$coefficients[[1]],  # Intercept coefficient 
model_output$max_acc_slope[[i]] <- summary(max_acc_mod)$coefficients[[2]], # Slope coefficient (for time)
model_output$max_acc_p[[i]] <- summary(max_acc_mod)$coefficients[[8]]), # p-value for slope 
c(model_output$max_acc_int[[i]] <- NA, # Intercept coefficient 
  model_output$max_acc_slope[[i]] <- NA,# Slope coefficient (for time)
  model_output$max_acc_p[[i]] <- NA)) # p-value for slope 

}

# End of loop #


# Write model output to file #
write.csv(model_output, "~/Desktop/model_results.csv")





### Look at data #
clean_output <- model_output %>%
  filter(total_plays >= 50)  

# Separate out for offensive line and defensive line #
dline_output <- clean_output %>%
  filter(position %in% c("DE","NT","DT"))

oline_output <- clean_output %>%
  filter(position %in% c("T","C","G"))


# Want to find top 20% of coefficients, bottom 20% of coefficients, and middle 80% #
dline_output <- dline_output %>%
  mutate(max_acc_slope_cat = case_when(max_acc_slope > quantile(dline_output$max_acc_slope, 0.8) ~ 'riser',
                                       max_acc_slope < quantile(dline_output$max_acc_slope, 0.8) & 
                                         max_acc_slope > quantile(dline_output$max_acc_slope, 0.2)  ~ 'steady',
                                       max_acc_slope < quantile(dline_output$max_acc_slope, 0.2) ~ 'faller')) %>%
  mutate(max_acc_int_cat = case_when(max_acc_int > quantile(dline_output$max_acc_int, 0.8) ~ 'fast',
                                     max_acc_int < quantile(dline_output$max_acc_int, 0.8) & 
                                       max_acc_int > quantile(dline_output$max_acc_int, 0.2)  ~ 'medium',
                                     max_acc_int < quantile(dline_output$max_acc_int, 0.2) ~ 'slow'))


oline_output <- oline_output %>%
  mutate(max_acc_slope_cat = case_when(max_acc_slope > quantile(oline_output$max_acc_slope, 0.8) ~ 'riser',
                                       max_acc_slope < quantile(oline_output$max_acc_slope, 0.8) & 
                                         max_acc_slope > quantile(oline_output$max_acc_slope, 0.2)  ~ 'steady',
                                       max_acc_slope < quantile(oline_output$max_acc_slope, 0.2) ~ 'faller')) %>%
  mutate(max_acc_int_cat = case_when(max_acc_int > quantile(oline_output$max_acc_int, 0.8) ~ 'fast',
                                     max_acc_int < quantile(oline_output$max_acc_int, 0.8) & 
                                       max_acc_int > quantile(oline_output$max_acc_int, 0.2)  ~ 'medium',
                                     max_acc_int < quantile(oline_output$max_acc_int, 0.2) ~ 'slow'))




# Look at cross tables #
defensive_cross_table <- CrossTable(x=dline_output$max_acc_int_cat, y=dline_output$max_acc_slope_cat,
                          prop.chisq=FALSE, expected = FALSE, prop.t=TRUE,prop.r=FALSE,prop.c=TRUE,
                          dnn= c("Max acceleration at start of game","Change in max acceleration over a game"),
                          format="SAS")

offensive_cross_table <- CrossTable(x=oline_output$max_acc_int_cat, y=oline_output$max_acc_slope_cat,
                                    prop.chisq=FALSE, expected = FALSE, prop.t=TRUE,prop.r=FALSE,prop.c=TRUE,
                                    dnn= c("Max acceleration at start of game","Change in max acceleration over a game"),
                                    format="SAS")


# Plot is too cluttered with all defensive line positions; instead will take a look at Defensive ends #
# Names of a few players are withheld if they overlap with other text #

dline_DE <- dline_output %>% 
  filter(position == "DE")

DE.p <- ggplot(dline_DE, aes(max_acc_slope,max_acc_int)) +
  theme_classic() +
   theme(axis.text=element_text(size=12),
         axis.title=element_text(size=16,face="bold"),
         plot.title= element_text(size=18, face="bold")) +
  geom_point(pch=21, colour=alpha("black",0.7),fill=alpha("red2",0.7), size=4, alpha=0.75) +
  geom_vline(xintercept=0) + ylim(2.2,4.6) + xlim(-0.016,0.02) +
  geom_hline(yintercept = median(dline_DE$max_acc_int)) +
  geom_text(aes(label=name), fontface="bold",size=3.5, nudge_y = 0.04, alpha=1, check_overlap = TRUE) +
  labs(title = "Defensive Ends: change in performance over the course of a game") +
  ylab("Performance at start of game (max acceleration") +
  xlab("Rate of change in performance over game") +
  scale_colour_discrete(name = "Official\nposition") +
  geom_curve(aes(x=0,y=4.5,xend=0.016,yend=4.5), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=0.007,y=4.55, label="Performance increases over game",fontface="bold")) +
  geom_curve(aes(x=0,y=4.5,xend=-0.015,yend=4.5), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=-0.007,y=4.55, label="Performance decreases over game",fontface="bold")) +
  geom_curve(aes(x=0,y=median(max_acc_int),xend=0,yend=4.5), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=0,y=4.3, label="Higher performance\n at the start of game",fontface="bold")) +
  geom_curve(aes(x=0,y=median(max_acc_int),xend=0,yend=2.2), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=0,y=2.4, label="Lower performance\n at the start of game",fontface="bold"))



ggsave(filename = "~/Desktop/defensive_ends.png", plot = DE.p,
       height=8,width=12, units="in")




# Repeat for offensive linemen here #
# Guards #

oline_G <- oline_output %>% 
  filter(position == "G")

G.p <- ggplot(oline_G, aes(max_acc_slope,max_acc_int)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title= element_text(size=18, face="bold")) +
  geom_point(pch=21, colour=alpha("black",0.7),fill=alpha("green3",0.7), size=4, alpha=0.75) +
  geom_vline(xintercept=0) + ylim(1.9,3.4) + xlim(-0.016,0.02) +
  geom_hline(yintercept = median(oline_G$max_acc_int)) +
  geom_text(aes(label=name), fontface="bold",size=3.5, nudge_y = 0.04, alpha=1, check_overlap = TRUE) +
  labs(title = "Guards: change in performance over the course of a game") +
  ylab("Performance at start of game (max acceleration") +
  xlab("Rate of change in performance over game") +
  scale_colour_discrete(name = "Official\nposition") +
  geom_curve(aes(x=0,y=3.35,xend=0.016,yend=3.35), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=0.007,y=3.4, label="Performance increases over game",fontface="bold")) +
  geom_curve(aes(x=0,y=3.35,xend=-0.015,yend=3.35), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=-0.007,y=3.4, label="Performance decreases over game",fontface="bold")) +
  geom_curve(aes(x=0,y=median(max_acc_int),xend=0,yend=3.35), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=0,y=3.25, label="Higher performance\n at the start of game",fontface="bold")) +
  geom_curve(aes(x=0,y=median(max_acc_int),xend=0,yend=1.9), curvature=0,
             colour="black",size=0.5, arrow=arrow(length=unit(0.02,"npc"),type="closed")) +
  geom_label(aes(x=0,y=2, label="Lower performance\n at the start of game",fontface="bold"))


ggsave(filename = "~/Desktop/guards.png", plot = G.p,
       height=8,width=12, units="in")



# Use Shaq Lawson as an example for a figure #


# Subset the tracking data for player i #
tracking_sub_sl <- all_tracking %>%
  filter(nflId == 43308)

# Subset the PFF data for player i #
pff_sub_sl <- pff %>%
  filter(nflId == 43308)

# Get the player name for player i #
players_sub_sl <- players %>%
  filter(nflId == 43308)
# 


# For each play in the dataset, calculate player i's max acceleration #
options(dplyr.summarise.inform = FALSE)
movement_data_sl <- tracking_sub_sl %>%
  group_by(gameId, playId) %>%
  summarise(max_speed = max(s),
            max_acc = max(a),
            mean_speed = mean(s),
            mean_acc = mean(a))

# Bind the true time data to the movement data to the PFF data #
grouped_data_sl <- movement_data_sl %>%
  left_join(plays, by = c("gameId","playId")) %>%
  left_join(pff_sub, by = c("gameId","playId"))


# Just select relevant features #
# Note some of these are relevant for some positions but not others #
grouped_data_sl <- grouped_data_sl %>%
  dplyr::select(c("nflId","gameId","playId","true_time",
                  "max_speed","max_acc","mean_speed","mean_acc",
                  "pff_hit","pff_hurry","pff_sack",
                  "pff_beatenByDefender","pff_hitAllowed","pff_hurryAllowed","pff_sackAllowed"))


# Ungroup data now for next steps #
final_data_sl <- ungroup(grouped_data_sl)


sl.p <- ggplot(final_data_sl, aes(true_time,max_acc)) +
  theme_classic() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title= element_text(size=18, face="bold")) +
  geom_point(pch=21, colour="black",fill="grey", size=5) +
  geom_smooth(method='lm',formula=y~x,color="black",size=3) +
  labs(title = "Shaq Lawson performance over the course of a game") +
  ylab("Performance (max acceleration") +
  xlab("Game time (minutes)") 

sl.p



ggsave(filename = "~/Desktop/shaw_lawson.png", plot = sl.p,
       height=8,width=12, units="in")


