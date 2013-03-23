# This script contains operations on RunsPerGame.txt for Question 5 of HW2
baseball_stats <- read.delim("~/development/data_mining/RunsPerGame.txt")

# Generate the linear models
rpg_model <- lm(formula=RPG~AVG+SLG+OBP, data=baseball_stats)
rpg_model_obp <- lm(formula=RPG~OBP, data=baseball_stats)
rpg_model_slg <- lm(formula=RPG~SLG, data=baseball_stats)
rpg_model_avg <- lm(formula=RPG~AVG, data=baseball_stats)

# Print a summary
summary(rpg_model)

# Viewing ANOVA statitistics for model
anova(rpg_model)

# Compare the two model 
rpg_model_with_slg <- lm(formula=RPG~OBP+SLG, data=baseball_stats)
rpg_model_with_avg <- lm(formula=RPG~OBP+AVG, data=baseball_stats)

anova(rpg_model_with_slg)
anova(rpg_model_with_avg)