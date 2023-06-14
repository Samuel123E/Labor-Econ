rm(list=ls(all=TRUE))

library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stargazer)
library(lmtest)
library(sandwich)
library(patchwork)
library(gtsummary)
library(xtable)


setwd("D:\\Labor_Econ_Term_Paper\\SOEP\\program\\output")

soep_dta <-read_dta("cleaned.dta")

setwd("D:/Labor_Econ_Term_Paper/SOEP/work_data_r")

soep_dta$treatment_d <- ifelse(soep_dta$hourly_inc14 < 8.5, 1,0)
soep_dta <- filter(soep_dta, soep_dta$hourly_inc14 >= 8.5*0.75 & hourly_inc14 <= 8.5*1.25)
soep_dta <- mutate(soep_dta, age = 2015 - brth_year)
soep_dta <- filter(soep_dta, soep_dta$age >=18)

############### Check differences between control and treatment group ############### 

selected_vars <- c("hrs_w14", "age", "sex")

summary_stats <- soep_dta %>%
  group_by(treatment_d) %>%
  summarise(across(all_of(selected_vars), list(mean = mean)),
            n = n())

# Convert the summary_stats dataframe to an xtable object
summary_table <- xtable(summary_stats)

# Set table caption and label
caption(summary_table) <- "Summary Statistics"
label(summary_table) <- "tab:summary_stats"

# Set table alignment for each column
align(summary_table) <- "|l|c|c|c|c|c|"

# Print the LaTeX code for the table
print(summary_table, type = "latex")


############### Plot to test parallel trend assumption ###############

soep_dta_sum <- soep_dta %>% group_by(treatment_d) %>%
  summarise(employ_10 = mean(employ_10), employ_11 =mean(employ_11),employ_12 = mean(employ_12),employ_13 = mean(employ_13), employ_14 = mean(employ_14),employ_15 =mean(employ_15),employ_16 = mean(employ_16), mean(hourly_inc14), sd(hourly_inc14), mean(hrs_w14), sd(hrs_w14), mean(age), sd(age), occ_change_10= mean(occ_change_10), occ_change_11= mean(occ_change_11), occ_change_12= mean(occ_change_12), occ_change_13= mean(occ_change_13), occ_change_14=mean(occ_change_14), occ_change_15=mean(occ_change_15), occ_change_16=mean(occ_change_16), mean(sex), sd(sex))



soep_dta_sum_long <- soep_dta_sum %>%
  pivot_longer(cols = starts_with("employ_"),
               names_to = "year",
               values_to = "employ") %>%
  mutate(year = str_remove(year, "employ_")) %>%
  arrange(treatment_d, year)

soep_dta_sum_long <- mutate(soep_dta_sum_long, year1 = 2000 + as.numeric(year))

# Define custom colors for treatment levels
color_palette <- c("#E69F00", "#56B4E9", "#009E73")

# Create the plot
soep_dta_plot <- ggplot(data = soep_dta_sum_long, aes(year1, employ, color = interaction(treatment_d))) +
  geom_point(size = 4) +
  geom_vline(xintercept = 2014, linetype = "dashed") +
  scale_color_manual(values = color_palette, labels = c("not Affected by Minimum Wage", "Affected by Minimum Wage")) +
  labs(x = "Year", y = "Employment Level") +  # Add title and axis labels
  theme_minimal() +  # Apply a minimal theme
  theme(plot.title = element_text(size = 16, hjust = 0.5),  # Adjust title font size and alignment
        axis.text = element_text(size = 12),  # Adjust axis label font size
        legend.title = element_blank(),  # Remove legend title
        legend.position = "right")  # Adjust legend position


# Save the plot with a white background
ggsave("empl_graph_1.png", plot = soep_dta_plot, width = 10, height = 5.25, bg = "white")
soep_dta_plot



############### Regression ###############

soep_dta_long <- soep_dta %>%
  pivot_longer(cols = starts_with("employ_"),
               names_to = "year",
               values_to = "employ") %>%
  mutate(year = str_remove(year, "employ_")) %>%
  arrange(pid, year)

soep_dta_long$year_d <- ifelse(soep_dta_long$year >=15, 1,0)

soep_dta_long$did = soep_dta_long$year_d*soep_dta_long$treatment_d

soep_dta_long_DID_15 <- filter(soep_dta_long, year == 13 | year == 15)

reg_15_1 <- lm(employ ~ treatment_d + year_d + did, data =soep_dta_long_DID_15)
reg_15_1_robust <- coeftest(reg_15_1, vcov = vcovHC(reg_15_1, type = "HC0"))

reg_15 <- lm(employ ~ treatment_d + year_d + did + age + sex, data =soep_dta_long_DID_15)
reg_15_robust <- coeftest(reg_15, vcov = vcovHC(reg_15, type = "HC0"))
#reg_15_robust


soep_dta_long_DID_16 <- filter(soep_dta_long, year == 13 | year == 16)

reg_16_1 <- lm(employ ~ treatment_d + year_d + did, data =soep_dta_long_DID_16)
reg_16_1_robust <- coeftest(reg_16_1, vcov = vcovHC(reg_16_1, type = "HC0"))

reg_16 <- lm(employ ~ treatment_d + year_d + did + age + sex, data =soep_dta_long_DID_16)
reg_16_robust <- coeftest(reg_16, vcov = vcovHC(reg_16, type = "HC0"))
#reg_16_robust

stargazer(reg_15_1_robust, reg_15_robust, reg_16_1_robust, reg_16_robust)

