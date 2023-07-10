# Madagascar impact evaluation
# plot deforestation in matched CFM and MNP pixels
# June 5 2023

library(tidyverse)

# Plot deforestation in matched CFM, PA pixels -----------------

matched_yr_wider <- read_csv('outputs/matched_yr_wider_90m_genetic_defor_annual_development_security_27Feb2023.csv') #update date
#matched_yr_wider$year <- as.factor(matched_yr_wider$year) #convert "year" to a factor

names(matched_yr_wider)

defor_annual_plot1 <- matched_yr_wider %>%
  group_by(year, PACFM) %>%
  summarise(mean_defor = mean(defor_annual), sd_defor = sd(defor_annual)) %>%
  ggplot(mapping = aes(x=year, y=mean_defor, colour=PACFM, fill=PACFM)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin=mean_defor-sd_defor, ymax=mean_defor+sd_defor), alpha=0.3) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_discrete(name  ="CFM or MNP",
                        breaks=c("CFM", "PA"),
                        labels=c("CFM", "MNP")) +
  scale_shape_discrete(name  ="CFM or MNP",
                       breaks=c("CFM", "PA"),
                       labels=c("CFM", "MNP")) +
  scale_fill_discrete(name  ="CFM or MNP",
                      breaks=c("CFM", "PA"),
                      labels=c("CFM", "MNP")) +
  labs(
    x = "Year",
    y = "Mean deforestation (percent)",
    title = "Mean annual deforestation in matched CFM and MNP samples") +
  #scale_x_continuous(breaks=seq(2005,2020,1))+
  theme_minimal()

defor_annual_plot1

defor_annual_plot2 <- matched_yr_wider %>%
  group_by(year, PACFM) %>%
  summarise(mean_defor = mean(defor_annual), sd_defor = sd(defor_annual)) %>%
  ggplot(mapping = aes(x=year, y=mean_defor, colour=PACFM)) +
  geom_line() +
  geom_point(aes(shape=PACFM)) +
  geom_errorbar(aes(ymin=mean_defor-sd_defor, ymax=mean_defor+sd_defor),position = "dodge", width = 0.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_discrete(name  ="CFM or MNP",
                        breaks=c("CFM", "PA"),
                        labels=c("CFM", "MNP")) +
  scale_shape_discrete(name  ="CFM or MNP",
                       breaks=c("CFM", "PA"),
                       labels=c("CFM", "MNP")) +
  labs(
    x = "Year",
    y = "Mean deforestation (percent)",
    title = "Mean annual deforestation in matched CFM and MNP samples") +
  theme_minimal() 

defor_annual_plot2
