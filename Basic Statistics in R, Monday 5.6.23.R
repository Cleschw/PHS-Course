#Title: Basic Statistics in R, Monday 5.6.23

install.packages("usethis")
usethis::use_git_config(user.name = "Cleschw", user.email = "cleo.schwarz@unibe.ch")
usethis::create_github_token()
install.packages("gitcreds")
library(gitcreds)
gitcreds::gitcreds_set()

install.packages("tidyverse")
library(tidyverse)

# exercise 5.6.23 afternoon 
data_ebola <- read_csv("data/raw/ebola.csv")
# filter data frame ebola: cumulative number of confirmed cases in Guinea,Liberia and Sierra Leone before 31 March 2015
data_ebola_cum_cases <- data_ebola |> 
  select(date = Date, country = Country, cum_conf_cases = Cum_conf_cases) |> 
  filter(date <= as.Date("2015-03-31") & (country == "Guinea" | country == "Liberia" | country == "Sierra Leone"))

# create point plot
plot_ebola_point_facet <- ggplot(data = data_ebola_cum_cases, mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + 
  geom_point(alpha = 0.7, shape = 22, size = 1.5, stroke = 1.5) + 
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"), 
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"), 
                    values = c(unibeRedS()[1], unibeMustardS()[1], unibeIceS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
    scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
                 labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
                 limits = as.Date(c("2014-08-28", "2015-04-01"))) +
    scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                       limits = c(0, 10000)) +
    ggtitle(label = "Confirmed Ebola") +
    xlab(label = "Time") +
    ylab(label = "Cum. # of confirmed cases")+
    theme_bw() + theme(legend.position="bottom")+
    facet_grid(cols = vars(country))
print(plot_ebola_point_facet)

# create line plot 
plot_ebola_line_facet <-ggplot(data = data_ebola_cum_cases, 
                          mapping = aes(x = date, y = cum_conf_cases)) + 
  geom_line(mapping = aes(group = country, colour = country), alpha = 0.7, linetype = "dashed", linewidth = 1.5) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 10000, by = 2500),
                     limits = c(0, 10000)) +
   ggtitle(label = "Confirmed Ebola cases") +
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  facet_grid(cols = vars(country))
print(plot_ebola_line_facet)

# create column plot
plot_ebola_col_facet <- ggplot(data = data_ebola_cum_cases, 
                            mapping = aes(x = date, y = cum_conf_cases, fill = country, colour = country)) + geom_col(alpha = 0.7, linetype = "solid", linewidth = 0.1, position = "stack", width = 0.7) +
  scale_fill_manual(name = "Country",
                    breaks = c("Guinea", "Liberia", "Sierra Leone"),
                    values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                    labels = c("GIN", "LBR", "SLE")) +
  scale_colour_manual(name = "Country",
                      breaks = c("Guinea", "Liberia", "Sierra Leone"),
                      values = c(unibeRedS()[1], unibeOceanS()[1], unibeMustardS()[1]),
                      labels = c("GIN", "LBR", "SLE")) +
  scale_x_date(breaks = as.Date(c("2014-08-29", "2014-10-01", "2014-12-01", "2015-02-01", "2015-04-01")),
               labels = c("29 August", "1 October", "1 December", "1 February", "1 April"),
               limits = as.Date(c("2014-08-28", "2015-04-01"))) +
  scale_y_continuous(breaks = seq(from = 0, to = 15000, by = 2500),
                     limits = c(0, 15000)) +
  ggtitle(label = "Confirmed Ebola cases") + 
  xlab(label = "Time") +
  ylab(label = "Cum. # of confirmed cases") +
  theme_bw() + theme(legend.position="bottom") +
  facet_grid(cols = vars(country))
print(plot_ebola_col_facet)

# install Unibecolours 
install.packages("unibeCols", repos = "https://ctu-bern.r-universe.dev")
library(unibeCols)

# new exercise 
install.packages("cowplot")
library(cowplot)

plot_ebola_point_grid <- plot_grid(plotlist = list(plot_ebola_point_facet, plot_ebola_line_facet, plot_ebola_col_facet),
                                   labels = c("V1", "V2", "V3"), label_size = 12, nrow = 2)
print(plot_ebola_point_grid)

# Insurance DATA 
insurance <- read_csv("data/raw/insurance_with_date.csv")
insurance <- insurance |> mutate(children = as.factor(children))
head(insurance)
dim(insurance)

# Density plot (geom_density)
ggplot( insurance, aes(x = bmi, colour = sex, fill = sex)) +
  geom_density( alpha = 0.4 ) +
  theme(text = element_text(size=20), legend.position = "bottom") +
  xlab( expression(paste( "BMI (kg/", m^2,")")) ) + 
  scale_colour_manual(name = "" , values=c("female"=unibePastelS()[1],
                                           "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  scale_fill_manual(name = "", values=c("female"=unibePastelS()[1],
                                        "male"=unibeIceS()[1]), labels = c("Female", "Male")) 

# histogram (geom_histogram)
ggplot( insurance ) + 
  geom_histogram( aes(x = charges, y = after_stat(density), colour = sex, fill = sex ),
                  alpha = 0.4, bins = 100 ) +
  geom_density( aes(x = charges, colour = sex), linewidth = 1.5 ) +
  theme(text = element_text(size=20), legend.position = "top") +
  xlab( "Charges in Dollar" ) + 
  scale_colour_manual(name = "" , values=c("female"=unibePastelS()[1],
                                           "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  scale_fill_manual(name = "", values=c("female"=unibePastelS()[1],
                                        "male"=unibeIceS()[1]), labels = c("Female", "Male")) +
  geom_vline(aes(xintercept = median(charges)), color = unibeRedS()[1], linewidth = 1)

#Exercise Quantiles 
ggplot( insurance , aes(x = age, y = bmi, color =smoker) ) + 
  geom_point(  ) +
  geom_quantile(  ) +
  theme(text = element_text(size=20), legend.position = "top") +
  xlab( "Age (years)" ) + ylab( expression(paste( "BMI (kg/", m^2,")")) ) + 
  scale_colour_manual(name = "" , values=c("no"=unibeRedS()[1],
                                           "yes"=unibeIceS()[1]), labels = c("No", "Yes")) +
  scale_fill_manual(name = "" , values=c("no"=unibeRedS()[1],
                                         "yes"=unibeIceS()[1]), labels = c("No", "Yes")) 
# exercise #1 violin plot
ggplot( insurance , aes(x = smoker, y = charges ) ) + 
  ylab( "Charges ($)" ) +
  geom_violin(  )
# part II: box plot -> coord_flip changes x and y 
ggplot( insurance , aes(x = smoker, y = charges ) ) + 
  geom_boxplot(  ) + 
  ylab( "Charges ($)" ) + 
  coord_flip()
