
library(tidyverse)
library(ggplot2)
library(patchwork)
library(readxl)
library(janitor)


SURVEY_BIAS_CORRECTION = TRUE # Determines if the bias correction factor be used. Set to false if not.

# read data used
PrEP_current  <- read.csv('data/PrEP_current.csv') # For the estimate of the HIV-PrEP population
pop_data      <- read.csv('data/pop_data.csv') # For the total tetracycline and doxycycline consumption imputation
pop_data_male <- read.csv('data/pop_data_male.csv') # For the estimate of the HIV-PrEP population



source('code/eligible_population_EMIS2024_func.R')  
source('code/survey_exp_elicitation_v1.R')
source('code/calc_total_ddd.R')



factor_survey_bias = 1
if(SURVEY_BIAS_CORRECTION){
  factor_survey_bias = 2.288527 # 
}



# Set number of simulations
n_simulations <- 10000 # number of simulations increased!
set.seed(12345)


# Run the simulation and store all relevant quantities
simulated_data <- replicate(n_simulations, {
  
  # generate the number of people on HIV-PrEP, the eligible population who will uptake doxyPEP
  eligible_pop <- eligible_population_EMIS_func2024()/factor_survey_bias  

  # The uptake fraction among those on HIV-PrEP and the number of occacions used among those taking doxy-PEP is
  # estimated from the expert elicitation. Here we draw one observation each 
  uptake_fraction <- rbeta(1, 
                           shape1 = uptake_fraction_beta_parms["alpha"], 
                           shape2 = uptake_fraction_beta_parms["beta"])
  
  uptake_pop <- round(eligible_pop * uptake_fraction)
  
  est_occacions <- rgamma(1, shape = estimated.shape, rate = estimated.rate)
  
  
  # To compare to the number of MSM using PrEP that are infected by syphilis
  # From systematic review https://www.ecdc.europa.eu/en/publications-data/systematic-review-chlamydia-gonorrhoea-trichomoniasis-and-syphilis-prevalence
  
  Mean_Syph = 6.48
  LowerCI_Syph = 3.95
  UpperCI_Syph = 9.02
  SD1=(Mean_Syph-LowerCI_Syph)/(1.96)
  SD2=(UpperCI_Syph-Mean_Syph)/(1.96)
  SD = mean(SD1,SD2)
  

  syphilis_infected = eligible_pop * rnorm(1,mean=Mean_Syph, sd=SD)/100
  Treatments = uptake_pop * est_occacions
  Treat_per_infected = Treatments/syphilis_infected
  
  
  c(eligible_pop = eligible_pop,
    uptake_fraction = uptake_fraction,
    uptake_population = uptake_pop,
    occasions_per_user = est_occacions,
    Treat_per_infected = Treat_per_infected)
  
}, simplify = TRUE)

# Convert to data frame for plotting and analysis
simulated_df <- as.data.frame(t(simulated_data))

# Convert uptake population to doses (DDD) using average number of condomless intercouse
# note that one doxyPEP course = 2 DDDs for doxycycline
simulated_df$doses <- round(simulated_df$uptake_population * simulated_df$occasions_per_user * 2) 

# -----------------------------------------------------
# Add averted doxycycling doses to calculation
# Traeger, M. W., et al. (2023). "Potential impact of doxycycline post-exposure prophylaxis prescribing strategies on incidence of bacterial sexually transmitted infections." Clin Infect Dis.
# -----------------------------------------------------

NNT = 4.6 
doses_per_case <- 2*7 # typical treatment for Chlamydia is 100g of doxy twice a day for one week. DDD for doxy is 100g, so total DDDs is 14.

simulated_df <- 
  simulated_df %>% 
  mutate(
    cases_averted = round(uptake_population/NNT),
    doses_averted = round(cases_averted * doses_per_case),
    net_doses = doses - doses_averted
    #net_doses = doses # if you want to disregard effect of chlamydia treatment
  )

# -----------------------------------------------------
# Compute summary statistics

# MSM using PrEP (eligible population)
round(quantile(simulated_df$eligible_pop, probs = c(0.025, 0.5, 0.975))) 

# % MSM using PrEP adopting doxy-PEP
round(100*quantile(simulated_df$uptake_fraction, probs = c(0.025, 0.5, 0.975)),2)

# Number of eligible population uptaking doxy-PEP
round(quantile(simulated_df$uptake_fraction*simulated_df$eligible_pop, probs = c(0.025, 0.5, 0.975)))  

# Courses per user per year 
round(quantile(simulated_df$occasions_per_user, probs = c(0.025, 0.5, 0.975)))   

# Gross DDD/year from doxy-PEP
round(quantile(simulated_df$doses, probs = c(0.025, 0.5, 0.975)))

# Averted DDD/year (chlamydia prevention)
round(quantile(simulated_df$doses_averted, probs = c(0.025, 0.5, 0.975)))   

# Net doxy-PEP DDD/year (Net doses (total - averted) in DDDs)
round(quantile(simulated_df$net_doses, probs = c(0.025, 0.5, 0.975)))

# Number of doxy-PEP courses per estimated syphilis infected MSM using PrEP
round(quantile(simulated_df$Treat_per_infected, probs = c(0.025, 0.5, 0.975)))   

# -----------------------------------------------------
# Figures



p1 <- ggplot(simulated_df, aes(x = eligible_pop/1000)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  labs(title = "", x = "MSM using PrEP (x1000)", y = "Density")

p2 <- ggplot(simulated_df, aes(x = uptake_fraction)) +
  geom_density(fill = "darkgreen", alpha = 0.5) +
  labs(title = "", x = "Proportion taking up DoxyPEP", y = "")


p3 <- ggplot(simulated_df, aes(x = occasions_per_user)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "", x = "Yearly courses per user", y = "Density")  +
  xlim(c(0, 100))

net_doses_max <- function(x){
  res = ceiling(1.1*quantile(simulated_df$net_doses/100000, probs = c(0.975))/x)*x
  return(res)
}

p4 <- ggplot(simulated_df, aes(x = net_doses/(100000))) +
  geom_density(fill = "darkred", alpha = 0.5) +
  labs(title = "", x = "Net yearly DDDs (x100 000)", y = "")+
  xlim(c(0, net_doses_max(50) ))



(p1 | p2 | p3 | p4) + plot_layout(ncol = 2, nrow =2) +
  plot_annotation(tag_levels = "A")  

FIG_NAME_2x2 = "All_Combined_yearly_2x2.png"
FIG_NAME_2x2_pdf = "All_Combined_yearly_2x2.pdf"

if(SURVEY_BIAS_CORRECTION){
  FIG_NAME_2x2 = "All_Combined_yearly_2x2_Survey_bias.png" 
  FIG_NAME_2x2_pdf = "All_Combined_yearly_2x2_Survey_bias.pdf"

}



#png(filename = FIG_NAME_2x2, res= 290,height=1200,width=1550,pointsize=7)
  
  (p1 | p2 | p3 | p4) + plot_layout(ncol = 2, nrow =2) +
    plot_annotation(tag_levels = "A")  # Adds "A", "B", "C"
  
#dev.off()
  
#pdf(file = FIG_NAME_2x2_pdf, height=5.5*(1200/1550),width=5.5,pointsize=7)
  
  (p1 | p2 | p3 | p4) + plot_layout(ncol = 2, nrow =2) +
    plot_annotation(tag_levels = "A")  # Adds "A", "B", "C"
  
#dev.off()


  




# -----------------------------------------------------
# compute total doxy and tetra dispensed


## Read the sheet of the Excel file from ATC data 
atc_dat <- read_excel("data/J01AA_2023_data_250821.xlsx", sheet = 2) %>% 
  janitor::clean_names()

# Get total DDDs once
totals <- calculate_total_ddd(IMPUTE = TRUE)

# Get quantiles of simulated doses
dose_q <- round(quantile(simulated_df$net_doses, probs = c(0.025, 0.5, 0.975)))

# Doses as % of total tetracyclines
pct_tetra <- (dose_q / totals$total_tetra_ddd) * 100

# Doses as % of total doxycycline
pct_doxy <- (dose_q / totals$total_doxy_ddd) * 100

# If you want to print

# pct_tetra
# pct_doxy


tab_res <- data.frame(
  Statistic = c("2.5th percentile", "Median", "97.5th percentile"),
  '% of total tetracycline' = round(pct_tetra, 3),
  '% of total doxyxycline' = round(pct_doxy, 3),
  check.names = FALSE
)

print(tab_res, row.names = FALSE)

# -----------------------------------------------------



