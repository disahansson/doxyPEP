
#' Estimate DoxyPEP Eligible Population in the EU/EEA 
#'
#' This function estimates the population eligible for doxyPEP in EU/EEA countries 
#' based on EMIS2024.
#'
#'
#' @param pop_data_male A dataframe with variables: `country_code`, `country_name` ,`population`,`sexually_active_population`, and `isEU`.
#' @param PrEP_current A dataframe with variables: `Country`, `PrEP_users`, `Sample_size`, `country_name`, and `sexually_active_population`.
#'
#' @return A single numeric value: the estimated number of doxyPEP-eligible individuals.
#' @export


library(openxlsx)
library(dplyr)
library(mc2d)







EU_Pop = pop_data_male
EU_Pop = as.data.frame(EU_Pop) 
row.names(EU_Pop) = EU_Pop$country_code
countries_to_Keep = EU_Pop$country_code
names(EU_Pop)[names(EU_Pop) == 'country_code'] <- 'Country'
EU_Pop = EU_Pop %>% select(Country, country_name, sexually_active_population)


## Should be 28 matching countries. 
## Iceland and Liechtenstein missing from the 30EU/EEA countries
sum(PrEP_current$Country %in% countries_to_Keep)
countries_to_Keep[!countries_to_Keep%in%PrEP_current$Country ]

PrEP_current =  PrEP_current %>% filter(Country %in% countries_to_Keep)
row.names(PrEP_current) = PrEP_current$Country

PrEP_current = merge(PrEP_current, EU_Pop, by = 'row.names')

PrEP_current = PrEP_current %>% select(-c(Row.names,Country.x))
names(PrEP_current)[names(PrEP_current) == 'Country.y'] <- 'Country'



################

# 
eligible_population_EMIS_func2024 <- function(IMPUTE=TRUE){
  
  if(IMPUTE){
    ################### if impute!!!
    # Add missing countries Liechenstein and Iceland
    PrEP_current <- PrEP_current %>%
      bind_rows(
        EU_Pop %>%
          filter(!country_name %in% PrEP_current$country_name)
      ) 
    rownames(PrEP_current)=NULL
    
  }
  
  MSM_prop = runif(nrow(PrEP_current), min = 0.04, max = 5.6)
  
  PrEP_current_dummy = PrEP_current %>% mutate(N_MSM_mean = ceiling(sexually_active_population*MSM_prop/100))
  
  
  # if(IMPUTE){
  PrEP_current_dummy = PrEP_current_dummy %>% mutate(Fraction_on_PrEP =
                                                       ifelse(is.na(PrEP_users), 
                                                              mean((PrEP_users/Sample_size), na.rm = TRUE),
                                                              (PrEP_users/Sample_size)) )
  # }else{
  #   PrEP_current_dummy = PrEP_current_dummy %>% mutate(Fraction_on_PrEP = (PrEP_users/Sample_size))
  # }
  #   
  
  PrEP_current_dummy = PrEP_current_dummy %>% mutate(N_on_PrEP = Fraction_on_PrEP * N_MSM_mean)
  
  
  
  res_PrEP <- ceiling(sum(PrEP_current_dummy$N_on_PrEP))
  return(res_PrEP)
}

