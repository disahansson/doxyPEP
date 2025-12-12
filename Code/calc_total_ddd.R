# calc_total_ddd.R
# data provided by Lisselotte Diaz Hogberg (Lotta)

# Load packages
library(dplyr)

# Function to calculate total yearly DDD
calculate_total_ddd <- function(IMPUTE=TRUE) {
  
  # Tetracyclines (all codes starting with J01AA)
  tetra <- atc_dat %>%
    select(-doxy_total_ddd)%>%
    left_join(pop_data, by = "country_code") 
  
  #tetra  <-  tetra  %>% filter(!is.na(tetra$tetra_total_ddd))
  
  # Add Liechenstein to table
  tetra <- tetra %>%
    bind_rows(
      pop_data %>%
        filter(!country_code %in% tetra$country_code)
    ) 

  # impute CY, LI and SE
  tetra <- tetra %>%
    mutate(tetra_total_ddd_imp = 
             ifelse(is.na(tetra_total_ddd), 
                    population*sum(tetra$tetra_total_ddd / sum(tetra$population), na.rm = TRUE),
                    tetra_total_ddd) ) 
  
  total_tetra_ddd <- sum(tetra$tetra_total_ddd_imp)
  
  if(IMPUTE==FALSE){
    total_tetra_ddd <- sum(tetra$tetra_total_ddd, na.rm = TRUE)
  }


  
  # Doxycycline 
  doxy <- atc_dat %>%
    select(-tetra_total_ddd)%>%
    left_join(pop_data, by = "country_code") 
  

  # Add Liechenstein
  doxy <- doxy %>%
    bind_rows(
      pop_data %>%
        filter(!country_code %in% doxy$country_code)
    ) 
  
  # impute CY, LI and SE
  doxy <- doxy %>%
    mutate(doxy_total_ddd_imp = 
             ifelse(is.na(doxy_total_ddd), 
                    population*sum(doxy$doxy_total_ddd / sum(doxy$population), na.rm = TRUE),
                    doxy_total_ddd) ) 
  
  total_doxy_ddd <- sum(doxy$doxy_total_ddd_imp)
  if(IMPUTE==FALSE){
    total_doxy_ddd <- sum(doxy$doxy_total_ddd, na.rm = TRUE)
  }
  

  return(list(
    total_tetra_ddd = total_tetra_ddd,
    total_doxy_ddd = total_doxy_ddd
  ))
}
