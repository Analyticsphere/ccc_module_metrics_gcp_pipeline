geocode <- read.csv("~/Downloads/norc_addresses_20250426_geocoded.csv")

library(dplyr)
library(gtsummary)
library(knitr)
library(kableExtra)

geocode$precision <- factor(geocode$precision, levels=c("Street", "Street Segment", "City", "ZIP", "None"))
geocode$address_nickname <- factor(geocode$address_nickname, 
                                   levels=c("home_address_01","home_address_02","home_address_03","home_address_04","home_address_05",
                                            "home_address_06","home_address_07","home_address_08","home_address_09","home_address_10",
                                            "home_address_11",
                                            "seasonal_address_01", "seasonal_address_02", "seasonal_address_03", "seasonal_address_04", "seasonal_address_05",
                                            "seasonal_address_06", "seasonal_address_07", "seasonal_address_08", "seasonal_address_09", "seasonal_address_10",
                                            "current_work_address_01", "previous_work_address_01", "school_address_01", "childhood_address_01",
                                            "user_profile_alternative_address", "user_profile_mailing_address", "user_profile_physical_address"))

geocoding__table <- geocode %>%  
  tbl_cross(
    row = address_nickname,
    col = precision,
    digits=c(0,2),
    percent = "row",
    label=list(address_nickname ~ "Address Type",
               precision ~ "Precision"),
    missing="ifany",
    margin_text="Total") 

#Ops doesn't like the grouped by variables on a different line the column labels
geocoding_df <- as.data.frame(cross__table)[-1, ]

colnames(geocoding_df)[1] <- "Address Type"

overall_surveys <- knitr::kable(geocoding_df, 
                                caption='Precision of Geocoding by Historic Address Type', 
                                row.names=FALSE,align=c("l","c","c","c","c","c", "c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(geocoding_df) - 1))  %>%
  kable_styling(latex_options = c("scale_down","hold_position"))
