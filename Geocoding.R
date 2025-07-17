
library(bigrquery)
#install_tinytex()
library(tinytex)
library(data.table) ###to write or read and data management 
library(tidyverse) ###for data management
library(reshape)  ###to work on transition from long to wide or wide to long data
library(listr) ###to work on a list of vector, files or..
library(sqldf) ##sql
library(lubridate) ###date time
library(stringr) ###to work on patterns, charaters
library(arsenal)
library(gt)
library(gtsummary)
library(knitr)
library(kableExtra)

options(tinytex.verbose = TRUE)

bq_auth()


geocode <- read.csv("~/Downloads/norc_addresses_20250426_geocoded.csv")

geocode1 <- geocode %>% select(Connect_ID, precision, address_nickname)

geocode1$precision <- factor(geocode1$precision, levels=c("Street", "Street Segment", "City", "ZIP", "None"))
geocode1$address_nickname <- factor(geocode1$address_nickname, 
                                   levels=c("home_address_01","home_address_02","home_address_03","home_address_04","home_address_05",
                                            "home_address_06","home_address_07","home_address_08","home_address_09","home_address_10",
                                            "home_address_11",
                                            "seasonal_address_01", "seasonal_address_02", "seasonal_address_03", "seasonal_address_04", "seasonal_address_05",
                                            "seasonal_address_06", "seasonal_address_07", "seasonal_address_08", "seasonal_address_09", "seasonal_address_10",
                                            "current_work_address_01", "previous_work_address_01", "school_address_01", "childhood_address_01",
                                            "user_profile_alternative_address", "user_profile_mailing_address", "user_profile_physical_address"))



parts <- "SELECT Connect_ID, d_544150384
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` 
where Connect_ID IS NOT NULL and (d_512820379='486306141' OR d_512820379='854703046') and 
d_919254129='353358909' and d_699625233='353358909' and d_663265240='231311385' and d_821247024='197316935'"

parts_table <- bq_project_query(project, parts)
parts_data <- bq_table_download(parts_table, bigint = "integer64")
parts_data$Connect_ID <- as.numeric(parts_data$Connect_ID)

querymod4 <- "
SELECT  Connect_ID,
d_590222838 as Movein1_m4,
d_728155643_d_943813942  as MoveOut1_m4,
case when d_728155643_d_728155643_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out1_m4,
d_623452399 as Movein2_m4,
d_380621340_d_943813942  as MoveOut2_m4,
case when d_380621340_d_380621340_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out2_m4,
d_876443676  as Movein3_m4,
d_999694720_d_943813942  as MoveOut3_m4,
case when d_999694720_d_999694720_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out3_m4,
d_270587543  as Movein4_m4,
d_849066624_d_943813942  as MoveOut4_m4,
case when d_849066624_d_849066624_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out4_m4,
d_195167143  as Movein5_m4,
d_425315696_d_943813942  as MoveOut5_m4,
case when d_425315696_d_425315696_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out5_m4,
d_495330716  as Movein6_m4,
d_529608184_d_943813942  as MoveOut6_m4,
case when d_529608184_d_529608184_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out6_m4,
d_783797179  as Movein7_m4,
d_234731641_d_943813942  as MoveOut7_m4,
case when d_234731641_d_234731641_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out7_m4,
d_243695994  as Movein8_m4,
d_664561394_d_943813942  as MoveOut8_m4,
case when d_664561394_d_664561394_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out8_m4,
d_562190570  as Movein9_m4,
d_450154625_d_943813942  as MoveOut9_m4,
case when d_450154625_d_450154625_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out9_m4,
d_790359238  as Movein10_m4,
d_812161236_d_943813942  as MoveOut10_m4,
case when d_812161236_d_812161236_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out10_m4,
d_925747671  as Movein11_m4,
d_964437163_d_943813942  as MoveOut11_m4,
case when d_964437163_d_964437163_d_678602069='1' THEN '2025' ELSE '' END as Never_Moved_Out11_m4,
D_379754686 as SH_FirstYear1_m4,
D_975198123_D_314198277  as SH_LastYear1_m4,
case when D_975198123_D_975198123_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering1_m4,
D_963795928 as SH_FirstYear2_m4,
D_778495585_D_314198277  as SH_LastYear2_m4,
case when D_778495585_D_778495585_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering2_m4,
D_775038507 as SH_FirstYear3_m4,
D_829751791_D_314198277  as SH_LastYear3_m4,
case when D_829751791_D_829751791_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering3_m4,
D_778721228 as SH_FirstYear4_m4,
D_914325712_D_314198277  as SH_LastYear4_m4,
case when D_914325712_D_914325712_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering4_m4,
D_504158362 as SH_FirstYear5_m4,
D_682457230_D_314198277  as SH_LastYear5_m4,
case when D_682457230_D_682457230_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering5_m4,
D_696972918 as SH_FirstYear6_m4,
D_404118353_D_314198277  as SH_LastYear6_m4,
case when D_404118353_D_404118353_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering6_m4,
D_921090200 as SH_FirstYear7_m4,
D_806530984_D_314198277  as SH_LastYear7_m4,
case when D_806530984_D_806530984_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering7_m4,
D_134624195 as SH_FirstYear8_m4,
D_613251957_D_314198277  as SH_LastYear8_m4,
case when D_613251957_D_613251957_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering8_m4,
D_274396191 as SH_FirstYear9_m4,
D_437395872_D_314198277  as SH_LastYear9_m4,
case when D_437395872_D_437395872_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering9_m4,
D_940513931 as SH_FirstYear10_m4,
D_288109261_D_314198277  as SH_LastYear10_m4,
case when D_288109261_D_288109261_D_196962759='1' THEN '2025' ELSE '' END as Still_Summering10_m4,
D_870675371_D_594558885 as CURWORK_Start_m4,
2025 as CURWORK_End_m4,
D_569375463 as PreWork_Start_m4,
D_822005593 as PreWorkEnd_m4,
D_224209278_D_770959058 as SchoolStart_m4,
2025 as SchoolEnd_m4,
D_191221569_D_948416598 as ChildhoodStart_m4,
case when D_191221569_D_191221569_D_858353109='1' THEN 'DOB' ELSE ' ' end as ChildhoodBirth_m4,
D_752015272_D_943813942 as ChildhoodEnd_m4,
case when D_752015272_D_752015272_D_139208251='1' THEN '2025' ELSE '' end as Living_Childhood_m4

FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module4_v1`
where Connect_ID is not null
"

mod4_table <- bq_project_query(project, querymod4)
mod4_data <- bq_table_download(mod4_table, bigint = "integer64")
mod4_data$Connect_ID <- as.numeric(mod4_data$Connect_ID)

module_4 = left_join(parts_data, mod4_data, by="Connect_ID") 
module4 = left_join(geocode1, module_4, by="Connect_ID")
dim(module4)



## Cleaning up typos 
module4$PreWork_Start_m4[module4$PreWork_Start_m4=="02006"]="2006"

#Removing odd values that aren't actual years
module4$ChildhoodBirth_m4[which(module4$ChildhoodBirth_m4 == "DOB")] <- module4$d_544150384[which(module4$ChildhoodBirth_m4 == "DOB")]

module4 <- module4 %>%
  mutate(across(
    ends_with("_m4"),
    ~ ifelse(as.numeric(.) < 1900, NA, .)
  )) 




# module4 <- module4 %>%  
#   mutate(time_living = case_when((address_nickname=="home_address_01" & (2025-as.numeric(Movein1_m4)) <11) |
#                                    (address_nickname=="home_address_02" & (2025-as.numeric(Movein1_m4)) <11) | 
#                                    (address_nickname=="home_address_03" & (2025-as.numeric(Movein2_m4)) <11) | 
#                                    (address_nickname=="home_address_04" & (2025-as.numeric(Movein3_m4)) <11) |
#                                    (address_nickname=="home_address_05" & (2025-as.numeric(Movein4_m4)) <11) |
#                                    (address_nickname=="home_address_06" & (2025-as.numeric(Movein5_m4)) <11) |
#                                    (address_nickname=="home_address_07" & (2025-as.numeric(Movein6_m4)) <11) |
#                                    (address_nickname=="home_address_08" & (2025-as.numeric(Movein7_m4)) <11) |
#                                    (address_nickname=="home_address_09" & (2025-as.numeric(Movein8_m4)) <11) |
#                                    (address_nickname=="home_address_10" & (2025-as.numeric(Movein9_m4)) <11) |
#                                    (address_nickname=="home_address_11" & (2025-as.numeric(Movein10_m4)) <11) |
#                                    (address_nickname=="seasonal_address_01" & (2025-as.numeric(SH_FirstYear1_m4)) <11) |
#                                    (address_nickname=="seasonal_address_02" & (2025-as.numeric(SH_FirstYear2_m4)) <11) | 
#                                    (address_nickname=="seasonal_address_03" & (2025-as.numeric(SH_FirstYear3_m4)) <11) |
#                                    (address_nickname=="seasonal_address_04" & (2025-as.numeric(SH_FirstYear4_m4)) <11) |
#                                    (address_nickname=="seasonal_address_05" & (2025-as.numeric(SH_FirstYear5_m4)) <11) |
#                                    (address_nickname=="seasonal_address_06" & (2025-as.numeric(SH_FirstYear6_m4)) <11) | 
#                                    (address_nickname=="seasonal_address_07" & (2025-as.numeric(SH_FirstYear7_m4)) <11) |
#                                    (address_nickname=="seasonal_address_08" & (2025-as.numeric(SH_FirstYear8_m4)) <11) |
#                                    (address_nickname=="seasonal_address_09" & (2025-as.numeric(SH_FirstYear9_m4)) <11) |
#                                    (address_nickname=="seasonal_address_10" & (2025-as.numeric(SH_FirstYear10_m4)) <11) |
#                                    (address_nickname=="current_work_address_01" & (2025-as.numeric(CURWORK_Start_m4)) <11) |
#                                    (address_nickname=="previous_work_address_01" & (2025-as.numeric(PreWork_Start_m4)) <11) |
#                                    (address_nickname=="school_address_01" & (2025-as.numeric(SchoolStart_m4)) <11) |
#                                    (address_nickname=="childhood_address_01" & (2025-as.numeric(coalesce(ChildhoodStart_m4, ChildhoodBirth_m4))) <11)  ~ "0-10 Years Ago"))
# 
# 
# 
# module4$time_living <- factor(module4$time_living, levels=c("0-10 Years Ago", "11-20 Years Ago", "21-30 Years Ago", "31-40 Years Ago", 
#                                                               "41-50 Years Ago", "51-60 Years Ago", "61-70 Years Ago", "71-80 Years Ago"))

module4 <- module4 %>%
  mutate(
    movein_year = case_when(
      address_nickname == "home_address_01" ~ as.numeric(Movein1_m4),
      address_nickname == "home_address_02" ~ as.numeric(Movein2_m4),
      address_nickname == "home_address_03" ~ as.numeric(Movein3_m4),
      address_nickname == "home_address_04" ~ as.numeric(Movein4_m4),
      address_nickname == "home_address_05" ~ as.numeric(Movein5_m4),
      address_nickname == "home_address_06" ~ as.numeric(Movein6_m4),
      address_nickname == "home_address_07" ~ as.numeric(Movein7_m4),
      address_nickname == "home_address_08" ~ as.numeric(Movein8_m4),
      address_nickname == "home_address_09" ~ as.numeric(Movein9_m4),
      address_nickname == "home_address_10" ~ as.numeric(Movein10_m4),
      address_nickname == "home_address_11" ~ as.numeric(Movein11_m4),
      address_nickname == "seasonal_address_01" ~ as.numeric(SH_FirstYear1_m4),
      address_nickname == "seasonal_address_02" ~ as.numeric(SH_FirstYear2_m4),
      address_nickname == "seasonal_address_03" ~ as.numeric(SH_FirstYear3_m4),
      address_nickname == "seasonal_address_04" ~ as.numeric(SH_FirstYear4_m4),
      address_nickname == "seasonal_address_05" ~ as.numeric(SH_FirstYear5_m4),
      address_nickname == "seasonal_address_06" ~ as.numeric(SH_FirstYear6_m4),
      address_nickname == "seasonal_address_07" ~ as.numeric(SH_FirstYear7_m4),
      address_nickname == "seasonal_address_08" ~ as.numeric(SH_FirstYear8_m4),
      address_nickname == "seasonal_address_09" ~ as.numeric(SH_FirstYear9_m4),
      address_nickname == "seasonal_address_10" ~ as.numeric(SH_FirstYear10_m4),
      address_nickname == "current_work_address_01" ~ as.numeric(CURWORK_Start_m4),
      address_nickname == "previous_work_address_01" ~ as.numeric(PreWork_Start_m4),
      address_nickname == "school_address_01" ~ as.numeric(SchoolStart_m4),
      address_nickname == "childhood_address_01" ~ as.numeric(coalesce(ChildhoodStart_m4, ChildhoodBirth_m4)),
      TRUE ~ NA_real_
    ),
    years_ago = 2025 - movein_year,
    time_living = case_when(
      is.na(years_ago) ~ NA_character_,
      years_ago < 11 ~ "0-10 Years Ago",
      years_ago < 21 ~ "11-20 Years Ago",
      years_ago < 31 ~ "21-30 Years Ago",
      years_ago < 41 ~ "31-40 Years Ago",
      years_ago < 51 ~ "41-50 Years Ago",
      years_ago < 61 ~ "51-60 Years Ago",
      years_ago < 71 ~ "61-70 Years Ago",
      years_ago < 81 ~ "71-80 Years Ago",
      TRUE ~ NA_character_
    ),
    time_living = factor(
      time_living,
      levels = c("0-10 Years Ago", "11-20 Years Ago", "21-30 Years Ago", "31-40 Years Ago", 
                 "41-50 Years Ago", "51-60 Years Ago", "61-70 Years Ago", "71-80 Years Ago")
    )
  )



geocoding__table <- module4 %>%  
  filter(time_living!="Unknown") %>% 
  tbl_cross(
    row = time_living,
    col = precision,
    digits=c(0,2),
    percent = "row",
    label=list(time_living ~ "Address Timeframe",
               precision ~ "Precision"),
    missing="ifany",
    margin_text="Total") 

geocoding_df <- as.data.frame(geocoding__table)[-1, ]

colnames(geocoding_df)[1] <- "Address Timeframe"

overall_surveys <- knitr::kable(geocoding_df, 
                                caption='Precision of Geocoding by Year Participant Starting Living or Frequenting an Address', 
                                row.names=FALSE,align=c("l","c","c","c","c","c", "c"), booktabs = TRUE) %>%  
  add_indent(seq(1, nrow(geocoding_df) - 1))  %>%
  kable_styling(latex_options = c("scale_down","hold_position")) %>% 
  add_footnote("Note: Addresses without a provided start year were not inlcuded in this table. User profile addresses are not included in this table, as those addresses have no start date. Addresses included in this analysis are home addresses, summer home addresses, current work addresses, previous work addresses, current school addresses, and childhood addresses.", 
               notation="none", threeparttable = TRUE) 

