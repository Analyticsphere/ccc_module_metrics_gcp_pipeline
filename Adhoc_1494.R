

library(bigrquery)
library(dplyr)
library(gmodels)
library(epiDisplay)
library(lubridate)
library(tidyverse)
library(knitr)
library(tinytex)
library(kableExtra)
library(gtsummary)
library(data.table)

library(stringr) 
library(tidyr)
library(gt)

bq_auth()



project = "nih-nci-dceg-connect-prod-6d04"
pt_pull <- "SELECT  pts.Connect_ID,
case 
when d_831041022='353358909' then 'Yes'
when d_831041022='104430631' then 'No'
else 'Null'
end as Data_Destruction,

case 
when d_747006172='353358909' then 'Yes'
when d_747006172='104430631' then 'No'
else 'Null'
end as Withdrawn,

case 
when d_525972260='353358909' then 'Yes'
else 'No'
end as RCA_Occurrence,


FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` pts
left join  `nih-nci-dceg-connect-prod-6d04.FlatConnect.cancerOccurrence`  rca
on pts.Connect_ID = rca.Connect_ID
where pts.Connect_ID is not null and d_821247024='197316935'" 


pt_pull_table <- bq_project_query(project, pt_pull)

adhoc1494 <- bq_table_download(pt_pull_table, bigint = "integer64")




######### Withdrawals and Data Destruction Among RCA Occurrence 
## Everyone who has data destruction has withdrawn

WD_DD_RCA <- adhoc1494 %>%  
  mutate(Data_Destruction = case_when(Data_Destruction=="Yes" ~ "Data Destruction Requested",
                                      Withdrawn=="Yes" ~ "Withdrawn, without Data Destruction",
                                      TRUE ~ "Neither"),
         Data_Destruction = factor(Data_Destruction, levels=c("Withdrawn, without Data Destruction",
                                                              "Data Destruction Requested","Neither"))) %>% 
  tbl_cross(
    row = RCA_Occurrence,
    col = Data_Destruction,
    label = list(Data_Destruction ~ " ",RCA_Occurrence~"Cancer Occurrence"),
    percent = "row",
    digits=c(0,2),
    margin_text="Total Verified Participants") 


WD_DD_RCA <- WD_DD_RCA %>%
  modify_caption("Data Destruction and Withdrawals Among Cancer Occurrence") %>%
  as_kable_extra(escape = FALSE, addtl_fmt = TRUE)

WD_DD_RCA %>% kable_styling(latex_options = c("scale_down","hold_position")) 









