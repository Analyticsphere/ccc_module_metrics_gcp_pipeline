############# Note: Participants with errors whose data we will not correct will be excluded from the rule. Github contains records of these exclusions as well.

library(DBI) 
library(bigrquery)
library(lubridate)
library(dplyr)
library(stringr)
library(gt)
library(expss)
library(knitr)
library(kableExtra)
library(glue)
library(openxlsx)

options(tinytex.verbose = TRUE)

bq_auth()

project <- "nih-nci-dceg-connect-prod-6d04"

currentDate <- Sys.Date()

#### Set up DBI connection for large participants table BQ pull
dataset <- "FlatConnect"

# Establish a connection to BigQuery
con <- dbConnect(
  bigrquery::bigquery(),
  project = project,
  dataset = dataset,
  billing = project  # Use project for billing
)

# Verify the connection by listing available tables
dbListTables(con)

## Because we're not restricting by Connect_ID is not null, we have millions of rows.
## Easier and quicker to import them by chunks of columns

chunk1 <- tbl(con, "participants", page_size = 1000) %>%
  filter(d_831041022=='104430631') %>%
  dplyr::select(Connect_ID, token, d_512820379, d_471593703, state_d_934298480, d_230663853, state_d_697256759, state_d_158291096,
                d_335767902, d_982402227, d_919254129, d_699625233, d_827220437, d_371067537, d_544150384, d_564964481, d_795827569, d_685002411_d_994064239, d_912301837, 
                d_685002411_d_194410742,d_685002411_d_949501163,d_685002411_d_277479354,d_685002411_d_867203506,d_685002411_d_352996056,d_685002411_d_217367618,d_747006172,
                d_906417725,d_773707518,d_831041022,d_987563196, d_659990606, d_100767870, state_studyId, state_d_521025370) %>%
  as_tibble() %>% 
  distinct(token, .keep_all = TRUE) #somehow pulling duplicsate rows
chunk1$Connect_ID <- as.numeric(chunk1$Connect_ID)

chunk2 <- tbl(con, "participants", page_size = 1000) %>%
  filter(d_831041022=='104430631') %>%
  dplyr::select(Connect_ID, token,  d_430551721, d_821247024, d_914594314,  state_d_725929722, d_126331570, d_536735468, d_130371375_d_266600170_d_945795905, d_130371375_d_266600170_d_320023644, state_d_538553381, state_d_527823810, state_d_849518448, state_d_119643471, state_d_253532712, state_d_684926335, 
                d_663265240, d_878865966, d_684635302, d_167958071, d_130371375_d_266600170_d_731498909, d_130371375_d_266600170_d_222373868, d_130371375_d_266600170_d_787567527,
                d_949302066 , d_517311251, d_205553981, d_117249500, d_976570371, d_914639140, d_311580100, d_454445267,d_255077064, d_832139544, 
                d_263355177, d_199471989, d_222161762, d_822499427, d_764863765, d_534669573, d_195145666, d_286191859, d_541836531, d_770257102, d_386488297, d_264644252, d_452942800, state_d_706256705, state_d_435027713, state_d_477091792, state_d_667474224, state_d_749475364) %>%
  as_tibble() %>% 
  distinct(token, .keep_all = TRUE) #somehow pulling duplicsate rows
chunk2$Connect_ID <- as.numeric(chunk2$Connect_ID)

chunk3 <- tbl(con, "participants", page_size = 1000) %>%
  filter(d_831041022=='104430631') %>%
  dplyr::select(Connect_ID, token, d_217640691, d_844088537, d_784810139, d_268176409, d_843688458, d_870643066, d_315032037, d_943232079, d_692560814, 
                d_956490759,d_265193023,d_253883960,d_547363263,d_459098666,d_220186468,d_320303124, d_609630315, d_389890053, d_176068627, d_439351436, d_280021666,
                d_610227793, d_109610692, d_148184166, d_808755658, d_196723965, d_501613780, d_130371375_d_266600170_d_648936790,
                state_d_444699761, state_d_188797763, state_d_953614051, state_d_793822265, state_d_147176963, state_d_557461333,  state_d_711794630, 
                state_d_679832994, state_d_559534463, state_d_570452130, state_d_629484663, state_d_547895941,
                d_685002411_d_936015433, d_685002411_d_688142378, d_685002411_d_101763809, d_685002411_d_525277409, d_685002411_d_671903816,
                d_761057722, d_207613315, d_163847117, state_d_956485028, state_d_148197146) %>%
  as_tibble() %>% 
  distinct(token, .keep_all = TRUE) #somehow pulling duplicate rows
chunk3$Connect_ID <- as.numeric(chunk3$Connect_ID)

parts_bq_partial <- full_join(chunk1, chunk2, by=c("token", "Connect_ID"))
partsbq <- full_join(parts_bq_partial, chunk3, by=c("token", "Connect_ID"))

biobq <- "SELECT Connect_ID, token, d_299553921_d_593843561, d_299553921_d_883732523, d_703954371_d_593843561, d_703954371_d_883732523, d_838567176_d_593843561, d_838567176_d_883732523, d_454453939_d_593843561, d_454453939_d_883732523, d_652357376_d_593843561, d_652357376_d_883732523, d_505347689_d_593843561, d_505347689_d_883732523 FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen` 
where Connect_ID IS NOT NULL"
biobq_table <- bq_project_query(project, biobq)
biobq <- bq_table_download(biobq_table, bigint = "integer64")

partsbq$Connect_ID <- as.numeric(partsbq$Connect_ID)
biobq$Connect_ID <- as.numeric(biobq$Connect_ID)

partsbq <-
  partsbq %>%  mutate(
    Site = case_when(
      d_827220437 == 125001209 ~ "KPCO",
      d_827220437 == 472940358 ~ "BSWH",
      d_827220437 == 300267574 ~ "KPHI",
      d_827220437 == 303349821 ~ "MF",
      d_827220437 == 327912200 ~ "KPGA",
      d_827220437 == 452412599 ~ "KPNW",
      d_827220437 == 531629870 ~ "HP",
      d_827220437 == 548392715 ~ "HFH",
      d_827220437 == 657167265 ~ "SF",
      d_827220437 == 809703864 ~ "UC"
    )
  )

base_vars= left_join(partsbq, biobq, by=c("Connect_ID", "token"))

#################

## Sorting by Site causes an error message when there are no rows. This function eliminates that problem
safe_arrange <- function(df, ...) {
  if (nrow(df) > 0) {
    df %>% arrange(...)
  } else {
    df
  }
}

# Initialize dataframe for all errors
all_errors <- data.frame(Connect_ID = numeric(), token = character(), Site = character(), rule_id = integer(), rule_label = character(), stringsAsFactors = FALSE)



## ---------------  RULES   -------------------------------------------------------------------



# Rule 1
incentive2 <- base_vars %>%  filter(d_949302066 == '231311385' & d_536735468 == '231311385' & d_976570371 == '231311385' & d_663265240 == '231311385' & 
                                      ((d_299553921_d_593843561==353358909 & d_299553921_d_883732523 != 681745422) |
                                         (d_703954371_d_593843561==353358909 & d_703954371_d_883732523 != 681745422) |
                                         (d_838567176_d_593843561==353358909 & d_838567176_d_883732523 != 681745422) |
                                         (d_454453939_d_593843561==353358909 & d_454453939_d_883732523 != 681745422) |
                                         (d_652357376_d_593843561==353358909 & d_652357376_d_883732523 != 681745422) |
                                         (d_505347689_d_593843561==353358909 & d_505347689_d_883732523 != 681745422)) &
                                      (d_130371375_d_266600170_d_731498909!=353358909 | d_130371375_d_266600170_d_222373868!=353358909 | is.na(d_130371375_d_266600170_d_787567527)))

if (nrow(incentive2) > 0) {
  temp <- incentive2 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 1,
      rule_label = "If all BL Modules are completed and the participant has a baseline research collection where the tube is collected and the reason tube wasn't collected wasn't refusal, then SMPaym_TmPaymEligBL_v1r0 must be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 2
mens_ccc <- "WITH combined_survey AS (
  SELECT Connect_ID
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.bioSurvey_v1`
  WHERE d_112151599 = '353358909'

  UNION DISTINCT

  SELECT Connect_ID
  FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.clinicalBioSurvey_v1`
  WHERE d_112151599 = '353358909'
)

SELECT p.Connect_ID, p.token, p.d_827220437, p.d_459098666, p.d_253883960, p.d_265193023, p.d_289750687, 
p.d_764863765, p.d_222161762, p.d_459098666
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` p
JOIN combined_survey c ON p.Connect_ID = c.Connect_ID
WHERE (p.d_253883960 = '231311385' OR p.d_265193023 = '231311385')"

mens_table <- bq_project_query(project, mens_ccc)
mens <- bq_table_download(mens_table, bigint = "integer64")
mens$Connect_ID <- as.numeric(mens$Connect_ID)

mens <-
  mens %>%  mutate(
    Site = case_when(
      d_827220437 == 125001209 ~ "Kaiser Permanente Colorado",
      d_827220437 == 472940358 ~ "Baylor Scott & White Health",
      d_827220437 == 300267574 ~ "Kaiser Permanente Hawaii",
      d_827220437 == 303349821 ~ "Marshfield Clinic Health System",
      d_827220437 == 327912200 ~ "Kaiser Permanente Georgia",
      d_827220437 == 452412599 ~ "Kaiser Permanente Northwest",
      d_827220437 == 517700004 ~ "National Cancer Institute",
      d_827220437 == 531629870 ~ "HealthPartners",
      d_827220437 == 548392715 ~ "Henry Ford Health System",
      d_827220437 == 657167265 ~ "Sanford Health",
      d_827220437 == 809703864 ~ "University of Chicago Medicine"
    )
  )

can_no_longer_contact <- list("6673328645", "1156167603", "5950274173", "1674063031", "5460895468", "9586303599", 
                              "2009345083", "8140966374", "4038472238", "3379215871", "6592021474", "1721018129", 
                              "3023733685", "5085319740", "8682077118", "1441310478", "4177949257", "6384048173", 
                              "1186358664", "2510716583", "1782585066")

mens_elg <- mens %>%  filter(is.na(d_289750687) & (d_764863765>=as.Date("2022-12-09") | d_222161762>=as.Date("2022-12-09")) & 
                               !(Connect_ID %in% can_no_longer_contact))

if (nrow(mens_elg) > 0) {
  temp <- mens_elg %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 2,
      rule_label = "If either the BUM Survey or the BU Survey was completed with SrvBlU_MENST60_v2r0 answered yes, then SrvMC_MenstSrvElig_v1r0 must be yes."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 3
MC_ccc_NE <- "SELECT CASE
  WHEN d_827220437 = '125001209'  THEN 'Kaiser Permanente Colorado'
  WHEN  d_827220437 = '472940358'  THEN 'Baylor Scott & White Health'
  WHEN d_827220437 = '300267574'  THEN 'Kaiser Permanente Hawaii'
  WHEN d_827220437 = '303349821'  THEN 'Marshfield Clinic Health System'
  WHEN   d_827220437 = '327912200'  THEN 'Kaiser Permanente Georgia'
  WHEN d_827220437 = '452412599'  THEN 'Kaiser Permanente Northwest'
  WHEN d_827220437 = '517700004'  THEN 'National Cancer Institute'
  WHEN d_827220437 = '531629870'  THEN 'HealthPartners'
  WHEN d_827220437 = '548392715'  THEN 'Henry Ford Health System'
  WHEN d_827220437 = '657167265'  THEN 'Sanford Health'
  WHEN d_827220437 = '809703864'  THEN 'University of Chicago Medicine'
END AS Site,

Connect_ID, token,
CASE 
    WHEN d_289750687 is null THEN 'No'
    WHEN d_289750687 = '353358909' THEN 'Yes'
  END AS Derived_Eligibility_Flag, 
CASE 
    WHEN d_459098666 = '231311385' THEN 'Completed'
    WHEN d_459098666 = '615768760' THEN 'Started'
  END AS Completion_Status,
d_844088537 as Time_Survey_Started  
 FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` 
where d_289750687 is null and (d_459098666='231311385' or d_459098666='615768760')
and date(d_844088537) >= '2023-01-01'
order by d_844088537 desc"

MC_ccc_NE_table <- bq_project_query(project, MC_ccc_NE)
MC_not_elg <- bq_table_download(MC_ccc_NE_table, bigint = "integer64")
MC_not_elg$Connect_ID <- as.numeric(MC_not_elg$Connect_ID)

if (nrow(MC_not_elg) > 0) {
  temp <- MC_not_elg %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 3,
      rule_label = "If SrvMC_MenstSrvElig_v1r0, then SrvMC_BaseComplete_v1r0 cannot be 'Started' or 'Submitted'. Note, this excludes those who started the survey prior to December 10th, 2022 as the eligibility has since been corrected."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 4
cash_NORC <- base_vars %>%  filter(d_130371375_d_266600170_d_731498909==353358909 & d_130371375_d_266600170_d_945795905=="cash" & !is.na(d_130371375_d_266600170_d_320023644))

if (nrow(cash_NORC) > 0) {
  temp <- cash_NORC %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 4,
      rule_label = "If HDPaym_PaymChosenBL_v2r0 = \"cash\" and SMPaym_PaymEligBL_v1r0=yes, then HDPaym_CaseNumberBL_v1r0 should not be populated. Or conversely, if SMPaym_PaymEligBL_v1r0=yes\"and HDPaym_CaseNumberBL_v1r0 is populated, then HDPaym_PaymChosenBL_v2r0 should not be \"cash\"."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 5
bday <- partsbq %>%  filter(!is.na(d_371067537) & 
                              d_371067537 != paste0(d_544150384, d_564964481, d_795827569))

if (nrow(bday) > 0) {
  temp <- bday %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 5,
      rule_label = "RcrtUP_DOB must equal the concatenation of RcrtUP_YOB, RcrtUP_MOB, and RcrtUP_BD. This excludes those with a null concatenated DOB as it was implements after Module 1 was in production."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 6
smmet0 <- partsbq %>%  filter(d_685002411_d_994064239==104430631 & d_685002411_d_194410742==104430631 & d_685002411_d_949501163==104430631 & 
                                d_685002411_d_277479354==104430631 & d_685002411_d_867203506==104430631 & d_685002411_d_352996056==104430631 & 
                                d_685002411_d_217367618==104430631 & d_747006172==104430631 & d_906417725==104430631 & d_773707518==104430631 & 
                                d_831041022==104430631 & d_987563196==104430631 & d_685002411_d_936015433==104430631 & d_685002411_d_688142378==104430631 & 
                                d_685002411_d_101763809==104430631 & d_685002411_d_525277409==104430631 & d_685002411_d_671903816==104430631 &
                                d_912301837!=208325815)

if (nrow(smmet0) > 0) {
  temp <- smmet0 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 6,
      rule_label = "If HdRef_Basesrv_v1r0 = \"no\" AND HdRef_Baseblood_v1r0 = \"no\" AND HdRef_Baseurine_v1r0 and HdRef_Basesaliva_v1r0 = \"no\" AND HdRef_Allsrv_v1r0 = \"no\" AND HdRef_Allsample_v1r0 = \"no\" AND HdRef_BlSpecSrv_v1r0 = \"no\" AND HdWd_WdConsent_v1r0 = \"no\" AND HdWd_Activepart_v1r0 = \"no\" AND HdWd_HIPAArevoked_v1r0 = \"no\" AND HdWd_Destroydata_v1r0 = \"no\" AND HdWd_Deceased_v1r0 = \"no\" AND HdRef_3moQOLsurv_v1r0 = 'no' AND HdRef_AllQOLsurv_v1r0='no' and HdRef_2024ConExpSrv_v1r0='no' AND HdRef_AllFutConExpSrv_v1r0='no' nad HdRef_CancHistScrnSrv_v1r0='no', then SMMet_PartStatus_v1r0= \"No Refusal\""
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 7
smmet2 <- partsbq %>%  filter( d_906417725==353358909 & d_773707518==104430631 & d_747006172==104430631 & d_831041022==104430631 & d_987563196==104430631 & d_912301837!=458508122)

if (nrow(smmet2) > 0) {
  temp <- smmet2 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 7,
      rule_label = "If HdWd_Activepart_v1r0 = yes\" AND HdWd_HIPAArevoked_v1r0 = \"no\" And HdWd_WdConsent_v1r0 = \"no\" AND HdWd_Deceased_v1r0 = \"no\" AND HdWd_Destroydata_v1r0 = \"no\", then SMMet_PartStatus_v1r0= \"Refused All Future Activities\""
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 8
smmet3 <- partsbq %>%  filter(d_773707518==353358909 & d_747006172==104430631 & d_831041022==104430631 & d_987563196==104430631 & d_912301837!=872012139)

if (nrow(smmet3) > 0) {
  temp <- smmet3 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 8,
      rule_label = "If HdWd_HIPAArevoked_v1r0 = yes\" And HdWd_WdConsent_v1r0 = \"no\" And HdWd_Deceased_v1r0 = \"no\" AND HdWd_Destroydata_v1r0 = \"no\", then SMMet_PartStatus_v1r0= \"Revoked HIPAA Only\""
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 9
smmet4 <- partsbq %>%  filter(d_773707518==353358909 &  d_747006172==353358909 & d_831041022==104430631 & d_987563196==104430631 & d_912301837!=854021266)

if (nrow(smmet4) > 0) {
  temp <- smmet4 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 9,
      rule_label = "If HdWd_HIPAArevoked_v1r0 = yes\" AND HdWd_WdConsent_v1r0 = \"yes\" AND HdWd_Deceased_v1r0 = \"no\" AND HdWd_Destroydata_v1r0 = \"no\", then SMMet_PartStatus_v1r0= \"Withdrew Consent\""
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 10
opt_out <- partsbq %>%  filter(!is.na(state_d_697256759) & (state_d_158291096==104430631 | is.na(state_d_158291096)))

if (nrow(opt_out) > 0) {
  temp <- opt_out %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 10,
      rule_label = "If RcrtSI_OptOutTm_v1r0 is a valid date, then RcrtSI_OptOut_v1r0 must be \"yes\""
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 11
pref_lang <- partsbq %>%  filter(is.na(d_255077064) & as.Date(d_454445267) > "2024-07-31")

if (nrow(pref_lang) > 0) {
  temp <- pref_lang %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 11,
      rule_label = "If RcrtCS_ConsentSumit_v1r0 was on or after August 1, 2024 then RcrtCS_PrefLang_v1r0 must be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 12
comp_m1 <- partsbq %>%  filter(!is.na(d_517311251) & (d_949302066!=231311385) & 
                                 Connect_ID!='1974006545')

if (nrow(comp_m1) > 0) {
  temp <- comp_m1 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 12,
      rule_label = "If the Mod1 survey completion timestamp is populated, then the Mod1 survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 13
comp_m2 <- partsbq %>%  filter(!is.na(d_832139544) & (d_536735468!=231311385))

if (nrow(comp_m2) > 0) {
  temp <- comp_m2 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 13,
      rule_label = "If the Mod2 survey completion timestamp is populated, then the Mod2 survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 14
comp_m3 <- partsbq %>%  filter(!is.na(d_770257102) & (d_976570371!=231311385))

if (nrow(comp_m3) > 0) {
  temp <- comp_m3 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 14,
      rule_label = "If the Mod3 survey completion timestamp is populated, then the Mod3 survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 15
comp_m4 <- partsbq %>%  filter(!is.na(d_264644252) & (d_663265240!=231311385))

if (nrow(comp_m4) > 0) {
  temp <- comp_m4 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 15,
      rule_label = "If the Mod4 survey completion timestamp is populated, then the Mod4 survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 16
comp_qol <- partsbq %>%  filter(!is.na(d_843688458) & (d_320303124!=231311385))

if (nrow(comp_qol) > 0) {
  temp <- comp_qol %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 16,
      rule_label = "If the PROMIS survey completion timestamp is populated, then the PROMIS survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 17
comp_ces <- partsbq %>%  filter(!is.na(d_199471989) & (d_956490759!=231311385))

if (nrow(comp_ces) > 0) {
  temp <- comp_ces %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 17,
      rule_label = "If the CES survey completion timestamp is populated, then the CES survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 18
comp_bum <- partsbq %>%  filter(!is.na(d_222161762) & (d_265193023!=231311385) & 
                                  !(Connect_ID %in% c("7568162154", "7659966139")))

if (nrow(comp_bum) > 0) {
  temp <- comp_bum %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 18,
      rule_label = "If the BUM survey completion timestamp is populated, then the BUM survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 19
comp_bu <- partsbq %>%  filter(!is.na(d_764863765) & (d_253883960!=231311385))

if (nrow(comp_bu) > 0) {
  temp <- comp_bu %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 19,
      rule_label = "If the BU survey completion timestamp is populated, then the BU survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 20
comp_mw <- partsbq %>%  filter(!is.na(d_195145666) & (d_547363263!=231311385) & 
                                 Connect_ID!="1981842976")

if (nrow(comp_mw) > 0) {
  temp <- comp_mw %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 20,
      rule_label = "If the MW survey completion timestamp is populated, then the MW survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 21
comp_cv <- partsbq %>%  filter(!is.na(d_784810139) & (d_220186468!=231311385) & 
                                 Connect_ID!="7568162154")

if (nrow(comp_cv) > 0) {
  temp <- comp_cv %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 21,
      rule_label = "If the COVID survey completion timestamp is populated, then the COVID survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 22
comp_mns <- partsbq %>%  filter(!is.na(d_217640691) & (d_459098666!=231311385))

if (nrow(comp_mns) > 0) {
  temp <- comp_mns %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 22,
      rule_label = "If the Menstrual History survey completion timestamp is populated, then the Menstrual History survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 23
comp_ssn <- partsbq %>%  filter(!is.na(d_315032037) & (d_126331570!=231311385) & 
                                  !(Connect_ID %in% c("2325948009", "4465969971", "8564038285")))

if (nrow(comp_ssn) > 0) {
  temp <- comp_ssn %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 23,
      rule_label = "If the SSN survey completion timestamp is populated, then the SSN survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 24
comp_csh <- partsbq %>%  filter(!is.na(d_389890053) & (d_176068627!=231311385))

if (nrow(comp_csh) > 0) {
  temp <- comp_csh %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 24,
      rule_label = "If the CSH survey completion timestamp is populated, then the CHS History survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}


## Clearing up space in GCP memory
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", "base_vars", "safe_arrange")))
gc()


# Rule 25
start_m1 <- partsbq %>%  filter(!is.na(d_205553981) & (d_949302066==972455046 | is.na(d_949302066)))

if (nrow(start_m1) > 0) {
  temp <- start_m1 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 25,
      rule_label = "If the Mod1 survey start timestamp is populated, then the Mod1 survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 26
start_m2 <- partsbq %>%  filter(!is.na(d_541836531) & (d_536735468==972455046 | is.na(d_536735468)))

if (nrow(start_m2) > 0) {
  temp <- start_m2 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 26,
      rule_label = "If the Mod2 survey start timestamp is populated, then the Mod2 survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 27
start_m3 <- partsbq %>%  filter(!is.na(d_386488297) & (d_976570371==972455046 | is.na(d_976570371)))

if (nrow(start_m3) > 0) {
  temp <- start_m3 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 27,
      rule_label = "If the Mod3 survey start timestamp is populated, then the Mod3 survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 28
start_m4 <- partsbq %>%  filter(!is.na(d_452942800) & (d_663265240==972455046 | is.na(d_663265240)) & 
                                  !(Connect_ID %in% c("7568162154", "7659966139")))

if (nrow(start_m4) > 0) {
  temp <- start_m4 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 28,
      rule_label = "If the Mod4 survey start timestamp is populated, then the Mod4 survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 29
start_qol <- partsbq %>% filter(!is.na(d_870643066) & (d_320303124 == 972455046 | is.na(d_320303124)))

if (nrow(start_qol) > 0) {
  temp <- start_qol %>%
    select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 29,
      rule_label = "If the PROMIS survey start timestamp is populated, then the PROMIS survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 30
start_ces <- partsbq %>%  filter(!is.na(d_263355177) & (d_956490759==972455046 | is.na(d_956490759)))

if (nrow(start_ces) > 0) {
  temp <- start_ces %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 30,
      rule_label = "If the CES survey start timestamp is populated, then the CES survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 31
start_bum <- partsbq %>%  filter(!is.na(d_822499427) & (d_265193023==972455046 | is.na(d_265193023)) & 
                                   !(Connect_ID %in% c('7568162154', "7659966139")))

if (nrow(start_bum) > 0) {
  temp <- start_bum %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 31,
      rule_label = "If the BUM survey start timestamp is populated, then the BUM survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 32
start_bu <- partsbq %>%  filter(!is.na(d_534669573) & (d_253883960==972455046 | is.na(d_253883960)))

if (nrow(start_bu) > 0) {
  temp <- start_bu %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 32,
      rule_label = "If the BU survey start timestamp is populated, then the BU survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 33
start_mw <- partsbq %>%  filter(!is.na(d_286191859) & (d_547363263==972455046 | is.na(d_547363263)) & 
                                  Connect_ID!="1981842976")

if (nrow(start_mw) > 0) {
  temp <- start_mw %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 33,
      rule_label = "If the MW survey start timestamp is populated, then the MW survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 34
start_cv <- partsbq %>%  filter(!is.na(d_268176409) & (d_220186468==972455046 | is.na(d_220186468)) & 
                                  Connect_ID!="7568162154")

if (nrow(start_cv) > 0) {
  temp <- start_cv %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 34,
      rule_label = "If the COVID survey start timestamp is populated, then the COVID survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 35
start_mns <- partsbq %>%  filter(!is.na(d_844088537) & (d_459098666==972455046 | is.na(d_459098666)))

if (nrow(start_mns) > 0) {
  temp <- start_mns %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 35,
      rule_label = "If the Menstrual History survey start timestamp is populated, then the Menstrual History survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 36
start_ssn <- partsbq %>%  filter(!is.na(d_943232079) & (d_126331570==972455046 | is.na(d_126331570)) & 
                                   !(Connect_ID %in% c("3307719002", "4465969971")))

if (nrow(start_ssn) > 0) {
  temp <- start_ssn %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 36,
      rule_label = "If the SSN survey start timestamp is populated, then the SSN survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 37
start_csh <- partsbq %>%  filter(!is.na(d_609630315) & (d_176068627==972455046 | is.na(d_176068627)))

if (nrow(start_csh) > 0) {
  temp <- start_csh %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 37,
      rule_label = "If the CSH survey start timestamp is populated, then the CSH History survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 38
csh_null_bq <-

  "SELECT Connect_ID,
CASE
    WHEN d_827220437 = '125001209' THEN 'Kaiser Permanente Colorado'
    WHEN d_827220437 = '472940358' THEN 'Baylor Scott & White Health'
    WHEN d_827220437 = '300267574' THEN 'Kaiser Permanente Hawaii'
    WHEN d_827220437 = '303349821' THEN 'Marshfield Clinic Health System'
    WHEN d_827220437 = '327912200' THEN 'Kaiser Permanente Georgia'
    WHEN d_827220437 = '452412599' THEN 'Kaiser Permanente Northwest'
    WHEN d_827220437 = '517700004' THEN 'National Cancer Institute'
    WHEN d_827220437 = '531629870' THEN 'HealthPartners'
    WHEN d_827220437 = '548392715' THEN 'Henry Ford Health System'
    WHEN d_827220437 = '657167265' THEN 'Sanford Health'
    WHEN d_827220437 = '809703864' THEN 'University of Chicago Medicine'
    ELSE NULL
END AS Site
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants`
where d_176068627 is null and
Connect_ID in (select Connect_ID from `nih-nci-dceg-connect-prod-6d04.FlatConnect.cancerScreeningHistorySurvey` )
order by Site"

csh_null_bq_table <- bq_project_query(project, csh_null_bq)
csh_null <- bq_table_download(csh_null_bq_table, bigint = "integer64")
csh_null$Connect_ID <- as.numeric(csh_null$Connect_ID)

if (nrow(csh_null) > 0) {
  temp <- csh_null %>% select(Connect_ID, Site) %>% mutate(token = NA, rule_id = 38, rule_label = "If the Cancer Screening History flag is null, there should be no survey data.")
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 39
hipaa_consent_match_pull <-
  "SELECT Connect_ID FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` 
where 
FORMAT_DATETIME('%Y-%m-%d %H:%M', TIMESTAMP(d_262613359)) 
  != FORMAT_DATETIME('%Y-%m-%d %H:%M', TIMESTAMP(d_454445267))
  AND DATE(d_454445267) > '2023-12-01'"

hipaa_consent_match_table <- bq_project_query(project, hipaa_consent_match_pull)
hipaa_consent_match <- bq_table_download(hipaa_consent_match_table, bigint = "integer64")
hipaa_consent_match$Connect_ID <- as.numeric(hipaa_consent_match$Connect_ID)

if (nrow(hipaa_consent_match) > 0) {
  temp <- hipaa_consent_match %>% left_join(partsbq %>% select(Connect_ID, token, Site), by = "Connect_ID") %>%
    mutate(
      rule_id = 39,
      rule_label = "The Autogenerated date/time stamp for HIPAA Authorization and Time consent submitted should match down to the minute (after 12/1/2023)."
    )
  all_errors <- bind_rows(all_errors, temp)
}


### BDAY CARD RULES
bdaybq <- "SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.birthdayCard` where Connect_ID IS NOT NULL"
bdaybq_table <- bq_project_query(project, bdaybq)
bdaybq <- bq_table_download(bdaybq_table, bigint = "integer64")

bdaybq$Connect_ID <- as.numeric(bdaybq$Connect_ID)
bday_base_vars= left_join(partsbq, bdaybq, by="Connect_ID")




# Rule 40
bday1 <- bday_base_vars %>%  filter(!is.na(d_768313785) & is.na(d_194486780))

if (nrow(bday1) > 0) {
  temp <- bday1 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 40,
      rule_label = "If maildate is populated then cardVersion should be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 41
bday2 <- bday_base_vars %>%  filter(is.na(d_768313785) & !is.na(d_194486780))

if (nrow(bday2) > 0) {
  temp <- bday2 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 41,
      rule_label = "If cardVersion d_194486780 is populated then maildate d_768313785 should be populated"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 42
bday3 <- bday_base_vars %>%  filter(!is.na(d_768313785) & !(d_916186376 %in% c("01", "02", "06", "07") | is.na(d_916186376)))

if (nrow(bday3) > 0) {
  temp <- bday3 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 42,
      rule_label = "If maildate d_768313785 is populated then dispCode d_916186376 should be values 01, 02, 06, 07, or N/A"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 43
bday4 <- bday_base_vars %>%  filter(is.na(d_768313785) & !is.na(d_916186376))

if (nrow(bday4) > 0) {
  temp <- bday4 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 43,
      rule_label = "If dispCode d_916186376 is populated then maildate d_768313785 should be populated"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 44
bday5 <- bday_base_vars %>%  filter(!is.na(d_768313785) & !(is.na(ymd_hms(d_194486780, quiet = TRUE)) | is.na(d_194486780)))

if (nrow(bday5) > 0) {
  temp <- bday5 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 44,
      rule_label = "If maildate d_768313785 is populated then returnDate d_902078073 should be submitted as an ISO 8601 timestamp or be N/A"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 45
bday6 <- bday_base_vars %>%  filter(is.na(d_768313785) & !is.na(d_902078073))

if (nrow(bday6) > 0) {
  temp <- bday6 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 45,
      rule_label = "If returnDate d_902078073 is populated then maildate d_768313785 should be populated"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 46
reinvite_date_type <- partsbq %>%  filter(!is.na(d_439351436) & is.na(d_280021666))

if (nrow(reinvite_date_type) > 0) {
  temp <- reinvite_date_type %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 46,
      rule_label = "If re-invitation date set, then re-invitation campaign type data should be sent"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 47
reinvite_start <- base_vars %>%  filter(as.Date(d_439351436) < "2025-02-06")

if (nrow(reinvite_start) > 0) {
  temp <- reinvite_start %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 47,
      rule_label = "There should not be any re-invitation data sent before 2/6/2025"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 48
consent_verif <- partsbq %>%  filter((d_919254129=="104430631" | is.na(d_919254129)) & d_821247024=="197316935")

if (nrow(consent_verif) > 0) {
  temp <- consent_verif %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 48,
      rule_label = "If RcrtCS_Consented_v1r0=no, RcrtV_Verification_v1r0 cannot be 'verified'. If RcrtV_Verification_v1r0 is 'verified', then RcrtCS_Consented_v1r0 must be yes."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 49
start_DHQ <- partsbq %>%  filter(!is.na(d_109610692) & (d_692560814==972455046 | d_692560814==789467219 | is.na(d_692560814)))

if (nrow(start_DHQ) > 0) {
  temp <- start_DHQ %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 49,
      rule_label = "If the DHQ survey start timestamp is populated, then the DHQ History survey flag must be 'completed' or 'started'"
    )
  all_errors <- bind_rows(all_errors, temp)
}



## Clearing up space in GCP memory
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", "base_vars", "safe_arrange")))
gc()




# Rule 50
comp_DHQ <- partsbq %>%  filter(!is.na(d_610227793) & (d_692560814==972455046 | d_692560814==789467219 | is.na(d_692560814)))

if (nrow(comp_DHQ) > 0) {
  temp <- comp_DHQ %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 50,
      rule_label = "If the DHQ survey completion timestamp is populated, then the DHQ History survey flag must be 'completed'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 51
dhq_qualified <- partsbq %>%  filter((d_692560814==615768760 | d_692560814==231311385) & 
                                       !(d_821247024 == 197316935 & as.Date(d_914594314) >="2024-12-01" & 
                                           (d_747006172==104430631 | (as.Date(d_659990606) > as.Date(d_610227793))) &
                                           difftime(Sys.Date(), as.Date(d_914594314), units="days") >=180))

if (nrow(dhq_qualified) > 0) {
  temp <- dhq_qualified %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 51,
      rule_label = "If a participant survey status (SrvDHQ3_6moStatus_v1r0) is started or submitted, they must be verified, verified on or after 12/1/24, are not withdrawn, AND have reached 180 days post verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 52
dhq_eligible <- partsbq %>%  filter(d_692560814==789467219  & !(as.Date(d_914594314) <"2024-12-01"))

if (nrow(dhq_eligible) > 0) {
  temp <- dhq_eligible %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 52,
      rule_label = "If a participant has a survey status (SrvDHQ3_6moStatus_v1r0) of not eligible, they must be verified and have been verified before 12/1/24"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 53
dhq_null <- partsbq %>%  filter(is.na(d_692560814)  & d_821247024 == 197316935 & 
                                  !(as.Date(d_659990606) < "2025-05-30"))

if (nrow(dhq_null) > 0) {
  temp <- dhq_null %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 53,
      rule_label = "If a participant has a null survey status (SrvDHQ3_6moStatus_v1r0), their verification status must not equal verified"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 54
dhq_external_status <- partsbq %>%  filter((d_692560814==615768760 | d_692560814==231311385) & 
                                             difftime(Sys.time(), as.Date(d_109610692), units="hours") >24 & 
                                             is.na(d_501613780))

if (nrow(dhq_external_status) > 0) {
  temp <- dhq_external_status %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 54,
      rule_label = "If a participant has an internal survey status (SrvDHQ3_6moStatus_v1r0) of 'started' or 'submitted' and the time the survey started was more then 24 hours ago then external status (SrvDHQ3_6moPltStatus_v1r0) must be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 55
dhq_uname_populated <- partsbq %>%  filter(!is.na(d_148184166) & (is.na(d_808755658) | is.na(d_196723965)))

if (nrow(dhq_uname_populated) > 0) {
  temp <- dhq_uname_populated %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 55,
      rule_label = "If 6-mo DHQ3 Username – (CID:148184166) is populated, then 6-mo DHQ3 UUID – (CID: 808755658) and 6-mo DHQ3 Study ID – (CID: 196723965) must be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 56
dhq_uuid_populated <- partsbq %>%  filter(!is.na(d_808755658) & (is.na(d_148184166) | is.na(d_196723965)))

if (nrow(dhq_uuid_populated) > 0) {
  temp <- dhq_uuid_populated %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 56,
      rule_label = "If 6-mo DHQ3 UUID – (CID: 808755658) is populated, then 6-mo DHQ3 Username – (CID:148184166) and 6-mo DHQ3 Study ID – (CID: 196723965) must be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 57
dhq_studyID_populated <- partsbq %>%  filter(!is.na(d_196723965) & (is.na(d_148184166) | is.na(d_808755658)))

if (nrow(dhq_studyID_populated) > 0) {
  temp <- dhq_studyID_populated %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 57,
      rule_label = "If 6-mo DHQ3 Study ID – (CID: 196723965) is populated, then 6-mo DHQ3 Username – (CID:148184166) and 6-mo DHQ3 UUID – (CID: 808755658) must be populated."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 58
dhq_system_null <- partsbq %>%  filter((is.na(d_692560814) | d_692560814==789467219) & !(is.na(d_148184166) & is.na(d_808755658) & is.na(d_196723965)))

if (nrow(dhq_system_null) > 0) {
  temp <- dhq_system_null %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 58,
      rule_label = "If survey status (SrvDHQ3_6moStatus_v1r0) is 'not yet eligible' or null, then 6-mo DHQ3 Username – (CID:148184166), 6-mo DHQ3 UUID – (CID: 808755658) and 6-mo DHQ3 Study ID – (CID: 196723965) must all be null"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 59
smmet <- partsbq %>%  filter(d_100767870==353358909 & (d_949302066!=231311385 | d_536735468!=231311385 | d_976570371!=231311385 | d_663265240!=231311385) & 
                               !(Connect_ID %in% c(6213377542, 6891536539, 1514220001, 8583917079)))

if (nrow(smmet) > 0) {
  temp <- smmet %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 59,
      rule_label = "If SMMet_BaseSrvCompl_v1r0 = yes, then SrvBOH_BaseStatus_v1r0, SrvMRE_BaseStatus_v1r0, SrvSAS_BaseStatus_v1r0, and SrvLAW_BaseStatus_v1r0 must all be 'submitted'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 60
reinv_active <- partsbq %>%  filter(!is.na(d_439351436) & d_512820379!='486306141')

if (nrow(reinv_active) > 0) {
  temp <- reinv_active %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 60,
      rule_label = "If RcrtSI_RInvTimeStamp_v1r0 is populated, then RcrtSI_RecruitType_v1r0 must be 'active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 61
auto_ver <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                 as.Date(d_914594314) < (currentDate - days(5)) & 
                                 !(state_d_444699761 %in%	c('734437214', '426360242')))

if (nrow(auto_ver) > 0) {
  temp <- auto_ver %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 61,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Automated verification, should be 'Method not used' or 'Method used'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 62
outreach_ver <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                     as.Date(d_914594314) < (currentDate - days(5)) & 
                                     !(state_d_188797763 %in%	c('353358909', '104430631')))

if (nrow(outreach_ver) > 0) {
  temp <- outreach_ver %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 62,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Outreach required for Verification, should be 'Yes' or 'No'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 63
manual_ver <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                   as.Date(d_914594314) < (currentDate - days(5)) &
                                   !(state_d_953614051 %in%	c('734437214', '426360242')))

if (nrow(manual_ver) > 0) {
  temp <- manual_ver %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 63,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Manual verification, should be 'Method not used' or 'Method used'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 65
fname_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                    as.Date(d_914594314) < (currentDate - days(5)) & 
                                    !(state_d_147176963 %in%	c('356674370', '219803804')))

if (nrow(fname_match) > 0) {
  temp <- fname_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 65,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then First Name Match, should be 'Not Matched' or 'Matched'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 66
lname_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                    as.Date(d_914594314) < (currentDate - days(5)) & 
                                    !(state_d_557461333 %in%	c('356674370', '219803804')))

if (nrow(lname_match) > 0) {
  temp <- lname_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 66,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Last Name Match, should be 'Not Matched' or 'Matched'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 67
dob_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                  as.Date(d_914594314) < (currentDate - days(5)) & 
                                  !(state_d_725929722 %in%	c('356674370', '219803804')))

if (nrow(dob_match) > 0) {
  temp <- dob_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 67,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then DOB Match, should be 'Not Matched' or 'Matched'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 68
pin_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                  as.Date(d_914594314) < (currentDate - days(5)) & 
                                  !(state_d_711794630 %in%	c('356674370', '219803804')))

if (nrow(pin_match) > 0) {
  temp <- pin_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 68,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then PIN Match, should be 'Not Matched' or 'Matched'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 69
token_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                    as.Date(d_914594314) < (currentDate - days(5)) & 
                                    !(state_d_679832994 %in%	c('356674370', '219803804')))

if (nrow(token_match) > 0) {
  temp <- token_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 69,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Token Match, should be 'Not Matched' or 'Matched'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 70
zip_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                  as.Date(d_914594314) < (currentDate - days(5)) & 
                                  !(state_d_559534463 %in%	c('356674370', '219803804')))

if (nrow(zip_match) > 0) {
  temp <- zip_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 70,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then ZIP Code Match, should be 'Not Matched' or 'Matched'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 71
site_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                   as.Date(d_914594314) < (currentDate - days(5)) & 
                                   !(state_d_570452130 %in%	c('539025306', '427405444')))

if (nrow(site_match) > 0) {
  temp <- site_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 71,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Site Match should be 'Criterium not met' or 'Criterium met'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 72
age_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                  as.Date(d_914594314) < (currentDate - days(5)) & 
                                  !(state_d_629484663 %in%	c('539025306', '427405444')))

if (nrow(age_match) > 0) {
  temp <- age_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 72,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Age Match should be 'Criterium not met' or 'Criterium met'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 73
cancer_match <- partsbq	%>% filter(d_821247024=='197316935' & d_512820379 %in%  c('486306141', '854703046') & 
                                     as.Date(d_914594314) < (currentDate - days(5)) & 
                                     !(state_d_547895941 %in%	c('539025306', '427405444')))

if (nrow(cancer_match) > 0) {
  temp <- cancer_match %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 73,
      rule_label = "If RcrtV_Verification_v1r0 is 'Verified' and RcrtSI_RecruitType_v1r0 is 'Active or 'Passive', then Cancer Status Match should be 'Criterium not met' or 'Criterium met'. Participants must be verified for at least 5 days."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 74
act_di_age <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_934298480) & 
                                   !(d_821247024=="922622075" & state_d_793822265=="854903954") & 

                                   !(Connect_ID %in% c("4093473296", "2701575745")))

if (nrow(act_di_age) > 0) {
  temp <- act_di_age %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 74,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' then RcrtSI_Age_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}




## Clearing up space in GCP memory
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", "base_vars", "safe_arrange")))
gc()



# Rule 75
act_di_race <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_849518448) & 
                                    d_827220437!='657167265' & d_827220437!='548392715' &  d_827220437!='472940358' & 
                                    !(d_821247024=="922622075" & state_d_793822265=="854903954") & 
                                    Connect_ID!="4093473296")

if (nrow(act_di_race) > 0) {
  temp <- act_di_race %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 75,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site is not Sanford Health or Henry Ford Health or Baylor Scott and White Health, then RcrtSI_Race_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 76
act_di_SF_race <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_119643471) &
                                       d_827220437=='657167265' &
                                       !(d_821247024=="922622075" & state_d_793822265=="854903954"))

if (nrow(act_di_SF_race) > 0) {
  temp <- act_di_SF_race %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 76,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site= Sanford Health then RcrtSI_SHRace_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 77
act_di_HFH_race <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_684926335) & 
                                        d_827220437=='548392715' &
                                        !(d_821247024=="922622075" & state_d_793822265=="854903954"))

if (nrow(act_di_HFH_race) > 0) {
  temp <- act_di_HFH_race %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 77,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site= Henry Ford Health, then RcrtSI_HFHSRace_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 78
act_di_BSWH_race <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_253532712) & 
                                         d_827220437=='472940358' &

                                         !(d_821247024=="922622075" & state_d_793822265=="854903954")  & 
                                         !(Connect_ID %in% c("2701575745")))
if (nrow(act_di_BSWH_race) > 0) {
  temp <- act_di_BSWH_race %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 78,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site= Baylor Scott and White Health, then RcrtSI_BSWHRaceEth_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 79
act_di_SF_eth <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_538553381) & 
                                      d_827220437=='657167265' & 
                                      !(d_821247024=="922622075" & state_d_793822265=="854903954"))

if (nrow(act_di_SF_eth) > 0) {
  temp <- act_di_SF_eth %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 79,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site= Sanford Health then RcrtSI_SHEthnicity must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 80
act_di_HFH_eth <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_527823810) & 
                                       d_827220437=='548392715' &
                                       !(d_821247024=="922622075" & state_d_793822265=="854903954"))

if (nrow(act_di_HFH_eth) > 0) {
  temp <- act_di_HFH_eth %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 80,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site= Henry Ford Health, then RcrtSI_HFHSEthnicity_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 81
act_di_member <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_477091792) & 
                                      !(d_821247024=="922622075" & state_d_793822265=="854903954") & 

                                      !(Connect_ID %in% c("4093473296", "2701575745")))

if (nrow(act_di_member) > 0) {
  temp <- act_di_member %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 81,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' then RcrtSI_MemberStat_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 82
act_di_cmpn <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_667474224) & 
                                    !(d_821247024=="922622075" & state_d_793822265=="854903954") & 

                                    !(Connect_ID %in% c("4093473296", "2701575745")))

if (nrow(act_di_cmpn) > 0) {
  temp <- act_di_cmpn %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 82,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' then RcrtSI_CampaignType_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 83
act_di_elg <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_749475364) & 
                                   !(d_821247024=="922622075" & state_d_793822265=="854903954") & 

                                   !(Connect_ID %in% c("4093473296", "2701575745")))

if (nrow(act_di_elg) > 0) {
  temp <- act_di_elg %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 83,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' then RcrtSI_EligibilityVer_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 84
act_di_sex <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_706256705) & 
                                   d_827220437!='548392715' &
                                   !(d_821247024=="922622075" & state_d_793822265=="854903954") & 

                                   !(Connect_ID %in% c("4093473296", "2701575745")))

if (nrow(act_di_sex) > 0) {
  temp <- act_di_sex %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 84,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site is not Henry Ford Health, then RcrtSI_Sex_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 85
act_di_HF_sex <- partsbq	%>% filter(d_512820379=='486306141' & is.na(state_d_435027713) & 
                                      d_827220437=='548392715' &
                                      !(d_821247024=="922622075" & state_d_793822265=="854903954"))

if (nrow(act_di_HF_sex) > 0) {
  temp <- act_di_HF_sex %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 85,
      rule_label = "If RcrtSI_RecruitType_v1r0='Active' and Site= Henry Ford Health, then RcrtSI_HFHS_Sex_v1r0 must be populated. This excludes those with RcrtV_Verification_v1r0='duplicate' and RcrtV_UpdateRecType_v1r0='passive to active'."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 86
bq_cv_lang <- "SELECT 
CASE d_827220437 
when '531629870' THEN  'HealthPartners'
when '548392715' THEN  'Henry Ford Health System'
when '125001209' THEN  'Kaiser Permanente Colorado'
when '327912200' THEN  'Kaiser Permanente Georgia'
when '300267574' THEN  'Kaiser Permanente Hawaii'
when '452412599' THEN  'Kaiser Permanente Northwest'
when '303349821' THEN  'Marshfield Clinic Health System'
when '657167265' THEN  'Sanford Health'
when '809703864' THEN  'University of Chicago Medicine'
when '472940358' THEN  'Baylor Scott & White Health'
end as Site,
covid.Connect_ID
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants`  pts
left join `nih-nci-dceg-connect-prod-6d04.FlatConnect.covid19Survey_v1`  covid
on pts.Connect_ID=covid.Connect_ID
where covid.Connect_ID is not null and -- if it is null, they finished COVID in the BU/BUM survey
DATE(pts.d_268176409) >= '2023-07-05' and covid.d_784119588 is null"

covid_tbl <- biobq_table <- bq_project_query(project, bq_cv_lang)
covid_lang <-  bq_table_download(covid_tbl, bigint = "integer64")
covid_lang$Connect_ID <- as.numeric(covid_lang$Connect_ID)

if (nrow(covid_lang) > 0) {
  temp <- covid_lang %>% select(Connect_ID, Site) %>% mutate(token = NA, rule_id = 86, rule_label = "If the COVID-19 Survey was started on or after 7/5/23, then the COVID-19 Survey Language variable must be populated")
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 87
AB1 <- partsbq	%>% mutate(AB_response = case_when(state_d_956485028== '562663942'	~ 'Altruism Personal',
                                                  state_d_956485028== '686986259'	~ 'Altruism General',
                                                  state_d_956485028== '477331464'	~ 'Cancer Personal',
                                                  state_d_956485028== '935486262'	~ 'Cancer General',
                                                  state_d_956485028== '518814501'	~ 'Research Personal',
                                                  state_d_956485028== '307763550'	~ 'Research General'),
                          UTM_camp = gsub("-", " ", d_163847117)) %>%
  filter(!is.na(AB_response) & 
           tolower(AB_response) != tolower(UTM_camp))

if (nrow(AB1) > 0) {
  temp <- AB1 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 87,
      rule_label = "If A/B Testing Message Response is populated, UTM_Campaign parameter for each recruit should match the data we receive for A/B Testing Message Response."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 88
AB2 <- partsbq	%>% filter( !is.na(state_d_956485028) &
                             (is.na(d_761057722) | is.na(d_207613315) | is.na(d_163847117)) & #at least one is missing
                             !(is.na(d_761057722) & is.na(d_207613315) & is.na(d_163847117))) #but not all are missing

if (nrow(AB2) > 0) {
  temp <- AB2 %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 88,
      rule_label = "If A/B Testing Message Response is populated and any one or two of the three UTM parameters are populated, all three should be available. (This issue is being looked at by DevOps in October.)"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 89
Cnsnt <- partsbq	%>% filter(is.na(d_454445267) & d_919254129=='353358909')

if (nrow(Cnsnt) > 0) {
  temp <- Cnsnt %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 89,
      rule_label = "If RcrtCS_ConsentSumit_v1r0 does not exist then RcrtCS_Consented_v1r0 must = no or NA"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 90
UP_Sub_Time <- partsbq	%>% filter(is.na(d_430551721) & d_831041022=='104430631' & d_699625233=='353358909')

if (nrow(UP_Sub_Time) > 0) {
  temp <- UP_Sub_Time %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 90,
      rule_label = "If RcrtUP_SubmitTime_v1r0 does not exist and HdWd_Destroydata_v1r0 = no, then RcrtUP_Submitted_v1r0 must = no or NA"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 91
UP_Sub_Time <- partsbq	%>% filter(d_699625233=='353358909' & d_831041022=='104430631' & (d_919254129=='104430631' | is.na(d_919254129)))

if (nrow(UP_Sub_Time) > 0) {
  temp <- UP_Sub_Time %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 91,
      rule_label = "If RcrtUP_Submitted_v1r0=yes and HdWd_Destroydata_v1r0 = no, then RcrtCS_Consented_v1r0 must be yes."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 92
SignIn_Time <- partsbq	%>% filter(is.na(d_335767902) & d_831041022=='104430631' & d_230663853=='353358909')

if (nrow(SignIn_Time) > 0) {
  temp <- SignIn_Time %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 92,
      rule_label = "If RcrtSI_SignTime_v1r0 does not exist and HdWd_Destroydata_v1r0 = no, then RcrtSI_SignedIn_v1r0 must = no or NA"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 93
Verification_Time <- partsbq	%>% filter(is.na(d_914594314) & d_831041022=='104430631' & d_821247024!='875007964')

if (nrow(Verification_Time) > 0) {
  temp <- Verification_Time %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 93,
      rule_label = "If RcrtV_VerificationTm_V1R0 does not exist and HdWd_Destroydata_v1r0 = no, then RcrtV_Verification_v1r0 must = not yet verified or NA"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 94
Man_OutRch <- partsbq	%>% filter((is.na(state_d_953614051) | state_d_953614051=='734437214') & state_d_188797763=='353358909')

if (nrow(Man_OutRch) > 0) {
  temp <- Man_OutRch %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 94,
      rule_label = "If RcrtV_Mannual_v1r0 = method not used or NA, then RcrtV_Outreach_v1r0 must be no  or NA"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 95
Non_act_verif <- base_vars %>%  filter(d_512820379 == 180583933 & 

                                         (d_821247024 %in% c(219863910, 922622075, 197316935) &
                                            !(state_d_148197146 %in% c(283434980, 866029623))))

if (nrow(Non_act_verif) > 0) {
  temp <- Non_act_verif %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 95,
      rule_label = "If participants are \"not active\", they cannot have verification completed; this excludes those with duplicate type 'Not Active recruit signed in as Passive recruit' and 'Not Active recruit signed in as an Active recruit'"
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 96
update_recr <- base_vars %>% filter(d_821247024 == 197316935 & is.na(state_d_793822265) &

                                      as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)

if (nrow(update_recr) > 0) {
  temp <- update_recr %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 96,
      rule_label = "If verified, update recruit type should be sent. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}

# Rule 97
roi_null_bq <-

  "SELECT Connect_ID,
CASE
    WHEN d_827220437 = '125001209' THEN 'Kaiser Permanente Colorado'
    WHEN d_827220437 = '472940358' THEN 'Baylor Scott & White Health'
    WHEN d_827220437 = '300267574' THEN 'Kaiser Permanente Hawaii'
    WHEN d_827220437 = '303349821' THEN 'Marshfield Clinic Health System'
    WHEN d_827220437 = '327912200' THEN 'Kaiser Permanente Georgia'
    WHEN d_827220437 = '452412599' THEN 'Kaiser Permanente Northwest'
    WHEN d_827220437 = '517700004' THEN 'National Cancer Institute'
    WHEN d_827220437 = '531629870' THEN 'HealthPartners'
    WHEN d_827220437 = '548392715' THEN 'Henry Ford Health System'
    WHEN d_827220437 = '657167265' THEN 'Sanford Health'
    WHEN d_827220437 = '809703864' THEN 'University of Chicago Medicine'
    ELSE NULL
END AS Site
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants`
where d_278023676 is null and
Connect_ID in (select Connect_ID from `nih-nci-dceg-connect-prod-6d04.FlatConnect.preference2026` )
order by Site"

roi_null_bq_table <- bq_project_query(project, roi_null_bq)
roi_null <- bq_table_download(roi_null_bq_table, bigint = "integer64")
roi_null$Connect_ID <- as.numeric(roi_null$Connect_ID)

if (nrow(roi_null) > 0) {
  temp <- roi_null %>% select(Connect_ID, Site) %>% 
    mutate(token = NA, 
           rule_id = 97, 
           rule_label = "If the ROI Pref Survey Status flag is null, there should be no survey data.")
  all_errors <- bind_rows(all_errors, temp)
}




# Rule 98
update_recr_by_type <- base_vars %>% filter(d_821247024!="922622075" & 
                                              !is.na(state_d_148197146) &
                                              as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)

if (nrow(update_recr_by_type) > 0) {

  temp <- update_recr_by_type %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 98,
      rule_label = "If verification != duplicate, but instead equal to either 'not yet verif', 'verif', 'cannot be verified', 'outreach timed out', or 'no longer enrolled', then 'Duplicate type' should be NA. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}




# Rule 99
Outreach_manual <- base_vars %>% filter(state_d_188797763==353358909 &
                                          (state_d_953614051==734437214 | is.na(state_d_953614051)) &
                                          as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)
if (nrow(Outreach_manual) > 0) {
  temp <- Outreach_manual %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 99,
      rule_label = "If RcrtV_Outreach = yes then RcrtV_Mannual has to = method used. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}



## Clearing up space in GCP memory
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", "base_vars", "safe_arrange")))
gc()



# Rule 100
Auto_verif <- base_vars %>% filter(d_821247024==197316935 & state_d_953614051==734437214 &
                                     (state_d_444699761==734437214 | is.na(state_d_444699761)) &
                                     as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)
if (nrow(Auto_verif) > 0) {
  temp <- Auto_verif %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 100,
      rule_label = "If verif status = verified and Manual Verif= method not used, then Auto Verif must be 'method used. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}



# Rule 101
Man_Auto_verif <- base_vars %>% filter(d_821247024==197316935 & state_d_444699761==734437214 &
                                         (state_d_953614051==104430631 | is.na(state_d_953614051)) &
                                         as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)
if (nrow(Man_Auto_verif) > 0) {
  temp <- Man_Auto_verif %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 101,
      rule_label = "If verif status = verified and Auto Verif=method not used, then Manual Verif must be method used. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}



# Rule 102
Dupl_type <- base_vars %>% filter(d_821247024==922622075 & is.na(state_d_148197146) &
                                    as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)

if (nrow(Man_Auto_verif) > 0) {
  temp <- Man_Auto_verif %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 102,
      rule_label = "If verifcation status = duplicate, there should be a duplicate type. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}



# Rule 103
Dupl_type_reverse <- base_vars %>% filter(d_821247024!=922622075 & !is.na(state_d_148197146) &
                                            as.numeric(difftime(currentDate, as.Date(d_914594314), units = "days"))>5)
if (nrow(Dupl_type_reverse) > 0) {
  temp <- Dupl_type_reverse %>% select(Connect_ID, token, Site) %>%
    mutate(
      rule_id = 103,
      rule_label = "If there's a duplicate type, verifcation status must = 'duplicate'. This rule allows for a lag of 5 days post-verification."
    )
  all_errors <- bind_rows(all_errors, temp)
}




## -------  SECOND TAB IN THE OUTPUT FILE TO LIST ALL RULES FROM THIS FILE ---------------

# Build rule list from the current script source so all rules are captured, even if no error rows exist.
script_file <- "QC_Errors_Excel.R"
if (!file.exists(script_file)) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_file <- sub("^--file=", "", file_arg[1])
  }
}

rule_definitions <- tibble(rule_id = integer(), rule_label = character())
if (file.exists(script_file)) {
  rule_lines <- readLines(script_file, warn = FALSE)
  rule_pattern <- 'rule_id\\s*=\\s*(\\d+)\\s*,\\s*rule_label\\s*=\\s*"((?:[^"\\\\]|\\\\.)*?)"'
  rule_matches <- stringr::str_match(rule_lines, rule_pattern)
  rule_matches <- rule_matches[!is.na(rule_matches[, 1]), , drop = FALSE]
  if (nrow(rule_matches) > 0) {
    rule_definitions <- tibble(
      rule_id = as.integer(rule_matches[, 2]),
      rule_label = rule_matches[, 3]
    )
    rule_definitions <- rule_definitions %>%
      mutate(rule_label = vapply(rule_label, function(x) eval(parse(text = paste0('"', x, '"'))), character(1))) %>%
      distinct(rule_id, .keep_all = TRUE) %>%
      arrange(rule_id)
  }
}

# Write both sheets to Excel
currentDate <- Sys.Date()
boxfolder <- 194196493018

openxlsx::write.xlsx(
  list(QC_Errors = all_errors, Rule_List = rule_definitions),
  glue("Recruitment_Custom_QC_Output_{currentDate}_boxfolder_{boxfolder}.xlsx"),
  row.names = F, na="",
  overwrite = TRUE
)

cat("Excel file 'Recruitment_Custom_QC_Output.xlsx' has been created.\n")
