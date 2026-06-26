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
library(logger)

options(tinytex.verbose = TRUE)

bq_auth()

project <- "nih-nci-dceg-connect-prod-6d04"
currentDate <- Sys.Date()
dataset <- "FlatConnect"

con <- dbConnect(
  bigrquery::bigquery(),
  project = project,
  dataset = dataset,
  billing = project
)

dbListTables(con)

chunk1 <- tbl(con, "participants", page_size = 1000) %>%
  filter(d_831041022=='104430631') %>%
  dplyr::select(Connect_ID, token, d_512820379, d_471593703, state_d_934298480, d_230663853, state_d_697256759, state_d_158291096,
                d_335767902, d_982402227, d_919254129, d_699625233, d_827220437, d_371067537, d_544150384, d_564964481, d_795827569, d_685002411_d_994064239, d_912301837,
                d_685002411_d_194410742,d_685002411_d_949501163,d_685002411_d_277479354,d_685002411_d_867203506,d_685002411_d_352996056,d_685002411_d_217367618,d_747006172,
                d_906417725,d_773707518,d_831041022,d_987563196, d_659990606, d_100767870, state_studyId, state_d_521025370) %>%
  as_tibble() %>%
  distinct(token, .keep_all = TRUE)
chunk1$Connect_ID <- as.numeric(chunk1$Connect_ID)

log_info("First participants table variable set pulled")

chunk2 <- tbl(con, "participants", page_size = 1000) %>%
  filter(d_831041022=='104430631') %>%
  dplyr::select(Connect_ID, token, d_430551721, d_821247024, d_914594314, state_d_725929722, d_126331570, d_536735468, d_130371375_d_266600170_d_945795905, d_130371375_d_266600170_d_320023644, state_d_538553381, state_d_527823810, state_d_849518448, state_d_119643471, state_d_253532712, state_d_684926335,
                d_663265240, d_878865966, d_684635302, d_167958071, d_130371375_d_266600170_d_731498909, d_130371375_d_266600170_d_222373868, d_130371375_d_266600170_d_787567527,
                d_949302066, d_517311251, d_205553981, d_117249500, d_976570371, d_914639140, d_311580100, d_454445267, d_255077064, d_832139544,
                d_263355177, d_199471989, d_222161762, d_822499427, d_764863765, d_534669573, d_195145666, d_286191859, d_541836531, d_770257102, d_386488297, d_264644252, d_452942800, state_d_706256705, state_d_435027713, state_d_477091792, state_d_667474224, state_d_749475364) %>%
  as_tibble() %>%
  distinct(token, .keep_all = TRUE)
chunk2$Connect_ID <- as.numeric(chunk2$Connect_ID)


log_info("Second participants table variable set pulled")

chunk3 <- tbl(con, "participants", page_size = 1000) %>%
  filter(d_831041022=='104430631') %>%
  dplyr::select(Connect_ID, token, d_217640691, d_844088537, d_784810139, d_268176409, d_843688458, d_870643066, d_315032037, d_943232079, d_692560814,
                d_956490759,d_265193023,d_253883960,d_547363263,d_459098666,d_220186468,d_320303124, d_609630315, d_389890053, d_176068627, d_439351436, d_280021666,
                d_610227793, d_109610692, d_148184166, d_808755658, d_196723965, d_501613780, d_130371375_d_266600170_d_648936790,
                state_d_444699761, state_d_188797763, state_d_953614051, state_d_793822265, state_d_147176963, state_d_557461333, state_d_711794630,
                state_d_679832994, state_d_559534463, state_d_570452130, state_d_629484663, state_d_547895941,
                d_685002411_d_936015433, d_685002411_d_688142378, d_685002411_d_101763809, d_685002411_d_525277409, d_685002411_d_671903816,
                d_761057722, d_207613315, d_163847117, state_d_956485028, state_d_148197146) %>%
  as_tibble() %>%
  distinct(token, .keep_all = TRUE)
chunk3$Connect_ID <- as.numeric(chunk3$Connect_ID)


log_info("Third participants table variable set pulled")

parts_bq_partial <- full_join(chunk1, chunk2, by=c("token", "Connect_ID"))
partsbq <- full_join(parts_bq_partial, chunk3, by=c("token", "Connect_ID"))

biobq <- "SELECT Connect_ID, token, d_299553921_d_593843561, d_299553921_d_883732523, d_703954371_d_593843561, d_703954371_d_883732523, d_838567176_d_593843561, d_838567176_d_883732523, d_454453939_d_593843561, d_454453939_d_883732523, d_652357376_d_593843561, d_652357376_d_883732523, d_505347689_d_593843561, d_505347689_d_883732523 FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.biospecimen`
where Connect_ID IS NOT NULL"
biobq_table <- bq_project_query(project, biobq)
biobq <- bq_table_download(biobq_table, bigint = "integer64")

partsbq$Connect_ID <- as.numeric(partsbq$Connect_ID)
biobq$Connect_ID <- as.numeric(biobq$Connect_ID)

partsbq <- partsbq %>% mutate(
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

base_vars <- left_join(partsbq, biobq, by=c("Connect_ID", "token"))





 # ----------   Functions -----------------------------------------------
safe_arrange <- function(df, ...) {
  if (nrow(df) > 0) df %>% arrange(...) else df
}

# Helper: pivot named explanation variables into the flat explanation_variable# / explanation_variable_value# schema.
# `vars` is a named list: list("Variable Name1" = conceptId1, "Variable Name2" = conceptId2, ...)
make_explanation_cols <- function(df, vars) {
  for (i in seq_along(vars)) {
    
    # Variable Name from DD column S
    df[[paste0("explanation_variable", i)]]       <- names(vars)[i]
    
    # Variable conceptId from DD column N (and/or G if nested)
    ## Need character value in case there are any variable conversions, like date
    df[[paste0("explanation_variable_value", i)]] <- as.character(vars[[i]])
  }
  df
}


# Output info storage: rule_id, rule_label, Variable name, concept ID value
tag_rule <- function(df, rule_num, rule_desc, vars) {
  rules_registry[[length(rules_registry) + 1]] <<- data.frame(rule_id = rule_num, rule_label = rule_desc)
  df %>%
    mutate(rule_id = rule_num, rule_label = rule_desc) %>%
    make_explanation_cols(vars)
}



# Store exclusions for Exclusions tab
register_exclusions <- function(rule_num, ids) {
  exclusions_registry[[length(exclusions_registry) + 1]] <<- data.frame(
    rule_id    = rule_num,
    Connect_ID = as.character(ids)
  )
}





# Empty lists for rules, errors, and exclusions 
rules_registry <- list()
all_errors <- data.frame(stringsAsFactors = FALSE)
exclusions_registry <- list()



## ---- RULES ---------------------------------------------------------------
log_info("Starting to run the rules")

# Rule 1
incentive2 <- base_vars %>%
  filter(d_949302066 == '231311385' & d_536735468 == '231311385' & 
           d_976570371 == '231311385' & d_663265240 == '231311385' &
           ((d_299553921_d_593843561==353358909 & d_299553921_d_883732523 != 681745422) |
              (d_703954371_d_593843561==353358909 & d_703954371_d_883732523 != 681745422) |
              (d_838567176_d_593843561==353358909 & d_838567176_d_883732523 != 681745422) |
              (d_454453939_d_593843561==353358909 & d_454453939_d_883732523 != 681745422) |
              (d_652357376_d_593843561==353358909 & d_652357376_d_883732523 != 681745422) |
              (d_505347689_d_593843561==353358909 & d_505347689_d_883732523 != 681745422)) &
           (d_130371375_d_266600170_d_731498909!=353358909 | d_130371375_d_266600170_d_222373868!=353358909 | 
              is.na(d_130371375_d_266600170_d_787567527))) %>%
  safe_arrange(Site) %>%
  tag_rule(1,
           "If all BL Modules are completed and the participant has a baseline research collection where the tube is collected and the reason tube wasn't collected wasn't refusal, then SMPaym_TmPaymEligBL_v1r0 must be populated.",
           list("SMPaym_PaymEligBL"     = .$d_130371375_d_266600170_d_731498909,
                "SMPaym_NORCPaymEligBL" = .$d_130371375_d_266600170_d_222373868,
                "SMPaym_TmPaymEligBL"   = .$d_130371375_d_266600170_d_787567527))
all_errors <- bind_rows(all_errors, incentive2)


# Rule 2 — BigQuery pull
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
p.d_764863765, p.d_222161762
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` p
JOIN combined_survey c ON p.Connect_ID = c.Connect_ID
WHERE (p.d_253883960 = '231311385' OR p.d_265193023 = '231311385')"

mens_table <- bq_project_query(project, mens_ccc)
mens <- bq_table_download(mens_table, bigint = "integer64")
mens$Connect_ID <- as.numeric(mens$Connect_ID)
mens <- mens %>% mutate(Site = case_when(
  d_827220437 == 125001209 ~ "KPCO", d_827220437 == 472940358 ~ "BSWH",
  d_827220437 == 300267574 ~ "KPHI", d_827220437 == 303349821 ~ "MF",
  d_827220437 == 327912200 ~ "KPGA", d_827220437 == 452412599 ~ "KPNW",
  d_827220437 == 517700004 ~ "NCI",  d_827220437 == 531629870 ~ "HP",
  d_827220437 == 548392715 ~ "HFH",  d_827220437 == 657167265 ~ "SF",
  d_827220437 == 809703864 ~ "UC"
))

can_no_longer_contact <- list("6673328645","1156167603","5950274173","1674063031","5460895468","9586303599",
                              "2009345083","8140966374","4038472238","3379215871","6592021474","1721018129",
                              "3023733685","5085319740","8682077118","1441310478","4177949257","6384048173",
                              "1186358664","2510716583","1782585066")
register_exclusions(2, unlist(can_no_longer_contact))

mens_elg <- mens %>%
  filter(is.na(d_289750687) &
           (d_764863765 >= as.Date("2022-12-09") | d_222161762 >= as.Date("2022-12-09")) &
           !(Connect_ID %in% can_no_longer_contact)) %>%
  safe_arrange(Site) %>%
  tag_rule(2,
           "If either the BUM Survey or the BU Survey was completed with SrvBlU_MENST60_v2r0 answered yes, then SrvMC_MenstSrvElig_v1r0 must be yes.",
           list("SrvMC_MenstSrvElig_v1r0" = .$d_289750687,
                "BU_CompletionTm"         = .$d_764863765,
                "BUM_CompletionTm"        = .$d_222161762))
all_errors <- bind_rows(all_errors, mens_elg)


# Rule 3 — BigQuery pull
MC_ccc_NE <- "SELECT CASE
  WHEN d_827220437='125001209' THEN 'KPCO'   WHEN d_827220437='472940358' THEN 'BSWH'
  WHEN d_827220437='300267574' THEN 'KPHI'   WHEN d_827220437='303349821' THEN 'MF'
  WHEN d_827220437='327912200' THEN 'KPGA'   WHEN d_827220437='452412599' THEN 'KPNW'
  WHEN d_827220437='517700004' THEN 'NCI'    WHEN d_827220437='531629870' THEN 'HP'
  WHEN d_827220437='548392715' THEN 'HFH'    WHEN d_827220437='657167265' THEN 'SF'
  WHEN d_827220437='809703864' THEN 'UC'
END AS Site,
Connect_ID, token,
CASE WHEN d_289750687 is null THEN 'No' WHEN d_289750687='353358909' THEN 'Yes' END AS Derived_Eligibility_Flag,
CASE WHEN d_459098666='231311385' THEN 'Completed' WHEN d_459098666='615768760' THEN 'Started' END AS Completion_Status,
d_844088537 as Time_Survey_Started
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants`
where d_289750687 is null and (d_459098666='231311385' or d_459098666='615768760')
and date(d_844088537) >= '2022-12-10'
order by d_844088537 desc"

MC_ccc_NE_table <- bq_project_query(project, MC_ccc_NE)
MC_not_elg <- bq_table_download(MC_ccc_NE_table, bigint = "integer64")
MC_not_elg$Connect_ID <- as.numeric(MC_not_elg$Connect_ID)
if (nrow(MC_not_elg) > 0) {
  MC_not_elg <- MC_not_elg %>%
    tag_rule(3,
             "If SrvMC_MenstSrvElig_v1r0 is null, then SrvMC_BaseComplete_v1r0 cannot be 'Started' or 'Submitted'. Excludes surveys started prior to Dec 10, 2022.",
             list("Derived_Eligibility_Flag" = .$Derived_Eligibility_Flag,
                  "Completion_Status"        = .$Completion_Status,
                  "Time_Survey_Started"      = .$Time_Survey_Started))
  all_errors <- bind_rows(all_errors, MC_not_elg)
}


# Rule 4
cash_NORC <- base_vars %>%
  filter(d_130371375_d_266600170_d_731498909==353358909 &
           d_130371375_d_266600170_d_945795905=="cash" &
           !is.na(d_130371375_d_266600170_d_320023644)) %>%
  safe_arrange(Site) %>%
  tag_rule(4,
           "If HDPaym_PaymChosenBL_v2r0 = 'cash' and SMPaym_PaymEligBL_v1r0=yes, then HDPaym_CaseNumberBL_v1r0 should not be populated.",
           list("HDPaym_PaymChosenBL" = .$d_130371375_d_266600170_d_945795905,
                "HDPaym_CaseNumberBL" = .$d_130371375_d_266600170_d_320023644))
all_errors <- bind_rows(all_errors, cash_NORC)


# Rule 5
bday <- partsbq %>%
  filter(!is.na(d_371067537) & d_371067537 != paste0(d_544150384, d_564964481, d_795827569)) %>%
  safe_arrange(Site) %>%
  tag_rule(5,
           "RcrtUP_DOB must equal the concatenation of RcrtUP_YOB, RcrtUP_MOB, and RcrtUP_BD.",
           list("RcrtUP_DOB" = .$d_371067537,
                "RcrtUP_YOB" = .$d_544150384,
                "RcrtUP_MOB" = .$d_564964481,
                "RcrtUP_BD"  = .$d_795827569))
all_errors <- bind_rows(all_errors, bday)


# Rule 6
smmet0 <- partsbq %>%
  filter(d_685002411_d_994064239==104430631 & d_685002411_d_194410742==104430631 & d_685002411_d_949501163==104430631 &
           d_685002411_d_277479354==104430631 & d_685002411_d_867203506==104430631 & d_685002411_d_352996056==104430631 &
           d_685002411_d_217367618==104430631 & d_747006172==104430631 & d_906417725==104430631 & d_773707518==104430631 &
           d_831041022==104430631 & d_987563196==104430631 & d_685002411_d_936015433==104430631 & d_685002411_d_688142378==104430631 &
           d_685002411_d_101763809==104430631 & d_685002411_d_525277409==104430631 & d_685002411_d_671903816==104430631 &
           d_912301837!=208325815) %>%
  safe_arrange(Site) %>%
  tag_rule(6,
           "If all refusal/withdrawal flags = no, then SMMet_PartStatus_v1r0 must = 'No Refusal'.",
           list("SMMet_PartStatus_v1r0" = .$d_912301837))
all_errors <- bind_rows(all_errors, smmet0)


# Rule 7
smmet2 <- partsbq %>%
  filter(d_906417725==353358909 & d_773707518==104430631 & d_747006172==104430631 &
           d_831041022==104430631 & d_987563196==104430631 & d_912301837!=458508122) %>%
  safe_arrange(Site) %>%
  tag_rule(7,
           "If HdWd_Activepart_v1r0=yes and withdrawal flags=no, then SMMet_PartStatus_v1r0 must = 'Refused All Future Activities'.",
           list("SMMet_PartStatus_v1r0" = .$d_912301837))
all_errors <- bind_rows(all_errors, smmet2)


# Rule 8
smmet3 <- partsbq %>%
  filter(d_773707518==353358909 & d_747006172==104430631 & d_831041022==104430631 &
           d_987563196==104430631 & d_912301837!=872012139) %>%
  safe_arrange(Site) %>%
  tag_rule(8,
           "If HdWd_HIPAArevoked_v1r0=yes and other withdrawal flags=no, then SMMet_PartStatus_v1r0 must = 'Revoked HIPAA Only'.",
           list("SMMet_PartStatus_v1r0" = .$d_912301837))
all_errors <- bind_rows(all_errors, smmet3)


# Rule 9
smmet4 <- partsbq %>%
  filter(d_773707518==353358909 & d_747006172==353358909 & d_831041022==104430631 &
           d_987563196==104430631 & d_912301837!=854021266) %>%
  safe_arrange(Site) %>%
  tag_rule(9,
           "If HdWd_HIPAArevoked_v1r0=yes and HdWd_WdConsent_v1r0=yes, then SMMet_PartStatus_v1r0 must = 'Withdrew Consent'.",
           list("SMMet_PartStatus_v1r0" = .$d_912301837))
all_errors <- bind_rows(all_errors, smmet4)


# Rule 10
opt_out <- partsbq %>%
  filter(!is.na(state_d_697256759) & (state_d_158291096==104430631 | is.na(state_d_158291096))) %>%
  safe_arrange(Site) %>%
  tag_rule(10,
           "If RcrtSI_OptOutTm_v1r0 is a valid date, then RcrtSI_OptOut_v1r0 must be 'yes'.",
           list("RcrtSI_OptOut_v1r0" = .$state_d_158291096))
all_errors <- bind_rows(all_errors, opt_out)


# Rule 11
pref_lang <- partsbq %>%
  filter(is.na(d_255077064) & as.Date(d_454445267) > "2024-07-31") %>%
  mutate(RcrtCS_ConsentSumit_v1r0 = as.Date(d_454445267)) %>%
  safe_arrange(Site) %>%
  tag_rule(11,
           "If RcrtCS_ConsentSumit_v1r0 was on or after August 1, 2024 then RcrtCS_PrefLang_v1r0 must be populated.",
           list("RcrtCS_PrefLang_v1r0"     = .$d_255077064,
                "RcrtCS_ConsentSumit_v1r0" = .$RcrtCS_ConsentSumit_v1r0))
all_errors <- bind_rows(all_errors, pref_lang)


log_info("First 25 rules ran")

## Clearing up space in GCP memory
rm(list = setdiff(ls(), 
                  c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", 
                    "base_vars", "safe_arrange", "all_errors", "tag_rule",
                    "make_explanation_cols", "rules_registry", "register_exclusions",
                    "exclusions_registry")))
gc()

# Rule 39 — BigQuery pull
hipaa_match_sql <- "SELECT Connect_ID FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants`
where FORMAT_DATETIME('%Y-%m-%d %H:%M', TIMESTAMP(d_262613359))
   != FORMAT_DATETIME('%Y-%m-%d %H:%M', TIMESTAMP(d_454445267))
AND DATE(d_454445267) > '2023-12-01'"
hipaa_consent_match <- bq_table_download(bq_project_query(project, hipaa_match_sql), bigint="integer64")
hipaa_consent_match$Connect_ID <- as.numeric(hipaa_consent_match$Connect_ID)
if (nrow(hipaa_consent_match) > 0) {
  hipaa_consent_match <- hipaa_consent_match %>%
    left_join(partsbq %>% select(Connect_ID, token, Site, d_262613359, d_454445267), by="Connect_ID") %>%
    tag_rule(39,
             "The HIPAA Authorization timestamp and consent submitted timestamp should match to the minute (after 12/1/2023).",
             list("RcrtCS_HIPAAAuthTm"   = .$d_262613359,
                  "RcrtCS_ConsentSumit"  = .$d_454445267))
  all_errors <- bind_rows(all_errors, hipaa_consent_match)
}


# Birthday card rules 40–45
bdaybq_sql <- "SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.birthdayCard` where Connect_ID IS NOT NULL"
bdaybq <- bq_table_download(bq_project_query(project, bdaybq_sql), bigint="integer64")
bdaybq$Connect_ID <- as.numeric(bdaybq$Connect_ID)
bday_base_vars <- left_join(partsbq, bdaybq, by="Connect_ID")

bday1 <- bday_base_vars %>% filter(!is.na(d_768313785) & is.na(d_194486780)) %>%
  safe_arrange(Site) %>%
  tag_rule(40, "If maildate is populated then cardVersion should be populated.",
           list("NORCBC_CardMailingDt" = .$d_768313785,
                "NORCBC_CardVersion"   = .$d_194486780))
all_errors <- bind_rows(all_errors, bday1)

bday2 <- bday_base_vars %>% filter(is.na(d_768313785) & !is.na(d_194486780)) %>%
  safe_arrange(Site) %>%
  tag_rule(41, "If cardVersion is populated then maildate should be populated.",
           list("NORCBC_CardMailingDt" = .$d_768313785,
                "NORCBC_CardVersion"   = .$d_194486780))
all_errors <- bind_rows(all_errors, bday2)

bday3 <- bday_base_vars %>% 
  filter(!is.na(d_768313785) & !(d_916186376 %in% c("01","02","06","07") | is.na(d_916186376))) %>%
  safe_arrange(Site) %>%
  tag_rule(42, "If maildate is populated then dispCode should be 01, 02, 06, 07, or N/A.",
           list("NORCBC_CardMailingDt" = .$d_768313785,
                "NORCBC_CardDispCode"  = .$d_916186376))
all_errors <- bind_rows(all_errors, bday3)

bday4 <- bday_base_vars %>% filter(is.na(d_768313785) & !is.na(d_916186376)) %>%
  safe_arrange(Site) %>%
  tag_rule(43, "If dispCode is populated then maildate should be populated.",
           list("NORCBC_CardMailingDt" = .$d_768313785,
                "NORCBC_CardDispCode"  = .$d_916186376))
all_errors <- bind_rows(all_errors, bday4)

bday5 <- bday_base_vars %>% 
  filter(!is.na(d_768313785) & !(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?Z$", d_902078073) |
                                   is.na(d_902078073))) %>%
  safe_arrange(Site) %>%
  tag_rule(44, "If maildate is populated then returnDate should be an ISO 8601 timestamp or N/A.",
           list("NORCBC_CardMailingDt" = .$d_768313785,
                "NORCBC_CardReturnDt"  = .$d_902078073))
all_errors <- bind_rows(all_errors, bday5)

bday6 <- bday_base_vars %>% filter(is.na(d_768313785) & !is.na(d_902078073)) %>%
  safe_arrange(Site) %>%
  tag_rule(45, "If returnDate is populated then maildate should be populated.",
           list("NORCBC_CardMailingDt" = .$d_768313785,
                "NORCBC_CardReturnDt"  = .$d_902078073))
all_errors <- bind_rows(all_errors, bday6)


# Rule 46
reinvite_date_type <- partsbq %>%
  filter(!is.na(d_439351436) & is.na(d_280021666)) %>%
  mutate(Verif_Status = case_when(
    d_821247024==875007964 ~ 'Not yet verified', d_821247024==197316935 ~ 'Verified',
    d_821247024==219863910 ~ 'Cannot be verified', d_821247024==922622075 ~ 'Duplicate',
    d_821247024==160161595 ~ 'Outreach timed out')) %>%
  safe_arrange(Site) %>%
  tag_rule(46, "If re-invitation date set, then re-invitation campaign type data should be sent.",
           list("RcrtSI_RInvTimeStamp_v1r0"    = .$d_439351436,
                "RcrtSI_RInvCampaignType_v1r0" = .$d_280021666,
                "D_T_Consented"                = .$d_454445267,
                "D_T_Verified"                 = .$d_914594314,
                "Verif_Status"                 = .$Verif_Status,
                "Date_T_UP_Submitted"          = .$d_430551721))
all_errors <- bind_rows(all_errors, reinvite_date_type)


# Rule 47
reinvite_start <- base_vars %>%
  filter(as.Date(d_439351436) < "2025-02-06") %>%
  safe_arrange(Site) %>%
  tag_rule(47, "There should not be any re-invitation data sent before 2/6/2025.",
           list("RcrtSI_RInvTimeStamp_v1r0"    = .$d_439351436,
                "RcrtSI_RInvCampaignType_v1r0" = .$d_280021666))
all_errors <- bind_rows(all_errors, reinvite_start)


# Rule 48
consent_verif <- partsbq %>%
  filter((d_919254129=="104430631" | is.na(d_919254129)) & d_821247024=="197316935") %>%
  safe_arrange(Site) %>%
  tag_rule(48, "If RcrtCS_Consented_v1r0=no, RcrtV_Verification_v1r0 cannot be 'verified'.",
           list("RcrtCS_Consented_v1r0"    = .$d_919254129,
                "RcrtV_Verification_v1r0"  = .$d_821247024))
all_errors <- bind_rows(all_errors, consent_verif)


# Rule 49
start_DHQ <- partsbq %>%
  filter(!is.na(d_109610692) & (d_692560814==972455046 | d_692560814==789467219 | is.na(d_692560814))) %>%
  safe_arrange(Site) %>%
  tag_rule(49, "If the DHQ survey start timestamp is populated, then the DHQ survey flag must be 'completed' or 'started'.",
           list("SrvDHQ3_StartTimestamp"   = .$d_109610692,
                "SrvDHQ3_6moStatus_v1r0"  = .$d_692560814))
all_errors <- bind_rows(all_errors, start_DHQ)


## Clearing up space in GCP memory
rm(list = setdiff(ls(), 
                  c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", 
                    "base_vars", "safe_arrange", "all_errors", "tag_rule",
                    "make_explanation_cols", "rules_registry", "register_exclusions",
                    "exclusions_registry")))
gc()




# Rule 50
comp_DHQ <- partsbq %>%
  filter(!is.na(d_610227793) & (d_692560814==972455046 | d_692560814==789467219 | is.na(d_692560814))) %>%
  safe_arrange(Site) %>%
  tag_rule(50, "If the DHQ survey completion timestamp is populated, then the DHQ survey flag must be 'completed'.",
           list("SrvDHQ3_CompletionTimestamp" = .$d_610227793,
                "SrvDHQ3_6moStatus_v1r0"     = .$d_692560814))
all_errors <- bind_rows(all_errors, comp_DHQ)


log_info("First 50 rules ran")

# Rule 51
dhq_qualified <- partsbq %>%
  filter((d_692560814==615768760 | d_692560814==231311385) &
           !(d_821247024==197316935 & as.Date(d_914594314) >= "2024-12-01" &
               (d_747006172==104430631 | (as.Date(d_659990606) > as.Date(d_610227793))) &
               difftime(Sys.Date(), as.Date(d_914594314), units="days") >= 180)) %>%
  mutate(verif_days = as.numeric(difftime(Sys.Date(), as.Date(d_914594314), units="days"))) %>%
  safe_arrange(Site) %>%
  tag_rule(51,
           "If SrvDHQ3_6moStatus_v1r0 is started or submitted, participant must be verified >= 12/1/24, not withdrawn, and >= 180 days post-verification.",
           list("SrvDHQ3_6moStatus_v1r0" = .$d_692560814,
                "verif_days"             = .$verif_days,
                "HdWd_WdConsent_v1r0"   = .$d_747006172))
all_errors <- bind_rows(all_errors, dhq_qualified)


# Rule 52
dhq_eligible <- partsbq %>%
  filter(d_692560814==789467219 & !(as.Date(d_914594314) < "2024-12-01")) %>%
  safe_arrange(Site) %>%
  tag_rule(52,
           "If SrvDHQ3_6moStatus_v1r0 = not eligible, participant must be verified before 12/1/24.",
           list("SrvDHQ3_6moStatus_v1r0" = .$d_692560814,
                "RcrtV_VerificationTm"   = .$d_914594314))
all_errors <- bind_rows(all_errors, dhq_eligible)


# Rule 53
dhq_null <- partsbq %>%
  filter(is.na(d_692560814) & d_821247024==197316935 & !(as.Date(d_659990606) < "2025-05-30")) %>%
  safe_arrange(Site) %>%
  tag_rule(53,
           "If SrvDHQ3_6moStatus_v1r0 is null, verification status must not equal verified.",
           list("SrvDHQ3_6moStatus_v1r0"  = .$d_692560814,
                "RcrtV_Verification_v1r0" = .$d_821247024))
all_errors <- bind_rows(all_errors, dhq_null)


# Rule 54
dhq_external_status <- partsbq %>%
  filter((d_692560814==615768760 | d_692560814==231311385) &
           difftime(Sys.time(), as.Date(d_109610692), units="hours") > 24 &
           is.na(d_501613780)) %>%
  mutate(survey_start_hours = as.numeric(difftime(Sys.time(), as.Date(d_109610692), units="hours"))) %>%
  safe_arrange(Site) %>%
  tag_rule(54,
           "If SrvDHQ3_6moStatus_v1r0 is started/submitted and survey started > 24 hrs ago, then external status must be populated.",
           list("SrvDHQ3_6moStatus_v1r0"  = .$d_692560814,
                "survey_start_hours"      = .$survey_start_hours,
                "SrvDHQ3_6moPltStatus"   = .$d_501613780))
all_errors <- bind_rows(all_errors, dhq_external_status)


# Rules 55–58: DHQ credential checks
dhq_uname <- partsbq %>%
  filter(!is.na(d_148184166) & (is.na(d_808755658) | is.na(d_196723965))) %>%
  safe_arrange(Site) %>%
  tag_rule(55, "If DHQ3 Username is populated, then UUID and Study ID must also be populated.",
           list("DHQ3_Username" = .$d_148184166,
                "DHQ3_UUID"    = .$d_808755658,
                "DHQ3_StudyID" = .$d_196723965))
all_errors <- bind_rows(all_errors, dhq_uname)

dhq_uuid <- partsbq %>%
  filter(!is.na(d_808755658) & (is.na(d_148184166) | is.na(d_196723965))) %>%
  safe_arrange(Site) %>%
  tag_rule(56, "If DHQ3 UUID is populated, then Username and Study ID must also be populated.",
           list("DHQ3_Username" = .$d_148184166,
                "DHQ3_UUID"    = .$d_808755658,
                "DHQ3_StudyID" = .$d_196723965))
all_errors <- bind_rows(all_errors, dhq_uuid)

dhq_studyid <- partsbq %>%
  filter(!is.na(d_196723965) & (is.na(d_148184166) | is.na(d_808755658))) %>%
  safe_arrange(Site) %>%
  tag_rule(57, "If DHQ3 Study ID is populated, then Username and UUID must also be populated.",
           list("DHQ3_Username" = .$d_148184166,
                "DHQ3_UUID"    = .$d_808755658,
                "DHQ3_StudyID" = .$d_196723965))
all_errors <- bind_rows(all_errors, dhq_studyid)

dhq_system_null <- partsbq %>%
  filter((is.na(d_692560814) | d_692560814==789467219) &
           !(is.na(d_148184166) & is.na(d_808755658) & is.na(d_196723965))) %>%
  safe_arrange(Site) %>%
  tag_rule(58,
           "If SrvDHQ3_6moStatus_v1r0 is not yet eligible or null, then DHQ3 Username, UUID, and Study ID must all be null.",
           list("SrvDHQ3_6moStatus_v1r0" = .$d_692560814,
                "DHQ3_Username"          = .$d_148184166,
                "DHQ3_UUID"              = .$d_808755658,
                "DHQ3_StudyID"           = .$d_196723965))
all_errors <- bind_rows(all_errors, dhq_system_null)


# Rule 59
register_exclusions(59, c(6213377542, 6891536539, 1514220001, 8583917079))

smmet <- partsbq %>%
  filter(d_100767870==353358909 &
           (d_949302066!=231311385 | d_536735468!=231311385 | d_976570371!=231311385 | d_663265240!=231311385) &
           !(Connect_ID %in% c(6213377542, 6891536539, 1514220001, 8583917079))) %>%
  safe_arrange(Site) %>%
  tag_rule(59, "If SMMet_BaseSrvCompl_v1r0=yes, then all four baseline survey flags must be 'submitted'.",
           list("SrvBOH_BaseStatus_v1r0" = .$d_949302066,
                "SrvMRE_BaseStatus_v1r0" = .$d_536735468,
                "SrvSAS_BaseStatus_v1r0" = .$d_976570371,
                "SrvLAW_BaseStatus_v1r0" = .$d_663265240))
all_errors <- bind_rows(all_errors, smmet)


# Rule 60
reinv_active <- partsbq %>%
  filter(!is.na(d_439351436) & d_512820379 != '486306141') %>%
  safe_arrange(Site) %>%
  tag_rule(60, "If RcrtSI_RInvTimeStamp_v1r0 is populated, then RcrtSI_RecruitType_v1r0 must be 'active'.",
           list("RcrtSI_RInvTimeStamp_v1r0" = .$d_439351436,
                "RcrtSI_RecruitType_v1r0"   = .$d_512820379))
all_errors <- bind_rows(all_errors, reinv_active)


# Rule 62
outreach_ver <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           Site!="HFH" & as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_188797763 %in% c('353358909','104430631'))) %>%
  safe_arrange(Site) %>%
  tag_rule(62, "If verified and Active/Passive, Outreach required for Verification must be 'Yes' or 'No'. 5-day lag. Excludes HFH.",
           list("RcrtSI_RecruitType_v1r0"     = .$d_512820379,
                "RcrtV_VerificationTm_V1R0"   = .$d_914594314,
                "Outreach_Req_for_Verif"       = .$state_d_188797763))
all_errors <- bind_rows(all_errors, outreach_ver)


# Rule 65
fname_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_147176963 %in% c('356674370','219803804'))) %>%
  safe_arrange(Site) %>%
  tag_rule(65, "If verified and Active/Passive, First Name Match must be 'Not Matched' or 'Matched'. 5-day lag.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "First_Name_Match"           = .$state_d_147176963))
all_errors <- bind_rows(all_errors, fname_match)


# Rule 66
lname_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_557461333 %in% c('356674370','219803804'))) %>%
  safe_arrange(Site) %>%
  tag_rule(66, "If verified and Active/Passive, Last Name Match must be 'Not Matched' or 'Matched'. 5-day lag.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "Last_Name_Match"            = .$state_d_557461333))
all_errors <- bind_rows(all_errors, lname_match)


# Rule 67
dob_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_725929722 %in% c('356674370','219803804'))) %>%
  safe_arrange(Site) %>%
  tag_rule(67, "If verified and Active/Passive, DOB Match must be 'Not Matched' or 'Matched'. 5-day lag.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "DOB_Match"                  = .$state_d_725929722))
all_errors <- bind_rows(all_errors, dob_match)


# Rule 68
pin_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           !(Site %in% c("HP","MF")) &
           !(Site=="BSWH" & is.na(state_d_711794630)) &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_711794630 %in% c('356674370','219803804'))) %>%
  safe_arrange(Site) %>%
  tag_rule(68, "If verified and Active/Passive, PIN Match must be 'Not Matched' or 'Matched'. 5-day lag. Excludes HP and MF.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "PIN_Match"                  = .$state_d_711794630))
all_errors <- bind_rows(all_errors, pin_match)


# Rule 69
token_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           !(Site %in% c("SF","MF")) &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_679832994 %in% c('356674370','219803804'))) %>%
  safe_arrange(Site) %>%
  tag_rule(69, "If verified and Active/Passive, Token Match must be 'Not Matched' or 'Matched'. 5-day lag. Excludes SF and MF.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "Token_Match"                = .$state_d_679832994))
all_errors <- bind_rows(all_errors, token_match)


# Rule 71
site_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_570452130 %in% c('539025306','427405444'))) %>%
  safe_arrange(Site) %>%
  tag_rule(71, "If verified and Active/Passive, Site Match must be 'Criterium not met' or 'Criterium met'. 5-day lag.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "Site_Match"                 = .$state_d_570452130))
all_errors <- bind_rows(all_errors, site_match)


# Rule 72
age_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_629484663 %in% c('539025306','427405444'))) %>%
  safe_arrange(Site) %>%
  tag_rule(72, "If verified and Active/Passive, Age Match must be 'Criterium not met' or 'Criterium met'. 5-day lag.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "Age_Match"                  = .$state_d_629484663))
all_errors <- bind_rows(all_errors, age_match)


# Rule 73
cancer_match <- partsbq %>%
  filter(d_821247024=='197316935' & d_512820379 %in% c('486306141','854703046') &
           as.Date(d_914594314) < (currentDate - days(5)) &
           !(state_d_547895941 %in% c('539025306','427405444'))) %>%
  safe_arrange(Site) %>%
  tag_rule(73, "If verified and Active/Passive, Cancer Status Match must be 'Criterium not met' or 'Criterium met'. 5-day lag.",
           list("RcrtSI_RecruitType_v1r0"   = .$d_512820379,
                "RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "Cancer_Status_Match"        = .$state_d_547895941))
all_errors <- bind_rows(all_errors, cancer_match)


# Rules 74–85: active recruit DI fields — shared exclusion pattern
register_exclusions(74, c(4093473296,2701575745))

act_di_age <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_934298480) &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           !(Connect_ID %in% c("4093473296","2701575745"))) %>%
  safe_arrange(Site) %>%
  tag_rule(74, "If RcrtSI_RecruitType='Active' then RcrtSI_Age_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_Age_v1r0"          = .$state_d_934298480))
all_errors <- bind_rows(all_errors, act_di_age)


register_exclusions(75, 4093473296)

act_di_race <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_849518448) &
           !d_827220437 %in% c('657167265','548392715','472940358') &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           Connect_ID!="4093473296") %>%
  safe_arrange(Site) %>%
  tag_rule(75, "If Active and not SF/HFH/BSWH, RcrtSI_Race_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_Race_v1r0"         = .$state_d_849518448))
all_errors <- bind_rows(all_errors, act_di_race)

act_di_SF_race <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_119643471) &
           d_827220437=='657167265' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954")) %>%
  safe_arrange(Site) %>%
  tag_rule(76, "If Active and Site=SF, RcrtSI_SHRace_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_SHRace_v1r0"       = .$state_d_119643471))
all_errors <- bind_rows(all_errors, act_di_SF_race)

act_di_HFH_race <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_684926335) &
           d_827220437=='548392715' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954")) %>%
  safe_arrange(Site) %>%
  tag_rule(77, "If Active and Site=HFH, RcrtSI_HFHSRace_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_HFHSRace_v1r0"    = .$state_d_684926335))
all_errors <- bind_rows(all_errors, act_di_HFH_race)


register_exclusions(78, 2701575745)

act_di_BSWH_race <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_253532712) &
           d_827220437=='472940358' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           !(Connect_ID %in% c("2701575745"))) %>%
  safe_arrange(Site) %>%
  tag_rule(78, "If Active and Site=BSWH, RcrtSI_BSWHRaceEth_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_BSWHRaceEth_v1r0" = .$state_d_253532712))
all_errors <- bind_rows(all_errors, act_di_BSWH_race)

act_di_SF_eth <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_538553381) &
           d_827220437=='657167265' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954")) %>%
  safe_arrange(Site) %>%
  tag_rule(79, "If Active and Site=SF, RcrtSI_SHEthnicity must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_SHEthnicity_v1r0" = .$state_d_538553381))
all_errors <- bind_rows(all_errors, act_di_SF_eth)

act_di_HFH_eth <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_527823810) &
           d_827220437=='548392715' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954")) %>%
  safe_arrange(Site) %>%
  tag_rule(80, "If Active and Site=HFH, RcrtSI_HFHSEthnicity_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"    = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0"   = .$state_d_793822265,
                "RcrtSI_HFHSEthnicity_v1r0" = .$state_d_527823810))
all_errors <- bind_rows(all_errors, act_di_HFH_eth)


register_exclusions(81, c(4093473296, 2701575745))

act_di_member <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_477091792) &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           !(Connect_ID %in% c("4093473296","2701575745"))) %>%
  safe_arrange(Site) %>%
  tag_rule(81, "If Active, RcrtSI_MemberStat_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_MemberStat_v1r0"  = .$state_d_477091792))
all_errors <- bind_rows(all_errors, act_di_member)


register_exclusions(82, c(4093473296, 2701575745))

act_di_cmpn <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_667474224) &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           !(Connect_ID %in% c("4093473296","2701575745"))) %>%
  safe_arrange(Site) %>%
  tag_rule(82, "If Active, RcrtSI_CampaignType_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"   = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0"  = .$state_d_793822265,
                "RcrtSI_CampaignType_v1r0" = .$state_d_667474224))
all_errors <- bind_rows(all_errors, act_di_cmpn)


register_exclusions(83, c(4093473296, 2701575745))

act_di_elg <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_749475364) &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           !(Connect_ID %in% c("4093473296","2701575745"))) %>%
  safe_arrange(Site) %>%
  tag_rule(83, "If Active, RcrtSI_EligibilityVer_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"    = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0"   = .$state_d_793822265,
                "RcrtSI_EligibilityVer_v1r0" = .$state_d_749475364))
all_errors <- bind_rows(all_errors, act_di_elg)


register_exclusions(84, c(4093473296, 2701575745))

act_di_sex <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_706256705) &
           d_827220437 != '548392715' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954") &
           !(Connect_ID %in% c("4093473296","2701575745"))) %>%
  safe_arrange(Site) %>%
  tag_rule(84, "If Active and not HFH, RcrtSI_Sex_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_Sex_v1r0"          = .$state_d_706256705))
all_errors <- bind_rows(all_errors, act_di_sex)

act_di_HF_sex <- partsbq %>%
  filter(d_512820379=='486306141' & is.na(state_d_435027713) &
           d_827220437=='548392715' &
           !(d_821247024=="922622075" & state_d_793822265=="854903954")) %>%
  safe_arrange(Site) %>%
  tag_rule(85, "If Active and Site=HFH, RcrtSI_HFHS_Sex_v1r0 must be populated.",
           list("RcrtV_Verification_v1r0"  = .$d_821247024,
                "RcrtV_UpdateRecType_v1r0" = .$state_d_793822265,
                "RcrtSI_HFHS_Sex_v1r0"    = .$state_d_435027713))
all_errors <- bind_rows(all_errors, act_di_HF_sex)


# Rule 86 — BigQuery pull
bq_cv_lang <- "SELECT
CASE d_827220437
  when '531629870' THEN 'HP'   when '548392715' THEN 'HFH'
  when '125001209' THEN 'KPCO' when '327912200' THEN 'KPGA'
  when '300267574' THEN 'KPHI' when '452412599' THEN 'KPNW'
  when '303349821' THEN 'MF'   when '657167265' THEN 'SF'
  when '809703864' THEN 'UC'   when '472940358' THEN 'BSWH'
end as Site,
covid.Connect_ID, pts.d_268176409 as CV_StartTimestamp, covid.d_784119588 as CV_LanguageVar
FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants` pts
left join `nih-nci-dceg-connect-prod-6d04.FlatConnect.covid19Survey_v1` covid
on pts.Connect_ID=covid.Connect_ID
where covid.Connect_ID is not null
and DATE(pts.d_268176409) >= '2023-07-05' and covid.d_784119588 is null"
covid_lang <- bq_table_download(bq_project_query(project, bq_cv_lang), bigint="integer64")
covid_lang$Connect_ID <- as.numeric(covid_lang$Connect_ID)
if (nrow(covid_lang) > 0) {
  covid_lang <- covid_lang %>%
    left_join(partsbq %>% select(Connect_ID, token), by="Connect_ID") %>%
    tag_rule(86, "If COVID-19 Survey started >= 7/5/23, then COVID-19 Survey Language variable must be populated.",
             list("SrvCV_StartTimestamp" = .$CV_StartTimestamp,
                  "SrvCV_LanguageVar"   = .$CV_LanguageVar))
  all_errors <- bind_rows(all_errors, covid_lang)
}


# Rule 87
AB1 <- partsbq %>%
  mutate(AB_response = case_when(
    state_d_956485028=='562663942' ~ 'Altruism Personal',
    state_d_956485028=='686986259' ~ 'Altruism General',
    state_d_956485028=='477331464' ~ 'Cancer Personal',
    state_d_956485028=='935486262' ~ 'Cancer General',
    state_d_956485028=='518814501' ~ 'Research Personal',
    state_d_956485028=='307763550' ~ 'Research General'),
    UTM_camp = gsub("-", " ", d_163847117)) %>%
  filter(!is.na(AB_response) & tolower(AB_response) != tolower(UTM_camp)) %>%
  safe_arrange(Site) %>%
  tag_rule(87, "If A/B Testing Message Response is populated, UTM_Campaign must match.",
           list("AB_response" = .$AB_response,
                "UTM_camp"    = .$UTM_camp))
all_errors <- bind_rows(all_errors, AB1)


# DEPRECATED Rule 88 -- "If A/B Testing Message Response is populated and any UTM parameter is populated, all three must be present.",


# Rule 89
Cnsnt <- partsbq %>%
  filter(is.na(d_454445267) & d_919254129=='353358909') %>%
  safe_arrange(Site) %>%
  tag_rule(89, "If RcrtCS_ConsentSumit_v1r0 does not exist then RcrtCS_Consented_v1r0 must = no or NA.",
           list("RcrtCS_ConsentSumit_v1r0" = .$d_454445267,
                "RcrtCS_Consented_v1r0"    = .$d_919254129))
all_errors <- bind_rows(all_errors, Cnsnt)


# Rule 90
UP_Sub_Time90 <- partsbq %>%
  filter(is.na(d_430551721) & d_831041022=='104430631' & d_699625233=='353358909') %>%
  safe_arrange(Site) %>%
  tag_rule(90, "If RcrtUP_SubmitTime_v1r0 does not exist and HdWd_Destroydata=no, then RcrtUP_Submitted_v1r0 must = no or NA.",
           list("RcrtUP_SubmitTime_v1r0" = .$d_430551721,
                "RcrtUP_Submitted_v1r0"  = .$d_699625233))
all_errors <- bind_rows(all_errors, UP_Sub_Time90)


# Rule 91
UP_Sub_Time91 <- partsbq %>%
  filter(d_699625233=='353358909' & d_831041022=='104430631' &
           (d_919254129=='104430631' | is.na(d_919254129))) %>%
  safe_arrange(Site) %>%
  tag_rule(91, "If RcrtUP_Submitted_v1r0=yes and HdWd_Destroydata=no, then RcrtCS_Consented_v1r0 must be yes.",
           list("RcrtUP_Submitted_v1r0" = .$d_699625233,
                "RcrtCS_Consented_v1r0" = .$d_919254129))
all_errors <- bind_rows(all_errors, UP_Sub_Time91)


# Rule 92
SignIn_Time <- partsbq %>%
  filter(is.na(d_335767902) & d_831041022=='104430631' & d_230663853=='353358909') %>%
  safe_arrange(Site) %>%
  tag_rule(92, "If RcrtSI_SignTime_v1r0 does not exist and HdWd_Destroydata=no, then RcrtSI_SignedIn_v1r0 must = no or NA.",
           list("RcrtSI_SignTime_v1r0"  = .$d_335767902,
                "RcrtSI_SignedIn_v1r0"  = .$d_230663853))
all_errors <- bind_rows(all_errors, SignIn_Time)


# Rule 93
Verification_Time <- partsbq %>%
  filter(is.na(d_914594314) & d_831041022=='104430631' & d_821247024 != '875007964') %>%
  safe_arrange(Site) %>%
  tag_rule(93, "If RcrtV_VerificationTm_V1R0 does not exist and HdWd_Destroydata=no, then RcrtV_Verification_v1r0 must = not yet verified or NA.",
           list("RcrtV_VerificationTm_V1R0" = .$d_914594314,
                "RcrtV_Verification_v1r0"   = .$d_821247024))
all_errors <- bind_rows(all_errors, Verification_Time)


# Rule 94
Man_OutRch <- partsbq %>%
  filter((is.na(state_d_953614051) | state_d_953614051=='734437214') & state_d_188797763=='353358909') %>%
  safe_arrange(Site) %>%
  tag_rule(94, "If RcrtV_Mannual_v1r0 = method not used or NA, then RcrtV_Outreach_v1r0 must be no or NA.",
           list("RcrtV_Mannual_v1r0"  = .$state_d_953614051,
                "RcrtV_Outreach_v1r0" = .$state_d_188797763))
all_errors <- bind_rows(all_errors, Man_OutRch)



# Rule 96
update_recr <- base_vars %>%
  filter(d_821247024==197316935 & is.na(state_d_793822265) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(96, "If verified, update recruit type should be sent. 5-day lag.",
           list("RcrtV_VerificationTm"  = .$d_914594314,
                "RcrtV_UpdateRecType"   = .$state_d_793822265))
all_errors <- bind_rows(all_errors, update_recr)




# Rule 98
update_recr_by_type <- base_vars %>%
  filter(d_821247024 != "922622075" & !is.na(state_d_148197146) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(98, "If verification != duplicate, then Duplicate type should be NA. 5-day lag.",
           list("RcrtV_Verification_v1r0" = .$d_821247024,
                "RcrtV_DuplicateType"     = .$state_d_148197146))
all_errors <- bind_rows(all_errors, update_recr_by_type)


# Rule 99
Outreach_manual <- base_vars %>%
  filter(state_d_188797763==353358909 &
           (state_d_953614051==734437214 | is.na(state_d_953614051)) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(99, "If RcrtV_Outreach=yes then RcrtV_Mannual must = method used. 5-day lag.",
           list("RcrtV_Outreach_v1r0" = .$state_d_188797763,
                "RcrtV_Mannual_v1r0"  = .$state_d_953614051))
all_errors <- bind_rows(all_errors, Outreach_manual)



## Clearing up space in GCP memory
rm(list = setdiff(ls(), c('currentDate', 'boxfolder', 'project', "partsbq", "biobq", 
                          "base_vars", "safe_arrange", "all_errors", "tag_rule",
                          "make_explanation_cols", "rules_registry", "register_exclusions",
                          "exclusions_registry")))
gc()



# Rule 100
Auto_verif <- base_vars %>%
  filter(d_821247024==197316935 & state_d_953614051==734437214 &
           (state_d_444699761==734437214 | is.na(state_d_444699761)) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(100, "If verified and Manual Verif=method not used, then Auto Verif must be 'method used'. 5-day lag.",
           list("RcrtV_Manual_v1r0"    = .$state_d_953614051,
                "RcrtV_Automated_v1r0" = .$state_d_444699761))
all_errors <- bind_rows(all_errors, Auto_verif)


# Rule 101
Man_Auto_verif <- base_vars %>%
  filter(d_821247024==197316935 & state_d_444699761==734437214 &
           (state_d_953614051==734437214 | is.na(state_d_953614051)) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(101, "If verified and Auto Verif=method not used, then Manual Verif must be 'method used'. 5-day lag.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, Man_Auto_verif)


# Rule 102
Dupl_type <- base_vars %>%
  filter(d_821247024==922622075 & is.na(state_d_148197146) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(102, "If verification status = duplicate, there should be a duplicate type. 5-day lag.",
           list("RcrtV_Verification_v1r0" = .$d_821247024,
                "RcrtV_DuplicateType"     = .$state_d_148197146))
all_errors <- bind_rows(all_errors, Dupl_type)


# Rule 103
Dupl_type_reverse <- base_vars %>%
  filter(d_821247024 != 922622075 & !is.na(state_d_148197146) &
           as.Date(d_914594314) < (currentDate - days(5))) %>%
  safe_arrange(Site) %>%
  tag_rule(103, "If there's a duplicate type, verification status must = 'duplicate'. 5-day lag.",
           list("RcrtV_Verification_v1r0" = .$d_821247024,
                "RcrtV_DuplicateType"     = .$state_d_148197146))
all_errors <- bind_rows(all_errors, Dupl_type_reverse)


# Rule 104
KP_ver3 <- base_vars %>%
  filter(d_821247024==197316935 & grepl("KP", Site) &
           as.Date(d_914594314) < (currentDate - days(7)) &
           (is.na(state_d_444699761) | is.na(state_d_188797763) | is.na(state_d_953614051))) %>%
  safe_arrange(Site) %>%
  tag_rule(104, "If site=KP and verified > 1 week ago, all three verification modes must be populated.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Outreach_v1r0"  = .$state_d_188797763,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, KP_ver3)


# Rule 105
HP_UC_BSWH3 <- base_vars %>%
  filter(d_821247024 %in% c(197316935, 219863910) &
           Site %in% c("HP","UC","BSWH") &
           as.Date(d_914594314) < (currentDate - days(5)) &
           (is.na(state_d_444699761) | is.na(state_d_188797763) | is.na(state_d_953614051))) %>%
  safe_arrange(Site) %>%
  tag_rule(105, "If site=HP/UC/BSWH and verified/cannot-be-verified > 5 days ago, all three verification modes must be populated.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Outreach_v1r0"  = .$state_d_188797763,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, HP_UC_BSWH3)


# Rule 106
HFH_ver2 <- base_vars %>%
  filter(d_821247024==197316935 & Site=="HFH" &
           as.Date(d_914594314) < (currentDate - days(5)) &
           (is.na(state_d_444699761) | is.na(state_d_953614051))) %>%
  safe_arrange(Site) %>%
  tag_rule(106, "If site=HFH and verified, automated and manual verification must be populated. 5-day lag.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, HFH_ver2)


# Rule 107
HFH_ver_OR <- base_vars %>%
  filter(d_821247024==197316935 & Site=="HFH" &
           as.Date(d_914594314) < (currentDate - days(5)) &
           state_d_444699761=="426360242" & state_d_953614051=="734437214" &
           !is.na(state_d_188797763)) %>%
  safe_arrange(Site) %>%
  tag_rule(107, "If site=HFH, verified, auto=method used, manual=method not used, then outreach required should be missing. 5-day lag.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Outreach_v1r0"  = .$state_d_188797763,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, HFH_ver_OR)


# Rule 108
HFH_not_ver3 <- base_vars %>%
  filter(d_821247024==219863910 & Site=="HFH" &
           as.Date(d_914594314) < (currentDate - days(5)) &
           (is.na(state_d_444699761) | is.na(state_d_188797763) | is.na(state_d_953614051))) %>%
  safe_arrange(Site) %>%
  tag_rule(108, "If site=HFH and cannot-be-verified, all three verification modes must be populated. 5-day lag.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Outreach_v1r0"  = .$state_d_188797763,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, HFH_not_ver3)


# Rule 109
SF_MF_ver_act <- base_vars %>%
  filter(d_821247024 %in% c(197316935, 219863910) &
           Site %in% c("SF","MF") & d_512820379=="486306141" &
           state_d_793822265=="132080040" &
           as.Date(d_914594314) < (currentDate - days(5)) &
           as.Date(d_914594314) >= "2026-05-01" &
           !(state_d_444699761=="426360242" & !is.na(state_d_188797763) & !is.na(state_d_953614051))) %>%
  safe_arrange(Site) %>%
  tag_rule(109,
           "If site=SF/MF, verified/cannot-be-verified, active, no-change-needed: auto=method used, manual and outreach must be populated. Verified >= 5/1/2026.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Outreach_v1r0"  = .$state_d_188797763,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, SF_MF_ver_act)


# Rule 110
SF_MF_ver_pass <- base_vars %>%
  filter(d_821247024 %in% c(197316935, 219863910) &
           Site %in% c("SF","MF") & d_512820379=="854703046" &
           state_d_793822265=="132080040" &
           as.Date(d_914594314) < (currentDate - days(5)) &
           as.Date(d_914594314) > "2026-05-01" &
           !(state_d_444699761=="734437214" & !is.na(state_d_188797763) & !is.na(state_d_953614051))) %>%
  safe_arrange(Site) %>%
  tag_rule(110,
           "If site=SF/MF, verified/cannot-be-verified, passive, no-change-needed: auto=method not used, manual and outreach must be populated. Verified >= 5/1/2026.",
           list("RcrtV_Automated_v1r0" = .$state_d_444699761,
                "RcrtV_Outreach_v1r0"  = .$state_d_188797763,
                "RcrtV_Manual_v1r0"    = .$state_d_953614051))
all_errors <- bind_rows(all_errors, SF_MF_ver_pass)





# Rule 111
Non_act_verif <- base_vars %>%
  # If recruit type is not active
  filter(d_512820379==180583933 &
           # we shouldn't see any other verif status other then 'not yet verified' or 
           # 'no longer enrolled' OR
            # 'duplicate' if Duplicate Type = 'Not active signed in as Passive Recruit' or 
                                              # 'Not active enrolled as an Active Recruit'
           !(d_821247024==875007964 | d_821247024==290379732 |
               (d_821247024==922622075 & state_d_148197146 %in% c(283434980, 866029623))
             )
         ) %>%
  safe_arrange(Site) %>%
  tag_rule(111, "If Recruit Type is 'Not active', then Verif Status should be 'Not yet verified' OR 'Duplicate' IF Duplicate Type = 'Not active signed in as Passive Recruit' or 'Not active enrolled as an Active Recruit'.",
           list("RcrtV_Verification_v1r0" = .$d_821247024,
                "RcrtV_DuplicateType"     = .$state_d_148197146))
all_errors <- bind_rows(all_errors, Non_act_verif)




## Need to save the raw variables for the Analytics tab

all_errors_raw <- all_errors



## Human Readable: concept ids converted to variable value for the Operations tab

## There is not a straight forward way to do this in BQ, so leaving it as R code
all_errors <- all_errors %>%
  mutate(across(everything(), ~ case_when(
    . == 353358909 ~ "Yes",
    . == 104430631 ~ "No",
    . == 972455046 ~ "Not Started",
    . == 615768760 ~ "Started",
    . == 231311385 ~ "Submitted",
    . == 875007964 ~ "Not yet verified",
    . == 197316935 ~ "Verified",
    . == 219863910 ~ "Cannot be verified",
    . == 922622075 ~ "Duplicate",
    . == 160161595 ~ "Outreach timed out",
    . == 180583933 ~ "Not active",
    . == 486306141 ~ "Active",
    . == 854703046 ~ "Passive",
    . == 734437214 ~ "Method not used",
    . == 426360242 ~ "Method used",
    . == 638335430 ~ "Active recruit signed in as Passive recruit",
    . == 283434980 ~ "Not Active recruit signed in as Passive recruit",
    . == 866029623 ~ "Not Active recruit signed in as an Active recruit",
    . == 654558118 ~ "Participant already enrolled",
    . == 979256174 ~ "Passive recruit signed in as Active recruit",
    . == 132080040 ~ "No change needed",
    . == 604663208 ~ "Not Active to Passive",
    . == 854903954 ~ "Passive to Active",
    . == 965707001 ~ "Active to Passive",
    . == 356674370 ~ "Not matched",
    . == 219803804 ~ "Matched",
    . == 472940358 ~ "Baylor Scott and White Health",
    . == 531629870 ~ "HealthPartners",
    . == 548392715 ~ "Henry Ford Health",
    . == 303349821 ~ "Marshfield Clinic Health System",
    . == 657167265 ~ "Sanford Health",
    . == 809703864 ~ "University of Chicago Medicine",
    . == 125001209 ~ "Kaiser Permanente Colorado",
    . == 327912200 ~ "Kaiser Permanente Georgia",
    . == 300267574 ~ "Kaiser Permanente Hawaii",
    . == 452412599 ~ "Kaiser Permanente Northwest",
    TRUE ~ as.character(.)
  )))





## ---- OUTPUT --------------------------------------------------------------

boxfolder <- "194196493018"


#### Operations report tab 
# Re-enforce consistent column ordering
fixed_cols <- c("Connect_ID", "token", "Site", "rule_id", "rule_label")


# Add any missing fixed cols as NA (e.g. token missing from some BQ-only rules)
for (col in fixed_cols) {
  if (!col %in% names(all_errors)) all_errors[[col]] <- NA
}


# Explanation column pairing for column ordering
n_pairs <- max(as.integer(gsub("explanation_variable_value", "", 
                               grep("^explanation_variable_value", names(all_errors), value = TRUE))),
               na.rm = TRUE)

expl_cols <- c()
for (i in seq_len(n_pairs)) {
  var_col <- paste0("explanation_variable", i)
  val_col <- paste0("explanation_variable_value", i)
  # Drop null explanation pair columns
  if (val_col %in% names(all_errors) && any(!is.na(all_errors[[val_col]]))) {
    expl_cols <- c(expl_cols, var_col, val_col)
  }
}


# Final QC output
all_errors <- all_errors[, c(fixed_cols, expl_cols)]





#### Rules Reference tab
rules_tab <- bind_rows(rules_registry) %>%
  distinct(rule_id, rule_label) %>%
  arrange(rule_id)


#### Analytics Report tab
analytics_report <- all_errors %>%
  left_join(partsbq %>% select(token, Site_CID = d_827220437), by = "token") %>%
  mutate(
    #explanation_variable1_CID       = all_errors_raw$explanation_variable1,
    explanation_variable_value1_CID = all_errors_raw$explanation_variable_value1,
    #explanation_variable2_CID       = all_errors_raw$explanation_variable2,
    explanation_variable_value2_CID = all_errors_raw$explanation_variable_value2,
    #explanation_variable3_CID       = all_errors_raw$explanation_variable3,
    explanation_variable_value3_CID = all_errors_raw$explanation_variable_value3
  ) %>%
  select(any_of(c(
    "Connect_ID", "token", "Site", "Site_CID",
    "rule_id", "rule_label",
    "explanation_variable1", #"explanation_variable1_CID",
    "explanation_variable_value1", "explanation_variable_value1_CID",
    "explanation_variable2", #"explanation_variable2_CID",
    "explanation_variable_value2", "explanation_variable_value2_CID",
    "explanation_variable3", #"explanation_variable3_CID",
    "explanation_variable_value3", "explanation_variable_value3_CID"
  )))


#### Exclusions tab
exclusions_tab <- bind_rows(exclusions_registry) %>%
  mutate(Connect_ID = as.character(Connect_ID)) %>%
  left_join(partsbq %>% select(Connect_ID, token) %>% mutate(Connect_ID = as.character(Connect_ID)), by = "Connect_ID") %>%
  arrange(rule_id)



# Output file
wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Ops_report")
openxlsx::writeData(wb, "Ops_report", all_errors, na.string = "")


openxlsx::addWorksheet(wb, "Analytics_Report")
openxlsx::writeData(wb, "Analytics_Report", analytics_report, na.string = "")


openxlsx::addWorksheet(wb, "Exclusions")
openxlsx::writeData(wb, "Exclusions", exclusions_tab, na.string = "")


openxlsx::addWorksheet(wb, "Rules")
openxlsx::writeData(wb, "Rules", rules_tab)

openxlsx::saveWorkbook(
  wb,
  glue("Recruitment_Custom_QC_Output_{currentDate}_boxfolder_{boxfolder}.xlsx"),
  overwrite = TRUE
)


cat(glue("Total error rows: {nrow(all_errors)}\n"))
cat(glue("Rules with errors: {paste(sort(unique(all_errors$rule_id)), collapse=', ')}\n"))
log_info("Code finished!")
