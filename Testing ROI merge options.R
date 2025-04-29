```{r merge2, include=FALSE}

recr_m2 <- bq_project_query(project, query="SELECT token,Connect_ID, d_821247024, d_914594314,  d_827220437,d_512820379, 
                            d_536735468 , d_517311251  FROM  `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP` WHERE  d_821247024='197316935'")
recr_m2 <- bq_table_download(recr_m2,bigint = "integer64",n_max = Inf, page_size = 10000)
cnames <- names(recr_m2)
# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)
# to check variables in recr_noinact_wl1
for (i in 1: length(cnames)){
  varname <- cnames[i]
  var<-pull(recr_m2,varname)
  recr_m2[,cnames[i]] <- ifelse(numbers_only(var), as.numeric(as.character(var)), var)
}



### Deal with Mod2 duplicates (took both version 1 and vesion 2, and only select version 2 data)
sql_M2_1 <- bq_project_query(project, query = "SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module2_v1_JP` WHERE Connect_ID IS NOT NULL")
sql_M2_2 <- bq_project_query(project, query = "SELECT * FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.module2_v2_JP` WHERE Connect_ID IS NOT NULL")

M2_V1 <- bq_table_download(sql_M2_1, bigint = "integer64")
M2_V2 <- bq_table_download(sql_M2_2, bigint = "integer64")

# Select matching column names
M2_V1_vars <- colnames(M2_V1)
M2_V2_vars <- colnames(M2_V2)
common_vars <- intersect(M2_V1_vars, M2_V2_vars)

# Subset to common columns
M2_V1_common <- M2_V1[, common_vars]
M2_V2_common <- M2_V2[, common_vars]

# Add version indicator
M2_V1_common$version <- 1
M2_V2_common$version <- 2

# Identify columns with mismatched types
mismatched_cols <- names(M2_V1_common)[sapply(names(M2_V1_common), function(col) {
  class(M2_V1_common[[col]]) != class(M2_V2_common[[col]])
})]

# Convert mismatched columns to character for consistency
M2_V1_common <- M2_V1_common %>%
  mutate(across(all_of(mismatched_cols), as.character))
M2_V2_common <- M2_V2_common %>%
  mutate(across(all_of(mismatched_cols), as.character))

# Combine both versions for participants who completed both
M2_common <- bind_rows(M2_V1_common, M2_V2_common) %>%
  arrange(Connect_ID, desc(version))

# For columns unique to each version
V1_only_vars <- setdiff(M2_V1_vars, common_vars)
V2_only_vars <- setdiff(M2_V2_vars, common_vars)

# Subset each version for unique columns and add version indicator
m2_v1_only <- M2_V1[, c("Connect_ID", V1_only_vars)] %>%
  mutate(version = 1)
m2_v2_only <- M2_V2[, c("Connect_ID", V2_only_vars)] %>%
  mutate(version = 2)

# Combine the unique and common data
M2_common_v1 <- left_join(M2_common, m2_v1_only, by = c("Connect_ID", "version"))
M2_combined_v1v2 <- left_join(M2_common_v1, m2_v2_only, by = c("Connect_ID", "version"))

# Filter for complete cases where specific completion criteria are met
M2_complete <- M2_combined_v1v2 %>%
  filter(Connect_ID %in% recr_m2$Connect_ID[recr_m2$d_536735468 == 231311385]) %>%
  arrange(desc(version))

# Remove duplicates, keeping only the most recent version for each Connect_ID
M2_complete_nodup <- M2_complete[!duplicated(M2_complete$Connect_ID),]
table(M2_complete_nodup$version)


roi_parts <- bq_project_query(project, query = "SELECT Connect_ID, d_832139544,
        d_821247024,
        d_747006172,
        d_987563196,
        d_536735468,
        d_686238347_d_446235715,
        d_686238347_d_749055145,
        #d_686238347_d_295732360
        FROM `nih-nci-dceg-connect-prod-6d04.FlatConnect.participants_JP`
        WHERE
        d_821247024 = '197316935'     -- is verified
        AND d_747006172 = '104430631' -- has not withdrawn consent
        AND d_987563196 = '104430631' -- should not be deceased
        AND d_536735468 ='231311385'  -- shuld have submitted module 2")

ROI_BQ <- bq_table_download(roi_parts, bigint = "integer64")


module2_roi= left_join(ROI_BQ, M2_complete_nodup, by="Connect_ID")


check_roi <- as_tibble(module2_roi)

```

