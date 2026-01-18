# Dockerfile

FROM rocker/tidyverse:4.4.3

# Set the correct path for xelatex
ENV PATH="$PATH:/root/bin:/usr/local/lib"

# Install TinyTeX and necessary LaTeX packages
RUN Rscript -e 'tinytex::install_tinytex()' && \
    Rscript -e 'tinytex::tlmgr_install(c( \
      "multirow", "ulem", "environ", "colortbl", "wrapfig", "pdflscape", \
      "tabu", "threeparttable", "threeparttablex", "makecell", "caption", \
      "anyfontsize"))'
      
# Install R libraries
RUN install2.r --error plumber bigrquery googleCloudStorageR gargle \
               tools epiDisplay knitr gtsummary reshape gmodels config magick \
               foreach arsenal rio gridExtra scales data.table listr sqldf \
               expss magrittr naniar UpSetR RColorBrewer DBI logger
               
# These libraries might not be available from install2.R so use CRAN
RUN R -e "install.packages(c('gt', 'vtable', 'pdftools'), dependencies=TRUE, repos='http://cran.rstudio.com/')"

# When I try to use kable extra with a normal installation from CRAN or install2.r
# I get the error:
# Error: package or namespace load failed for 'kableExtra':
# .onLoad failed in loadNamespace() for 'kableExtra', details:
#  call: !is.null(rmarkdown::metadata$output) && rmarkdown::metadata$output %in% 
#  error: 'length = 2' in coercion to 'logical(1)'
# The solution is to install a patched version from github
# https://github.com/haozhu233/kableExtra/issues/750
RUN R -e "devtools::install_github('kupietz/kableExtra')"

# Copy R code to directory in instance
COPY ["./ccc_module_metrics_api.R", "./ccc_module_metrics_api.R"]
COPY ["./config.yml", "./config.yml"]
COPY ["./preamble.tex", "./preamble.tex"]
COPY ["./CCC Weekly Module Metrics_RMD.Rmd", "./CCC Weekly Module Metrics_RMD.Rmd"]
COPY ["./Merged Module 1 Summary Statistics.Rmd", "./Merged Module 1 Summary Statistics.Rmd"]
COPY ["./Merged Module 2 Summary Statistics.Rmd", "./Merged Module 2 Summary Statistics.Rmd"]
COPY ["./Module 3 Summary Statatistics.Rmd", "./Module 3 Summary Statatistics.Rmd"]
COPY ["./Module 4 Missingness Analysis.Rmd", "./Module 4 Missingness Analysis.Rmd"]
COPY ["./Rectruitment Derived Variable QC.Rmd", "./Rectruitment Derived Variable QC.Rmd"]
COPY ["./Module 1 Custom QC Rule Errors.Rmd", "./Module 1 Custom QC Rule Errors.Rmd"]
COPY ["./3 Month Quality of Life Survey Summary Statistics.Rmd" , "./3 Month Quality of Life Survey Summary Statistics.Rmd"]
COPY ["./Notifications QC.Rmd", "./Notifications QC.Rmd"]
COPY ["./Data Destruction CSV Output.R", "./Data Destruction CSV Output.R"]
COPY ["./PROMIS Completion vs Notifications.Rmd", "PROMIS Completion vs Notifications.Rmd"]
COPY ["./RCA Metrics.Rmd", "RCA Metrics.Rmd"]
COPY ["./RCA Custom QC.Rmd", "RCA Custom QC.Rmd"]
COPY ["./Module_2_Custom_QC.Rmd", "Module_2_Custom_QC.Rmd"]
COPY ["./Module_3_Custom_QC.Rmd", "Module_3_Custom_QC.Rmd"]
COPY ["./Cancer Screening Summary Statistics.Rmd", "Cancer Screening Summary Statistics.Rmd"]
COPY ["./Monthly Derived Survey Variables.Rmd", "Monthly Derived Survey Variables.Rmd"]
COPY ["./entrypoint.R", "entrypoint.R"]

ENV GCS_SERVICE_ACCOUNT_EMAIL="qa-qc-prod@nih-nci-dceg-connect-prod-6d04.iam.gserviceaccount.com"

# Run R code
EXPOSE 8080

ENTRYPOINT ["Rscript", "entrypoint.R"]
