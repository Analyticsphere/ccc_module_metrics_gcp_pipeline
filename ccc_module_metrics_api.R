# File:       ccc_module_metrics_api.R
# Decription: This script generates a plumber api that runs/renders Kelsey's 
#             Rmarkdown file.
# Author:     Jake Peters
# Date:       October 2022

library(plumber)
library(rmarkdown)
library(googleCloudStorageR)
library(gargle)
library(tools)

#* heartbeat...for testing purposes only. Not required to run analysis.
#* @get /
#* @post /
function(){return("alive")}

#* Runs Kelsey's markdown file 
#* @get /ccc_api
#* @post /ccc_api
function(){
    
    # Define parameters 
    rmd_file_name    <- "ccc_module_metrics.rmd"
    report_file_name <- "ccc_module_metrics.pdf"
    bucket           <- "gs://analytics_team_reports"
    
    # Add time stamp to report name
    report_fid <- paste0(file_path_sans_ext(report_file_name), 
                         format(Sys.time(), "_%m_%d_%Y"),
                         ".", file_ext(report_file_name))
    
    # Select document type given the extension of the report file name
    output_format <- switch(file_ext(report_file_name),  
                            "pdf"  = "pdf_document",
                            "html" = "html_document") 
    if (is.null(output_format)) { 
      stop("Report file extension is invalid. Script did not execute.")}
    
    # Render the rmarkdown file
    rmarkdown::render(rmd_file_name, 
                      output_format = output_format,
                      output_file = report_fid, 
                      clean = TRUE)
    
    # Authenticate with Google Storage and write report file to bucket
    scope <- c("https://www.googleapis.com/auth/cloud-platform")
    token <- token_fetch(scopes=scope)
    gcs_auth(token=token)
    gcs_upload(report_fid, bucket=bucket, name=report_fid) 

    #TODO write report_fid to Box
    
    # Return a string for for API testing purposes
    ret_str <- paste("All done. Check", bucket, "for", report_fid)
    print(ret_str)
    return(ret_str) 
}
