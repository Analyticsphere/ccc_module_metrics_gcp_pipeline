cat("Booting plumber...\n")

port <- as.numeric(Sys.getenv("PORT", "8080"))
cat("PORT =", port, "\n")

suppressWarnings(
  tryCatch({
    pr <- plumber::plumb("ccc_module_metrics_api.R")
    cat("Plumber loaded.\n")
    pr$run(host = "0.0.0.0", port = port)
  }, error = function(e) {
    cat("FATAL:", e$message, "\n")
    Sys.sleep(600)
  })
)
