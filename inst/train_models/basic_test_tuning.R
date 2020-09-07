library(tfruns)

### Train Models
# training_run("inst/train_models/basic_test.R", flags = list(n_epochs = 30))

### Tune Models
runs <- tuning_run(
  "inst/train_models/basic_test.R",
  runs_dir = "inst/runs",
  flags = list(
    n_epochs = c(50),
    optimizer_type = c("rmsprop"),
    patience = 10
  )
)
runs

# Save html of single run
run_to_save <- runs$run_dir[1]
dir_name <- substring(run_to_save, regexpr("(?!.*\\/).+$", run_to_save, perl = TRUE))
filename <- paste0("inst/tfruns/", dir_name, ".html")
save_run_view(run_to_save, filename)
browseURL(filename)

# Save run comparison
filename <- "inst/tfruns/basic_sequential_compare.html"
save_run_comparison(filename = filename)
browseURL(filename)

# Single run stats
# latest_run()
# view_run("runs/2020-09-07T09-31-17Z")
# ls_runs()
#
# # Save HTML of single run
# filename <- "inst/tfruns/basic_sequential_epochs30.html"
# save_run_view(filename = filename)
# browseURL(filename)
