get_update_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }

  if (!is.null(sys.frame(1)$ofile)) {
    return(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))
  }

  normalizePath("inst/scripts/obitos_grav_puerp/update_obitos_grav_puerp.R", winslash = "/", mustWork = TRUE)
}

script_path <- get_update_script_path()
source(file.path(dirname(script_path), "obitos_grav_puerp_pipeline.R"), local = TRUE)

if (sys.nframe() == 0) {
  args <- parse_obitos_grav_puerp_args()
  project_dir <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)

  build_obitos_grav_puerp_data(
    project_dir = project_dir,
    rebuild_only = args$rebuild_only,
    force_download = args$force_download,
    apply_to_app = args$apply_to_app,
    stop_on_target_mismatch = args$stop_on_target_mismatch,
    resident_uf = args$resident_uf,
    historical_years = args$historical_years,
    preliminary_years = args$preliminary_years,
    timeout = args$timeout
  )
}
