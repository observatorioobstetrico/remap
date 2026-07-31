get_update_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }

  if (!is.null(sys.frame(1)$ofile)) {
    return(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))
  }

  normalizePath("inst/scripts/cobertura_ab/update_cobertura_ab.R", winslash = "/", mustWork = TRUE)
}

source(file.path(dirname(get_update_script_path()), "cobertura_ab_pipeline.R"), local = TRUE)

if (sys.nframe() == 0) {
  args <- parse_cobertura_ab_args()
  update_cobertura_ab_data(rebuild_only = args$rebuild_only)
}
