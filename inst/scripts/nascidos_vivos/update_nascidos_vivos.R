get_update_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (length(file_arg)) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  }

  if (!is.null(sys.frame(1)$ofile)) {
    return(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE))
  }

  normalizePath("inst/scripts/nascidos_vivos/update_nascidos_vivos.R", winslash = "/", mustWork = TRUE)
}

script_path <- get_update_script_path()
source(file.path(dirname(script_path), "nascidos_vivos_pipeline.R"), local = TRUE)

if (sys.nframe() == 0) {
  args <- parse_nascidos_vivos_args()
  project_dir <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
  build_nascidos_vivos_aps_data(project_dir = project_dir, rebuild_only = args$rebuild_only)
}
