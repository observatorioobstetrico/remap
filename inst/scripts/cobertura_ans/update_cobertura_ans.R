get_cobertura_ans_update_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)

  if (!length(file_arg)) {
    return(normalizePath("inst/scripts/cobertura_ans/update_cobertura_ans.R", winslash = "/", mustWork = TRUE))
  }

  normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)
}

script_path <- get_cobertura_ans_update_script_path()
source(file.path(dirname(script_path), "cobertura_ans_pipeline.R"), local = TRUE)

project_dir <- normalizePath(file.path(dirname(script_path), "..", "..", ".."), winslash = "/", mustWork = TRUE)
args <- parse_cobertura_ans_args()

build_cobertura_ans_data(project_dir = project_dir, rebuild_only = args$rebuild_only)
