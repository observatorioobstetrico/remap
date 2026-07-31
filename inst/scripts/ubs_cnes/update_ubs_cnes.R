#!/usr/bin/env Rscript

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_file <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else "inst/scripts/ubs_cnes/update_ubs_cnes.R"
project_dir <- normalizePath(file.path(dirname(script_file), "..", "..", ".."), winslash = "/", mustWork = TRUE)

script_path <- normalizePath(file.path(project_dir, "inst", "scripts", "ubs_cnes", "ubs_cnes_pipeline.R"), winslash = "/", mustWork = TRUE)
source(script_path)

args <- parse_ubs_cnes_args(commandArgs(trailingOnly = TRUE))
build_ubs_cnes_aps_data(project_dir = project_dir, rebuild_only = args$rebuild_only)
