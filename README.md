
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{shinyremap}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{shinyremap}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
shinyremap::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-07-03 08:57:27 -03"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
#> ℹ Loading shinyremap
#> Warning: replacing previous import 'bs4Dash::column' by 'shiny::column' when
#> loading 'shinyremap'
#> Warning: replacing previous import 'bs4Dash::actionButton' by
#> 'shiny::actionButton' when loading 'shinyremap'
#> Warning: replacing previous import 'bs4Dash::tabsetPanel' by
#> 'shiny::tabsetPanel' when loading 'shinyremap'
#> Warning: replacing previous import 'bs4Dash::insertTab' by 'shiny::insertTab'
#> when loading 'shinyremap'
#> Warning: replacing previous import 'bs4Dash::navbarMenu' by 'shiny::navbarMenu'
#> when loading 'shinyremap'
#> Warning: replacing previous import 'shiny::runExample' by
#> 'shinyalert::runExample' when loading 'shinyremap'
#> Warning: replacing previous import 'bs4Dash::closeAlert' by
#> 'shinyalert::closeAlert' when loading 'shinyremap'
#> ── R CMD check results ────────────────────────────── shinyremap 0.0.0.9000 ────
#> Duration: 55.2s
#> 
#> ❯ checking for portable file names ... WARNING
#>   Found the following file with a non-portable file name:
#>     Orientações para configuração do ambiente de trabalho do app.pdf
#>   These are not fully portable file names.
#>   See section 'Package structure' in the 'Writing R Extensions' manual.
#> 
#> ❯ checking whether package 'shinyremap' can be installed ... WARNING
#>   See below...
#> 
#> ❯ checking code files for non-ASCII characters ... WARNING
#>   Found the following files with non-ASCII characters:
#>     R/app_ui.R
#>     R/global_data_call.R
#>     R/global_data_obitos.R
#>     R/mod_analise_cruzada_server.R
#>     R/mod_analise_cruzada_ui.R
#>     R/mod_anomalias_server.R
#>     R/mod_anomalias_ui.R
#>     R/mod_cesarias_server.R
#>     R/mod_cesarias_ui.R
#>     R/mod_classrobson_server.R
#>     R/mod_classrobson_ui.R
#>     R/mod_home_ui.R
#>     R/mod_nascimentos_server.R
#>     R/mod_nascimentos_ui.R
#>     R/mod_obitos_nao_considerados_server.R
#>     R/mod_obitos_nao_considerados_ui.R
#>     R/mod_obitos_oficiais_server.R
#>     R/mod_obitos_oficiais_ui.R
#>     R/mod_prematuros_server.R
#>     R/mod_prematuros_ui.R
#>     R/mod_prenatal_server.R
#>     R/mod_prenatal_ui.R
#>     R/mod_robson_cesareas_server.R
#>     R/mod_robson_cesareas_ui.R
#>     R/mod_rras_aps_server.R
#>     R/mod_rras_aps_ui.R
#>     R/mod_series_obitos_server.R
#>     R/mod_series_obitos_ui.R
#>   Portable packages must use only ASCII characters in their R code and
#>   NAMESPACE directives, except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#>   Function 'tools::showNonASCIIfile' can help in finding non-ASCII
#>   characters in files.
#> 
#> ❯ checking dependencies in R code ... WARNING
#>   '::' or ':::' import not declared from: 'ggplot2'
#>   'library' or 'require' calls not declared from:
#>     'dplyr' 'janitor' 'readr'
#>   'library' or 'require' calls in package code:
#>     'dplyr' 'janitor' 'readr'
#>     Please use :: or requireNamespace() instead.
#>     See section 'Suggested packages' in the 'Writing R Extensions' manual.
#>   Namespaces in Imports field not imported from:
#>     'attempt' 'htmltools' 'pkgload' 'stringr' 'tibble'
#>     All declared Imports should be used.
#> 
#> ❯ checking for missing documentation entries ... WARNING
#>   Undocumented code objects:
#>     'app_server' 'app_ui' 'mod_analise_cruzada_server'
#>     'mod_analise_cruzada_ui' 'mod_obitos_nao_considerados_server'
#>     'mod_obitos_nao_considerados_ui' 'mod_obitos_oficiais_server'
#>     'mod_obitos_oficiais_ui' 'mod_robson_cesareas_server'
#>     'mod_robson_cesareas_ui' 'mod_series_obitos_server'
#>     'mod_series_obitos_ui'
#>   All user-level objects in a package should have documentation entries.
#>   See chapter 'Writing R documentation files' in the 'Writing R
#>   Extensions' manual.
#> 
#> ❯ checking package dependencies ... NOTE
#>   Imports includes 28 non-default packages.
#>   Importing from so many packages makes the package vulnerable to any of
#>   them becoming unavailable.  Move as many as possible to Suggests and
#>   use conditionally.
#> 
#> ❯ checking installed package size ... NOTE
#>     installed size is 68.1Mb
#>     sub-directories of 1Mb or more:
#>       app  67.8Mb
#> 
#> ❯ checking top-level files ... NOTE
#>   Non-standard files/directories found at top level:
#>     'Orientações para configuração do ambiente de trabalho do app.pdf'
#>     'teste' 'utils.txt'
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   mod_obitos_oficiais_server : <anonymous>: warning in
#>     selectInput(ns("drs"), "Selecione a DRS:", choice =
#>     sort(unique(data_list$oficiais$drs))): partial argument match of
#>     'choice' to 'choices'
#>   mod_obitos_oficiais_server : <anonymous>: warning in
#>     selectInput(ns("regiao_de_saude"), "Selecione a Região de Saúde:",
#>     choice = sort(unique(data_list$oficiais$regiao_de_saude))): partial
#>     argument match of 'choice' to 'choices'
#>   mod_obitos_oficiais_server : <anonymous>: warning in
#>     selectInput(ns("municipio_sp"), "Selecione o Município:", choice =
#>     sort(unique(data_list$oficiais$municipio_sp))): partial argument
#>     match of 'choice' to 'choices'
#>   cria_indicadores: no visible binding for global variable 'tipo'
#>   cria_indicadores: no visible binding for global variable 'ano'
#>   load_indicadores_data: no visible global function definition for
#>     'read_csv'
#>   load_obitos_data: no visible binding for global variable 'cod_ibge'
#>   load_obitos_data: no visible binding for global variable 'municipio'
#>   load_obitos_data: no visible binding for global variable 'rras'
#>   load_obitos_data: no visible binding for global variable
#>     'regiao_de_saude'
#>   load_obitos_data: no visible binding for global variable 'drs'
#>   load_obitos_data : read_and_expand: no visible binding for global
#>     variable 'municipio'
#>   load_series_data: no visible binding for global variable 'cod_ibge'
#>   load_series_data: no visible binding for global variable 'municipio'
#>   load_series_data: no visible binding for global variable 'rras'
#>   load_series_data: no visible binding for global variable
#>     'regiao_de_saude'
#>   load_series_data: no visible binding for global variable 'drs'
#>   load_series_data: no visible global function definition for 'str_trim'
#>   load_series_data: no visible binding for global variable
#>     'macro_r_saude'
#>   load_series_data: no visible global function definition for
#>     'str_detect'
#>   load_series_data: no visible global function definition for 'regex'
#>   load_series_data: no visible global function definition for
#>     'str_replace'
#>   load_series_data: no visible binding for global variable
#>     'municipio_lower'
#>   load_series_data: no visible binding for global variable 'municipio.x'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'rras'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'drs'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'regiao_de_saude'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'municipio_sp'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'idade'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'racacor'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'tipo_de_morte_materna'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'periodo_do_obito'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'investigacao_cmm'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'obitos'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'variavel_linha'
#>   mod_analise_cruzada_server : <anonymous>: no visible binding for global
#>     variable 'variavel_coluna'
#>   mod_analise_cruzada_server : <anonymous>: no visible global function
#>     definition for 'colorRampPalette'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'anomalia'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'total_nascidos'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'faltante_anomalia'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'n_anom'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'nascidos'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'faltantes'
#>   mod_anomalias_server : <anonymous>: no visible binding for global
#>     variable 'pct'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'total_nascidos'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'faltante_tipo_parto'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'cesarea'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'cesareas'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'nascidos'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'faltantes'
#>   mod_cesarias_server : <anonymous>: no visible binding for global
#>     variable 'pct'
#>   mod_nascimentos_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_nascimentos_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_nascimentos_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_nascimentos_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_nascimentos_server : <anonymous>: no visible binding for global
#>     variable 'nascidos'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible global
#>     function definition for 'na.omit'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'ano'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'idade'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'racacor'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'periodo_do_obito'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'investigacao_cmm'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'capitulo_cid10'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'rras'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'drs'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'regiao_de_saude'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'municipio_sp'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'causabas_categoria'
#>   mod_obitos_nao_considerados_server : <anonymous>: no visible binding
#>     for global variable 'obitos'
#>   mod_obitos_nao_considerados_server : <anonymous> : <anonymous>: no
#>     visible global function definition for 'write.csv'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'idade'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'racacor'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'tipo_de_morte_materna'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'periodo_do_obito'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'investigacao_cmm'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'rras'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'drs'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'regiao_de_saude'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'municipio_sp'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'capitulo_cid10'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'causabas_categoria'
#>   mod_obitos_oficiais_server : <anonymous>: no visible binding for global
#>     variable 'obitos'
#>   mod_obitos_oficiais_server : <anonymous> : <anonymous>: no visible
#>     global function definition for 'write.csv'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'total_nascidos'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'faltante_premat'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'premat'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'prematuros'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'nascidos'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'faltantes'
#>   mod_prematuros_server : <anonymous>: no visible binding for global
#>     variable 'pct'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'nascidos'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'faltante_consulta'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'nenhuma_consulta'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'consulta1'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'consulta4'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'nenhuma'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'faltantes'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'uma_ate_seis'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'sete_ou_mais'
#>   mod_prenatal_server : <anonymous>: no visible binding for global
#>     variable 'pct'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'grupo_robson_aux'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'nascidos'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'tipo_parto'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'faltante'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'cesarea'
#>   mod_robson_cesareas_server : <anonymous>: no visible binding for global
#>     variable 'pct'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     'uf'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     'municipio'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     'ano'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     '.'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     'grupo_robson_aux'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     'nascidos'
#>   mod_robson_server : <anonymous>: no visible binding for global variable
#>     'pct'
#>   mod_rras_aps_server : <anonymous> : build_bar_plot: no visible global
#>     function definition for 'as.formula'
#>   mod_rras_aps_server : <anonymous>: no visible global function
#>     definition for 'aggregate'
#>   mod_rras_aps_server : <anonymous>: no visible binding for global
#>     variable 'MUNICIPAL'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'ano'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'uf'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'macro_r_saude'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'drs'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'r_saude'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'municipio'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'regiao'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'localidade'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'RMM'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'rmm'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'obito_mif_investigado_com_ficha_sintese'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'obito_mif_investigado_sem_ficha_sintese'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'total_obitos_mulher_idade_fertil'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'obito_materno_investigado_com_ficha_sintese'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'obito_materno_investigado_sem_ficha_sintese'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'total_obitos_maternos'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'cobertura'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'soma_obitos_mat_totais'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'prop_obitos_diretos'
#>   mod_series_obitos_server : <anonymous>: no visible binding for global
#>     variable 'eixo'
#>   Undefined global functions or variables:
#>     . MUNICIPAL RMM aggregate ano anomalia as.formula capitulo_cid10
#>     causabas_categoria cesarea cesareas cobertura cod_ibge
#>     colorRampPalette consulta1 consulta4 drs eixo faltante
#>     faltante_anomalia faltante_consulta faltante_premat
#>     faltante_tipo_parto faltantes grupo_robson_aux idade investigacao_cmm
#>     localidade macro_r_saude municipio municipio.x municipio_lower
#>     municipio_sp n_anom na.omit nascidos nenhuma nenhuma_consulta
#>     obito_materno_investigado_com_ficha_sintese
#>     obito_materno_investigado_sem_ficha_sintese
#>     obito_mif_investigado_com_ficha_sintese
#>     obito_mif_investigado_sem_ficha_sintese obitos pct periodo_do_obito
#>     premat prematuros prop_obitos_diretos r_saude racacor read_csv regex
#>     regiao regiao_de_saude rmm rras sete_ou_mais soma_obitos_mat_totais
#>     str_detect str_replace str_trim tipo tipo_de_morte_materna tipo_parto
#>     total_nascidos total_obitos_maternos total_obitos_mulher_idade_fertil
#>     uf uma_ate_seis variavel_coluna variavel_linha write.csv
#>   Consider adding
#>     importFrom("grDevices", "colorRampPalette")
#>     importFrom("stats", "aggregate", "as.formula", "na.omit")
#>     importFrom("utils", "write.csv")
#>   to your NAMESPACE file.
#> 
#> 0 errors ✔ | 5 warnings ✖ | 4 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> shinyremap Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/global_data_call.R: 0.00%
#> R/global_data_indicadores.R: 0.00%
#> R/global_data_obitos.R: 0.00%
#> R/mod_analise_cruzada_server.R: 0.00%
#> R/mod_analise_cruzada_ui.R: 0.00%
#> R/mod_anomalias_server.R: 0.00%
#> R/mod_anomalias_ui.R: 0.00%
#> R/mod_cesarias_server.R: 0.00%
#> R/mod_cesarias_ui.R: 0.00%
#> R/mod_classrobson_server.R: 0.00%
#> R/mod_classrobson_ui.R: 0.00%
#> R/mod_home_server.R: 0.00%
#> R/mod_home_ui.R: 0.00%
#> R/mod_nascimentos_server.R: 0.00%
#> R/mod_nascimentos_ui.R: 0.00%
#> R/mod_obitos_nao_considerados_server.R: 0.00%
#> R/mod_obitos_nao_considerados_ui.R: 0.00%
#> R/mod_obitos_oficiais_server.R: 0.00%
#> R/mod_obitos_oficiais_ui.R: 0.00%
#> R/mod_prematuros_server.R: 0.00%
#> R/mod_prematuros_ui.R: 0.00%
#> R/mod_prenatal_server.R: 0.00%
#> R/mod_prenatal_ui.R: 0.00%
#> R/mod_robson_cesareas_server.R: 0.00%
#> R/mod_robson_cesareas_ui.R: 0.00%
#> R/mod_rras_aps_server.R: 0.00%
#> R/mod_rras_aps_ui.R: 0.00%
#> R/mod_series_obitos_server.R: 0.00%
#> R/mod_series_obitos_ui.R: 0.00%
#> R/run_app.R: 0.00%
```
