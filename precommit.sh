R -e "styler::style_pkg(); devtools::document(); devtools::build_vignettes(); devtools::build_readme(); devtools::check(); pkgdown::build_site(); codemetar::write_codemeta();"
