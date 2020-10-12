R -e "styler::style_pkg(); devtools::document(); devtools::build_vignettes(); devtools::build_readme(); devtools::check(); codemetar::write_codemeta();"
