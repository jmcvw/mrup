# style
mrup_style <- shiny::tags$head(
  shiny::tags$style(shiny::HTML("
                      .mru_btn {
                        color: #ffffff;
                        background-color: #337ab7;
                        border-color: #2e6da4;
                      }
                      #done {
                        background-color: #3cb371;
                      }
                      ")))


#~/.local/share/rstudio/monitored/lists/project_mru

# Known possible locations of mru file
mon_list <- file.path(c('.rstudio-desktop',
                        '.local/share/rstudio',
                        'RStudio-Desktop',
                        'rstudio'),
                      'monitored/lists/project_mru')

app_loc <- c(mon_list[1:2], file.path("AppData/Local", mon_list[-(1:2)]))

h_roots <- c('~', 'C:/Users/%s')

mru_path_opts <- as.vector(sapply(h_roots, file.path, app_loc), 'character')

# Set root dirs
root_dirs <- c(`Git repos` = normalizePath('~/R/ProjectDir/git_repos', .Platform$file.sep),
               `~/R` = normalizePath('~/R', .Platform$file.sep),
               Home  = normalizePath('~', .Platform$file.sep))

# Dirs to exclude from search
excl_dirs <- paste0(
  c(dirname(.libPaths()), '\\.git$', '\\.Rproj\\.user$'),
  collapse = '|')


usethis::use_data(mrup_style, mru_path_opts,
                  root_dirs, excl_dirs,
                  internal = TRUE, overwrite = TRUE)
