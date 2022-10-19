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
                      input[type=checkbox] {
                        transform: scale(1.6);
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

mru_path_opts <- unlist(lapply(h_roots, file.path, app_loc))

# Set root dirs
# Path 1 only on Windows and only for me
# don't update data on Mac, at least not till I've looked into how to stop overwriting
root_dirs <- c(`Git repos` = normalizePath('~/R/ProjectDir/git_repos'),
               `Documents` = normalizePath('~/Documents', '/'),
               `~/R` = normalizePath('~/R', '/'),
               Home  = normalizePath('~', '/'))


# Dirs to exclude from search
excl_dirs <- paste0(
  c(dirname(.libPaths()), '\\.git$', '\\.Rproj\\.user$'),
  collapse = '|')


usethis::use_data(mrup_style, mru_path_opts,
                  root_dirs, excl_dirs,
                  internal = TRUE, overwrite = TRUE)
