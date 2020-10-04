# Known possible locations of mru file
mon_list <- file.path(c('rstudio', 'RStudio-Desktop', '.rstudio-desktop'),
                      "monitored/lists/project_mru")

app_loc <- c(file.path("AppData/Local", mon_list[-3]), mon_list[3])

h_roots <- c('~', file.path('C:/Users', Sys.info()['user']))

mru_path_opts <- as.vector(sapply(h_roots, file.path, app_loc), 'character')

# Set root dirs
root_dirs <- c(`Git repos` = normalizePath('~/R/ProjectDir/git_repos', .Platform$file.sep),
               `~/R` = normalizePath('~/R', .Platform$file.sep),
               Home  = normalizePath('~', .Platform$file.sep))

# Dirs to exclude from search
excl_dirs <- paste0(
  c(dirname(.libPaths()), '\\.git$', '\\.Rproj\\.user$'),
  collapse = '|')

usethis::use_data(mru_path_opts, root_dirs, excl_dirs, internal = TRUE, overwrite = TRUE)
