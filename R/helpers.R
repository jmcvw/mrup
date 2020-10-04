#### ---------- HELPER FUNS ---------- ####

#' @export
#' @keywords internal
#'
ProjId <- function(path, value = 'proj') {
  switch(value,
         full = sub('^.+/Documents', '~', path),
         proj = sub('\\.Rproj$', '', basename(path)),
         dir  = sub('^.+/Documents', '~', dirname(path))
  )
}




#' @export
#' @keywords internal
#'
dirRecur <- function(search.dir, ext = '\\.Rproj$',
                     excl = excl_dirs) {

  dirs <- list.dirs(search.dir, full.names = TRUE, recursive = FALSE)

  if (!missing(excl))
    dirs <- dirs[!grepl(excl, dirs)]

  files  <- dir(search.dir, patt = ext, full.names = TRUE)

  if (!length(dirs) && !length(files ))
    return()

  if(!length(dirs) && length(files ))
    return(files )

  c(files , lapply(dirs, dirRecur, ext, excl), recursive = TRUE)

}

#' @export
#' @keywords internal
#'
find_roots <- function() {

  dirs <- c(mrup:::root_dirs,
            shinyFiles::getVolumes()())
  dirs[dir.exists(dirs)]

}

#' Simplify MRU
#'
#' creates mru minus projects that appear on RStudio's drop-down menu could be
#' conoslidated better - or even removed
#'
#' @param d
#' @param mru
#'
#' @return
#' @export
#' @keywords internal
#'
simplify_mru <- function(d, mru) {
  x <- d()[!d()$project %in% names(mru()[1:10]), ]

  paste0(x[['project']], ' (', x[['days_since_mod']], ' days)')
}


#' @return
#' @export
#'
#' @examples
choose_proj_to_open <- function(choice, open_new) {
  choice <- sub('\\s.*$', '', choice)
  choice <- all_proj()[all_proj()$project == choice, 'path']

  rstudioapi::openProject(choice, open_new)
}

#' Compile project data
#'
#' Compile a dataframe of projects contained within the chosen search directory
#'
#' @param proj_list
#' @param search_path
#' @param input
#'
#' @return
#' @export
#' @keywords internal
#'
compile_proj <- function(proj_list, search_path, input) {
  proj_list <- data.frame(path = dirRecur(search_path),
                         stringsAsFactors = FALSE)

  if (!nrow(proj_list)) return(NULL)

  proj_list[!proj_list$project %in% input, ]
  proj_list$project <- ProjId(proj_list[[1]], 'proj')

  proj_list$last_modified  <- as.Date(file.info(proj_list[[1]])$mtime)
  proj_list$days_since_mod <- as.integer(Sys.Date() - proj_list$last_modified)
  proj_list$last_modified  <- format(proj_list$last_modified, '%b %Y')

  proj_list <- proj_list[order(proj_list$days_since_mod), ]

  proj_list[c(2:4, 1)]
}

#' @return
#' @export
#' @keywords internal
#'
remove_proj_from_mru <- function(remove, mru) {
  mru(mru()[!mru() %in% remove])
}

#' @return
#' @export
#' @keywords internal
#'
add_proj_to_mru <- function(add, mru) {

  temp_mru <- ProjId(add()$path, 'full')
  names(temp_mru) <- ProjId(temp_mru, 'proj')
  mru({ c(temp_mru, mru()) })

}

#' Rename project files
#'
#' Renames both the project directory and .Rproj file, as well as updating
#'
#' @param d Dataframe; All projects found in chosen directory
#' @param old_name Character; Name of project to change
#' @param new_name Character
#'
#' @export
#' @keywords internal
#'
rename_proj <- function(d, mru, old_name, new_name) {

  on <- d()[d()$project == old_name, 'path']
  nn <- gsub(old_name, new_name, on)
  file.rename(on, file.path(dirname(on), basename(nn)))  # rename file
  file.rename(dirname(on), dirname(nn))  # rename dir

  # Update mru
  temp_mru <- mru()

  temp_mru[old_name] <- gsub(
    old_name,
    new_name,
    temp_mru[old_name]
  )

  temp_mru <- ProjId(temp_mru, 'full')
  names(temp_mru) <- ProjId(temp_mru, 'proj')

  mru(temp_mru)

}
