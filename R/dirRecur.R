LoadPackages(tidyr, dplyr, purrr, ggplot2)

system.time(
  dir('~/R', '\\.Rproj$', full.names = TRUE, recursive = TRUE)
)

d <- dir('~/R', full.names = T)
d <- d[!basename(d) %in% .libPaths()]

grepl(d, .libPaths())

'R/.*[23]\\.[0-9]{1,2}.+$'
substr(.libPaths())

any(sapply(.libPaths(), function(p, q) grepl(p, q), d[1]))
sapply(.libPaths(), paste)


searched <- c()
for (i in d) {

  if(any(sapply(.libPaths(), function(p, q) grepl(p, q), i)))
    print(T)
  else print(F)
  # searched <- dir(d[i], '\\.Rproj$', recursive = T)

}


RecRegEx <- function() {

}

exclude <- '\\.git'
excl_dirs <- '[0-9]?\\.[0-9]'
dir('~/R', '^.+(?!3\\.5).+\\.Rproj$', full.names = TRUE, recursive = TRUE, PERL = TRUE)

dir('~/R/ProjectDir/git_repos', recursive = TRUE, include.dirs = F)

dir2 <- function(d) {
  d <- dir(d, full.names = TRUE)
  if length()

  return(dir('~/R', 'ProjectDir/.+\\.Rproj$', full.names = TRUE))
}

dirR <- dir('~/R', full.names = TRUE)
for (d in dirR) {
  print(dir(d, full.names = TRUE))
}

dirRrec <- dir('~/R', recursive = TRUE)
length(dirRrec)
length(dirR)

list.dirs('~/R')[!list.dirs('~/R') %in% .libPaths()]





dir('d:/l1', recursive = T, include.dirs = T)
list.files('d:/l1', recursive = T, include.dirs = T)
list.dirs('d:/l1', recursive = T)



sapply(list.dirs('d:/l1', recursive = T), function(d) file.create(file.path(d, paste0(basename(d), 'textfile.txt'))))

z <- list.dirs('D:/l1/l2b')
sapply(z, dir)

recurDir <- function(d, ext = '\\.Rproj$') {

  dirs <- list.dirs(d, full.names = T, recursive = F)
  keep <- dir(d, patt = ext, full.names = TRUE)

  if (!length(dirs) && !length(keep))
    return()
  else if(!length(dirs) && length(keep)) return(keep)

  keep2 <- lapply(dirs, recurDir)

  list(keep, keep2)
}
debugonce(recurDir)

res <- recurDir('D:/l1', '\\.txt$')
res[!is.null(names(res))]
length(res)
View(res)
unlist(res)
str(res)
structure(res)

listviewer::jsonedit(res)


dir('D:/l1', patt = '\\.txt$', recursive = TRUE)
list.dirs('D:/l1')
list.dirs('D:/l1/l2a/l2a-1', recursive = F)

dir('D:/l1', patt = '\\.txt$', full.names = TRUE)
dir('D:/l1', full.names = TRUE)[grepl('\\.txt$', d)]
dir('D:/l1', recursive = T)

length(dir('D:/l1/l2b/l2b1')) == 0
any(dir('D:/l1'))






dirs <- list.dirs('D:/l1', full.names = TRUE, recursive = FALSE)

for (d in dirs) {

  print(dir(d))
}

