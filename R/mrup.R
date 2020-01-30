#' Manage recent projects
#'
#' An add-in for managing the RStudio \italic{Recently Used Projects} list. It
#' simplifies opening projects that do not appear in the RStudio projects
#' drop-down menu as well as projects to be added to, or removed from the recent
#' project drop-down menu. It also makes it possible to rename existing projects
#' without manually navigating the file system
#'
#' The four functions of the app are each accessed through a dedicated tab at
#' the bottom of the window.
#'
#' @section Open tab
#'
#'   The "Open project" tab makes it easier than navigating the directory system
#'   to locate old projects. The chosen directory and all sub-directories are
#'   searched and all projects found are listed. The search directory can be
#'   changed by clicking the link and navigating to the desired location.
#'
#'   It is possible to open the chosen project in either the current or a new
#'   RStudio session.
#'
#' @section Remove tab:
#'
#'   The RStudio menu shows a list of the 10 most recently used projects. This
#'   is based on the project_mru file which may contain more than 10 projects
#'   (apparently 15 max?). The remove tab lists the entire contents of the
#'   project_mru file, and allows any of them to be removed.
#'
#'   Pressing the \code{Remove} button creates a modified list to replace the
#'   current list.
#'
#'   Replacement only takes place if the \code{Save changes} button is pressed.
#'
#' @section Add tab:
#'
#'   All subdirectories of the \code{~/R} directory are searched for
#'   \code{.Rproj} files, and may take a few seconds to complete.  The directory
#'   searched is the same as set in the "Open project" tab, where it can also be
#'   changed.
#'
#'   Once done, a list is generated that indicates how long it has been since
#'   each project was modified. Selected projects are shown in a table that also
#'   shows the full path to the project. Pressing \code{Cancel} closes the app
#'   without making any changes
#'
#'   Pressing the \code{Add} button creates a list with all selected projects
#'   placed at the top of the project list. Replacement only takes place if the
#'   \code{Save changes} button is pressed. Pressing \code{Cancel} closes the
#'   app without making any changes
#'
#'   Since the list can only show 10 projects, those further down will be
#'   removed, but will stay on the project_mru file (for a while at least).
#'
#'   \strong{NB:} It may take several seconds to locate all project files (even
#'   in the default location).
#'
#' @section Rename tab:
#'
#'   As with opening and adding projects to the MRU list, the projects listed
#'   are those found in the directory set in the "Open project" tab. After
#'   making changes pressing the refresh button allows the list to be updated
#'   without restarting the add-in.
#'
#'   When renaming, select 1 project and enter the new name in the box, without
#'   a file extension. As long as the project directory has the same name as the
#'   project itself, both shall be renamed, and the \code{.Rproj} extension will
#'   remain intact.
#'
#'   The name change is implemented immediately on pressing \code{Rename}, with
#'   no need to press \code{Save changes}, and without the ability to cancel. To
#'   undo a rename, the project can be re-renamed straight away. In order to
#'   rename the same project more than once (e.g. to restore the original name)
#'   it is currently necessary to reload the app by pressing the reload button
#'   at the top right of the viewer pane.
#'
#'   There may be instances where the project has a name that is different from
#'   the containing directory. In this case only the \code{.Rproj} file (and
#'   therefore the project itself) is renamed. If it is desired that the names
#'   be made the same, changing the \code{.Rproj} first to match the directory,
#'   then renaming again to the desired name will result in both the directory
#'   and project acquiring the new name.
#'
#' @details
#'
mrup <- function() {

  library(shiny)
  library(miniUI)
  library(shinyFiles)

  #### ---------- HELPER FUNS ---------- ####

  ProjId <- function(path, value = 'proj') {
    switch(value,
           full = sub('^.+/Documents', '~', path),
           proj = sub('\\.Rproj$', '', basename(path)),
           dir  = sub('^.+/Documents', '~', dirname(path))
    )
  }

  excl_dirs <- paste0(
    c(dirname(.libPaths()), '\\.git$', '\\.Rproj\\.user$'),
    collapse = '|')

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

  #### ---------- SET MRU PATH ---------- ####
  # Known possible locations of mru file
  mru_path_opts <- c(
    file.path('~/AppData/Local/RStudio-Desktop/monitored/lists/project_mru'),
    file.path('C:/Users', Sys.info()['user'], 'AppData/Local/RStudio-Desktop/monitored/lists/project_mru'),
    file.path('~/.rstudio-desktop/monitored/lists/project_mru'),
    file.path('C:/Users', Sys.info()['user'], '.rstudio-desktop/monitored/lists/project_mru')
  )

  mru_path <- mru_path_opts[file.exists(mru_path_opts)]

  # choose path
  if (!length(mru_path_opts))
    stop('File "project_mru" not found')

  root_dirs <- c(`Git repos` = normalizePath('~/R/ProjectDir/git_repos', .Platform$file.sep),
                 `~/R` = normalizePath('~/R', .Platform$file.sep),
                 Home  = normalizePath('~', .Platform$file.sep),
                 getVolumes()())
  root_dirs <- root_dirs[dir.exists(root_dirs)]

  # --------------------------------------------------

  #### ---------- UI ---------- ####
  ui <- miniPage(

    #### ---------- STYLE ---------- ####
    tags$head(
      tags$style(HTML("
                      .mru_btn {
                        color: #ffffff;
                        background-color: #337ab7;
                        border-color: #2e6da4;
                      }
                      #done {
                        background-color: #3cb371;
                      }
                      "))
    ),

    #### ---------- LAYOUT ---------- ####
    miniTitleBar(strong('Recently-Used Projects'),
                 left = miniTitleBarButton('done', 'Save', primary = TRUE),
                 right = miniTitleBarCancelButton()
    ),

    miniTabstripPanel(
      between = p('See ', actionLink('help', code('?mrup')), ' for details'),

      #### ---------- Open project ---------- ####

      miniTabPanel('Open project', icon = icon('folder-open'),
                   miniContentPanel(
                     uiOutput('open_proj_ui')
                   )
      ),

      #### ---------- REMOVE PROJECT TABPANEL ---------- ####
      miniTabPanel('Remove project from list', icon = icon('minus-circle'),
                   miniContentPanel(
                     uiOutput('remove_proj_ui')
                   )
      ),

      #### ---------- ADD PROJECT TABPANEL ---------- ####
      # See issue #11 - maybe no needed anymore?
      miniTabPanel('Add project to list', icon = icon('plus-circle'),
                   miniContentPanel(
                     uiOutput('add_proj_ui')
                   )
      ),

      #### ---------- RENAME PROJECT TABPANEL---------- ####
      miniTabPanel('Rename project', icon = icon('file-text'),
                   miniContentPanel(
                     uiOutput('rename_proj')
                   )
      )
    )
  )

  #### ---------- SERVER ---------- ####
  server <- function(input, output) {#, session) {


    if (!length(find.package('rstudioapi', quiet = T)) ||
        !rstudioapi::isAvailable()) {
      cat('\n"MRUP" requires Rstudio to open or search for projects.\n')
      shiny::stopApp()
    }

    current_mru <- reactiveVal({
      mru_list <- readLines(mru_path)
      names(mru_list) <- ProjId(mru_list)
      mru_list
    })

    #--------------------------------------------------#

    observeEvent(input$help, {
      # help(topic = 'mrup', package = 'mrup')
      # do.call(help, list(package = 'mrup', 'mrup'))
      rstudioapi::previewRd(file.path(find.package('mrup'), 'mrup.Rd'))
    })

    ####---------- OPEN PROJECT UI ----------####

    output$open_proj_ui <- renderUI({

      tagList(

        validate(need(all_proj(), 'Compiling project list...')),

        strong('Current search directory. '),
        p('The selected directory will also be used for finding projects to rename or add to the MRU list'),
        shinyDirLink('dir', '(Change?)', 'Browse...'),
        verbatimTextOutput('dir'),

        strong('Open a project, not currently on the "Recently used" list, to open'),

        p('Any project in the chosen directory can be opened'),

        selectInput(
          'open_proj_choice', 'Projects',

          choices = c('Choose...' = '', proj_no_mru()),
          selectize = TRUE,
          multiple = FALSE,
          width = '100%'),

        checkboxInput('new_session',
                      'Open in new RStudio session?',
                      value = FALSE),

        actionButton('open_btn', 'Open', class = 'mru_btn')
      )
    })

    #### ---------- REMOVE PROJECT UI ---------- ####

    output$remove_proj_ui <- renderUI({

      length_mru <- length(current_mru())

      list(

        strong('Remove projects from recent projects list'),

        p('Only the 10 most recently used projects are displayed in the projects list.'),

        p('Removals are carried out after pressing ', code('Remove')),

        if (length_mru != 10)
          p('RStudio currently has a record of the last ', length_mru, 'projects.'),

        checkboxGroupInput('selection',
                           'Current project list',
                           current_mru()),

        actionButton('remove_btn', 'Remove', class = 'mru_btn')
      )
    })

    #### ---------- ADD-PROJECT UI ---------- ####

    proj_no_mru <- reactive({
      # could be conoslidated
      x <- all_proj()[!all_proj()$project %in% names(current_mru()[1:10]), ]

      paste0(x[['project']], ' (', x[['days_since_mod']], ' days)')

    })

    output$add_proj_ui <- renderUI({
      list(
        strong('Add projects to recent project list'),
        p('Projects are listed in order of days since their last modification.'),
        p('Additions are placed at the top of the recent projects list, after pressing ', code('Add')),

        # strong('Current search directory. '),
        # shinyDirLink('dir', '(Change?)', 'Browse...'),
        # verbatimTextOutput('dir'),

        selectInput(
          'proj_add_names', 'Projects',

          choices = c('Choose...' = '', proj_no_mru()),
          selectize = TRUE,
          multiple = TRUE,
          width = '100%'),

        actionButton('add_btn', 'Add', class = 'mru_btn'),

        tableOutput('choice_table')
      )
    })

    #### ---------- RENAME-PROJECT UI ---------- ####

    output$rename_proj <- renderUI({
      # n <- min(length(current_mru()), 10)

      choices <- isolate({ all_proj()$project })

      tagList(
        strong('Rename existing projects'),

        p('To rename the same project more than once
        it is necessary to reload the app after each rename
        (click the reload icon, top right)'),

        selectInput(
          'old_name', 'Choose project to rename',
          choices = c('Choose...' = '', choices),
          selectize = TRUE,
          width = '100%'
        ),

        textInput('new_name', 'Enter new project name',
                  width = '100%'),

        actionButton('rename_btn', 'Rename', class = 'mru_btn'),
        em('Takes place immediately, without pressing save.'),
      )
    })


    #### ---------- CHOOSE SEARCH DIR ---------- ####

    search_dir <- reactiveValues(path = root_dirs[1])

    dir <- reactive(input$dir)

    output$dir <- renderText({
      search_dir$path
    })

    shinyDirChoose(
      input,
      'dir',
      roots = root_dirs,
      filetypes = c('Rproj', 'Rmd', 'R')
    )

    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$dir
                 },
                 handlerExpr = {
                   if (!'path' %in% names(dir())) return()

                   search_dir$path <- parseDirPath(root_dirs,
                                                   input$dir)
                 }
    )

    #### ---------- SEARCH DIR FOR .RPROJ FILES ---------- ####

    all_proj <- reactive({

      # Issue #14

      all_proj <- data.frame(path = dirRecur(search_dir$path),
                             stringsAsFactors = FALSE)

      if (!nrow(all_proj)) return(NULL)

      all_proj[!all_proj$project %in% input$old_name, ]
      all_proj$project <- ProjId(all_proj[[1]], 'proj')

      all_proj$last_modified  <- as.Date(file.info(all_proj[[1]])$mtime)
      all_proj$days_since_mod <- as.integer(Sys.Date() - all_proj$last_modified)
      all_proj$last_modified  <- format(all_proj$last_modified, '%b %Y')

      all_proj <- all_proj[order(all_proj$days_since_mod), ]

      all_proj[c(2:4, 1)]

    })

    #### ---------- OPEN PROJECT ---------- ####

    observeEvent(input$open_btn, {
      choice <- sub('\\s.*$', '', input$open_proj_choice)
      choice <- all_proj()[all_proj()$project == choice, 'path']

      rstudioapi::openProject(choice, input$new_session)
    })

    #### ---------- ADD CHOSEN PROJECTS ---------- ####

    add_choices <- reactive({
      choices <- sub('\\s.*$', '', input$proj_add_names)

      all_proj()[all_proj()$project %in% choices, ]
    })

    output$choice_table <- renderTable({
      if (!is.null(input$proj_add_names)) {
        tmp_df <- add_choices()
        tmp_df[['path']] <- dirname(tmp_df[['path']])
        tmp_df
      }

    })

    #### ---------- UPDATE MRU ---------- ####

    observeEvent(input$remove_btn, {
      current_mru(current_mru()[!current_mru() %in% input$selection])
    })

    observeEvent(input$add_btn, {
      temp_mru <- ProjId(add_choices()$path, 'full')
      names(temp_mru) <- ProjId(temp_mru, 'proj')
      current_mru({ c(temp_mru, current_mru()) })
    })

    #### ---------- RENAME PROJECT ---------- ####

    observeEvent(input$rename_btn, {

      # Rename .Rproj file and dir
      on <- all_proj()[all_proj()$project == input$old_name, 'path']
      nn <- gsub(input$old_name, input$new_name, on)
      file.rename(on, file.path(dirname(on), basename(nn)))  # rename file
      file.rename(dirname(on), dirname(nn))  # rename dir

      # Update mru
      temp_mru <- current_mru()

      temp_mru[input$old_name] <- gsub(
        input$old_name,
        input$new_name,
        temp_mru[input$old_name]
      )

      temp_mru <- ProjId(temp_mru, 'full')
      names(temp_mru) <- ProjId(temp_mru, 'proj')

      current_mru(temp_mru)
    })

    #### ---------- STOP APP AND SAVE CHANGES ---------- ####

    observeEvent(input$done, {
      writeLines(current_mru(), mru_path)
      stopApp()
    })

  }

  # paneViewer allows use of file tab to navigate folders
  viewer <- paneViewer(minHeight = 'maximize')

  runGadget(ui, server, viewer = viewer)

}
