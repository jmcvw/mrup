#' Manage recent projects
#'
#' An add-in for managing the RStudio recently used projects list. It allows
#' projects to be added to, or removed from the recent project drop-down menu.
#' It also makes it possible to rename existing projects without navigating file
#' system manually.
#'
#' The three functions of the app are each accessed through a dedicated tab at
#' the bottom of the window.
#'
#' @section Remove tab:
#'
#'   The RStudio menu shows a list of the 10 most recently used projects. This
#'   is based on the project_mru file which may contain more than 10 projects
#'   (apparently 15 max?). The remove tab lists the entire contents of the
#'   project_mru file, and allows any of them to be removed. The benefit of this
#'   is that is is possible to see which projects were recently removed.
#'
#'   Pressing the \code{Remove} button creates a modified list to replace the
#'   current list. Replacement only takes place if the \code{Save changes}
#'   button is pressed.
#'
#' @section Add tab:
#'
#'   All subdirectories of the \code{~/R} directory are searched for
#'   \code{.Rproj} files, and will likely take a few seconds to complete. On
#'   Windows systems it is possible to specify a different directory (but see
#'   note below).
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
#'   \strong{NB:} Four points to be aware of when selecting a different directory to search.
#'
#'   \enumerate{
#'     \item{It may take several seconds to locate all project files (even in the default location).}
#'     \item{Sometimes the "Choose directory" dialogue windows opens behind the RStudio window.}
#'     \item{Pressing the link more than once will cause several "Choose dir" windows to open.}
#'       \item{Pressing cancel on the choose directory
#'   dialogue window currently triggers an error.
#'       \itemize{
#'         \item{It can can be dealt with by refreshing the app (the refresh button at top right).}
#'     }
#'   }
#'}
#'
#' @section Rename tab:
#'
#'   Because searching for all files takes some time, only projects already on
#'   the list can be renamed. Using the refresh button allows the list to be
#'   updated without restarting the add-in. When renaming, select 1 project and
#'   enter the new name in the box, without a file extension. As long as the
#'   project directory has the same name as the project itself, both shall be
#'   renamed, and the \code{.Rproj} extension will remain.
#'
#'   The name change is implemented immediately on pressing \code{Rename}, with
#'   no need to press \code{Save changes}, and without the ability to cancel. To
#'   undo a rename, the project can be re-renamed straight away.
#'
#'   There may be instances where the project has a name that is different from
#'   the containing directory. In this case only the \code{.Rproj} file (and
#'   therefore the preject itself) is renamed. If it is desired that the names
#'   be made the same, changing the \code{.Rproj} first to match the directory,
#'   then renaming again to the desired name will result in both the directory
#'   and project aquiring the new name.
#'
#' @details
#'
mrup <- function() {

  library(shiny)
  library(miniUI)

  # Helper Funs ----
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

  # Set mru path ----
  # Known possible locations of mru file
  # the 3rd path is required for cases where RStudio is installed on C:\
  # and the user home dir is on a different drive - ie my case!
  mru_path_opts <- c(
    file.path('~/AppData/Local/RStudio-Desktop/monitored/lists/project_mru'),
    file.path('C:/Users', Sys.info()['user'], 'AppData/Local/RStudio-Desktop/monitored/lists/project_mru'),
    file.path('~/.rstudio-desktop/monitored/lists/project_mru'),
    file.path('C:/Users', Sys.info()['user'], '.rstudio-desktop/monitored/lists/project_mru')
  )

  # choose path
  if (!any(file.exists(mru_path_opts)))
    stop('File "project_mru" not found')

  mru_path <- mru_path_opts[file.exists(mru_path_opts)]

  # ----------------------------------------------------------------------

  ui <- miniPage(

    # Style ----
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

    # Layout ----
    miniTitleBar(strong('Recently-Used Projects'),
                 left = miniTitleBarButton("done", "Save", primary = TRUE),
                 right = miniTitleBarCancelButton()
    ),

    miniTabstripPanel(
      between = p('See ', actionLink('help', code('?mrup')), ' for details'),

      # Remove project from list TabPanel ----
      miniTabPanel("Remove project from list", icon = icon('minus-circle'),
                   miniContentPanel(
                     uiOutput('remove_proj_ui')
                   )
      ),

      # Add project TabPanel ----
      # See issue #11 - maybe no needed anymore?
      miniTabPanel("Add project to list", icon = icon('plus-circle'),
                   miniContentPanel(
                     strong('Add projects to recent project list'),
                     uiOutput('add_proj_ui')
                   )
      ),

      # Rename project ----
      miniTabPanel("Rename project", icon = icon('file-text'),
                   miniContentPanel(
                     uiOutput('rename_proj')
                   )
      )
    )
  )

  # ----------------------------------------------------

  server <- function(input, output) {#, session) {

    current_mru <- reactiveVal({
      mru_list <- readLines(mru_path)
      names(mru_list) <- ProjId(mru_list)
      mru_list
    })

    search_dir <- reactiveVal({
        search_dir <- '~/R'
    })

    observeEvent(input$help, {
      if ('rstudioapi' %in% installed.packages())
        rstudioapi::previewRd(file.path(find.package('mrup'), "mrup.Rd"))
    })

    # Remove project UI ----
    output$remove_proj_ui <- renderUI({

      length_mru <- length(current_mru())

      list(

        strong('Remove projects for recent projects list'),

        p('Only the 10 most recently used projects are displayed in the projects list.'),

        if (length_mru != 10)
          p('RStudio currently has a record of the last ', length_mru, 'projects.'),

        checkboxGroupInput('selection',
                           'Current project list',
                           current_mru()),

        actionButton('remove_btn', 'Remove', class = 'mru_btn')
      )
    })

    # Read all .Rproj file paths ----
    all_proj <- reactive({

      # Issue #14

      all_proj <- data.frame(path = dirRecur(search_dir()),
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

    proj_no_mru <- reactive({
      all_proj()[!all_proj()$project %in% names(current_mru()[1:10]), ]
    })

    # Add project UI ----
    output$add_proj_ui <- renderUI({
      list(
        p('Projects are listed in order of days since their last modification.'),
        p('Additions are placed at the top of the recent projects list.'),

        p('By default the ', code('~/R'), ' driectory is searched.'),

        if (Sys.info()['sysname'] == 'Windows')
          p('Click ', actionLink('choose_dir', 'here'),
            ' to choose a different directory.'),

        selectInput(
          'proj_add_names', 'Projects',

          choices = paste0(proj_no_mru()[['project']], ' (', proj_no_mru()[['days_since_mod']], ' days)'),
          selectize = TRUE,
          multiple = TRUE),

        actionButton('add_btn', 'Add', class = 'mru_btn'),

        tableOutput('choice_table')
      )
    })


    # Rename project UI ----
    output$rename_proj <- renderUI({
      n <- min(length(current_mru()), 10)

      list(
        strong('Rename existing projects'),

        selectInput(
          'old_name', 'Choose project to rename',
          choices = ProjId(current_mru()[1:n], 'proj'),
          size = n,
          selectize = FALSE
        ),

        textInput('new_name', 'Enter new project name'),

        actionButton('rename_btn', 'Rename', class = 'mru_btn'),
        em('Takes place immediately, without pressing save.')
      )
    })

    # Add - chosen projects  ----
    add_choices <- reactive({
      choices <- sub(' \\([0-9]+ days).*$', '', input$proj_add_names)

      all_proj()[all_proj()$project %in% choices, ]
    })

    output$choice_table <- renderTable({
      if (!is.null(input$proj_add_names)) {
        add_choices()
      }

    })

    # Update mru ----
    observeEvent(input$remove_btn, {
      current_mru(current_mru()[!current_mru() %in% input$selection])
    })

    observeEvent(input$add_btn, {
      temp_mru <- ProjId(add_choices()$path, 'full')
      names(temp_mru) <- ProjId(temp_mru, 'proj')
      current_mru({ c(temp_mru, current_mru()) })
    })

    # Change search dir ----
    observeEvent(input$choose_dir, {
      search_dir({ choose.dir('~/R') })
    })

    observeEvent(input$rename_btn, {

      # Rename .Rproj file and dir
      on <- current_mru()[input$old_name]
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

    # Stop app & save changes ----
    observeEvent(input$done, {
      writeLines(current_mru(), mru_path)
      stopApp()
    })

  }

  # paneViewer allows use of file tab to navigate folders
  viewer <- paneViewer(minHeight = 'maximize')

  runGadget(ui, server, viewer = viewer)

}

