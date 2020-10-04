#' Manage recent projects
#'
#' An add-in for managing the RStudio \emph{Recently Used Projects} list. It
#' simplifies opening projects that do not appear in the RStudio projects
#' drop-down menu as well as projects to be added to, or removed from the recent
#' project drop-down menu. It also makes it possible to rename existing projects
#' without manually navigating the file system
#'
#' The four functions of the app are each accessed through a dedicated tab at
#' the bottom of the window.
#'
#' @section Open tab:
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
#'   no need to press \code{Save}, and without the ability to cancel. To undo a
#'   rename, the project can be re-renamed straight away. Pressing \code{save}
#'   is required to update the RStudio drop down menu. In order to rename the
#'   same project more than once (e.g. to restore the original name) it is
#'   currently necessary to reload the app by pressing the reload button at the
#'   top right of the viewer pane.
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
#' @import shiny
#' @import miniUI
#'
mrup <- function() {

  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("mrup requires RStudio v0.99.878 or newer",
         call. = FALSE)
  }


  mru_path <- mru_path_opts[file.exists(mru_path_opts)][1]

  # choose path
  if (!length(mru_path))
    stop('File "project_mru" not found')

  root_dirs <- find_roots()

  #### ---------- UI ---------- ####
  ui <- miniPage(

    #### ---------- LAYOUT ---------- ####
    mrup_style,

    miniTitleBar(strong('Recently-Used Projects'),
                 left = miniTitleBarButton('done', 'Save', primary = TRUE),
                 right = miniTitleBarCancelButton()
    ),

    miniTabstripPanel(
      between = p('See ', actionLink('help', code('?mrup')), ' for details'),

      ####---------- UI TABS ----------####
      miniTabPanel('Open project', icon = icon('folder-open'),
                   miniContentPanel(
                     uiOutput('open_proj_ui')
                   )
      ),

      miniTabPanel('Remove project from list', icon = icon('minus-circle'),
                   miniContentPanel(
                     uiOutput('remove_proj_ui')
                   )
      ),

      miniTabPanel('Add project to list', icon = icon('plus-circle'),
                   miniContentPanel(
                     uiOutput('add_proj_ui')
                   )
      ),

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

    ####---------- OPEN PROJECT UI ----------####

    output$open_proj_ui <- renderUI({

      tagList(

        validate(need(all_proj(), 'Compiling project list...')),

        strong('Current search directory. '),
        p('The selected directory will also be used for finding projects to rename or add to the MRU list'),
        shinyFiles::shinyDirLink('dir', '(Change?)', 'Browse...'),
        verbatimTextOutput('dir'),

        strong('Open a project, not currently on the "Recently used" list, to open'),

        p('Any project in the chosen directory can be opened'),

        selectInput(
          'open_proj_choice', 'Projects',

          choices = c('Choose...' = '', simplified_mru()),
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

    ####---------- ADD PROJ UI ----------####

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

          choices = c('Choose...' = '', simplified_mru()),
          selectize = TRUE,
          multiple = TRUE,
          width = '100%'),

        actionButton('add_btn', 'Add', class = 'mru_btn'),

        tableOutput('choice_table')
      )
    })

    ####---------- RENAME PROJ UI ----------####

    output$rename_proj <- renderUI({

      n <- min(length(current_mru()), 1) # 10) # to set size of select input box
      choices <- isolate({ all_proj()$project })

      tagList(
        strong('Rename existing projects'),

        p('To rename the same project more than once
        it is necessary to reload the app after each rename
        (click the reload icon, top right)'),

        selectInput(
          'old_name', 'Choose project to rename',
          choices = c('Choose...' = '', choices),
          width = '100%'
        ),

        textInput('new_name', 'Enter new project name',
                  width = '100%'),

        actionButton('rename_btn', 'Rename', class = 'mru_btn'),
        p(),
        em('Press save to also update the RStudio dropdown menu.'),
      )
    })

    ####---------- REACTIVES ----------####

    search_dir <- reactiveValues(path = root_dirs[1])

    dir <- reactive(input$dir)

    current_mru <- reactiveVal({
      mru_list <- readLines(mru_path)
      names(mru_list) <- ProjId(mru_list)
      mru_list
    })

    all_proj <- reactive({
      compile_proj(search_dir$path, input$old_name)
    })

    simplified_mru <- reactive({
      simplify_mru(all_proj, current_mru)
    })


    # Add proj choices
    add_choices <- reactive({
      choices <- sub('\\s.*$', '', input$proj_add_names)

      all_proj()[all_proj()$project %in% choices, ]
    })

    ####---------- OUTPUTS ----------####


    output$choice_table <- renderTable({
      if (!is.null(input$proj_add_names)) {
        tmp_df <- add_choices()
        tmp_df[['path']] <- dirname(tmp_df[['path']])
        tmp_df
      }
    })

    # Dir choose
    output$dir <- renderText({
      search_dir$path
    })

    shinyFiles::shinyDirChoose(
      input,
      'dir',
      roots = root_dirs,
      filetypes = c('Rproj', 'Rmd', 'R')
    )

    ####---------- OBSERVERS ----------####

    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$dir
                 },
                 handlerExpr = {
                   if (!'path' %in% names(dir())) return()

                   # <<<<<<< HEAD
                   search_dir$path <- shinyFiles::parseDirPath(root_dirs,
                                                               input$dir)
                   # =======
                   #                    root <- root_dirs[dir()$root]
                   #
                   #                    search_dir$path <-
                   #                      file.path(root, paste0(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                   # >>>>>>> master
                 }
    )

    observeEvent(input$help, {
      rstudioapi::previewRd(system.file('mrup.Rd', package = 'mrup'))
    })

    observeEvent(input$open_btn, {
      choose_proj_to_open(all_proj, input$open_proj_choice, input$new_session)
    })

    observeEvent(input$remove_btn, {
      remove_proj_from_mru(input$selection, current_mru)
    })

    observeEvent(input$add_btn, {
      add_proj_to_mru(add_choices, current_mru)
    })

    observeEvent(input$rename_btn, {
      rename_proj(all_proj, current_mru,
                  input$old_name, input$new_name)
    })

    observeEvent(input$done, {
      writeLines(current_mru(), mru_path)
      stopApp()
    })

  }

  viewer <- paneViewer(minHeight = 'maximize')
  runGadget(ui, server, viewer = viewer)
}
