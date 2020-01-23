

mrup <- function() {

  library(shiny)
  library(miniUI)
  library(shinyFiles)

  ####---------- MODULES ----------####

selectDirUI <- function(input, id, roots, session = session) {
    ns <- NS(id)
    tagList(
      shinyDirLink(ns('dir'), '(Change?)', 'Locate project folder...'),
      verbatimTextOutput(ns('dir'))
    )
  }

#------------------#

  selectDirServ <- function(input, output, session, dir, search_dir) {


    observeEvent(ignoreNULL = TRUE,
                 eventExpr = {
                   input$dir
                 },
                 handlerExpr = {

                   shinyDirChoose(
                     input,
                     'dir',
                     roots = root_dirs,
                     filetypes = c('Rproj', 'Rmd', 'R')
                   )

                   if (!'path' %in% names(dir)) return()

                   root <- root_dirs[dir$root]

                   search_dir$path <-
                     file.path(root, paste0(unlist(dir$path[-1]), collapse = .Platform$file.sep))
                 }
    )
    output$dir <- renderText({
      search_dir$path
    })
  }





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

  root_dirs <- c(`Git repos` = normalizePath('~/R/ProjectDir/git_repos', .Platform$file.sep), '/R' = normalizePath('~/R'))
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

      #### ---------- ADD PROJECT TABPANEL ---------- ####
      # See issue #11 - maybe no needed anymore?
      miniTabPanel('Add project to list', icon = icon('plus-circle'),
                   miniContentPanel(
                     strong('Add projects to recent project list'),
                     uiOutput('add_proj_ui')
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

        # shinyDirLink('dir', '(Change?)', 'Browse...'),
        # verbatimTextOutput('dir'),

        strong('Open a project, not currently on the "Recently used" list, to open'),

        p('Any project in the chosen directory can be opened'),

        selectDirUI(input = input, id = 'open', roots = root_dirs),
        selectInput(
          'open_proj_choice', 'Projects',

          choices = c('Choose...' = '', proj_no_mru()),
          selectize = TRUE,
          multiple = FALSE,
          width = '100%'),

        checkboxInput('new_session',
                      'Open in new RStudio session?',
                      value = TRUE),
        actionButton('open_btn', 'Open', class = 'mru_btn')
      )
    })

    #### ---------- ADD-PROJECT UI ---------- ####

    proj_no_mru <- reactive({
      # could be conoslidated
      x <- all_proj()[!all_proj()$on_list, ]
      sprintf('%s (%s days)', x[['project']], x[['days_since_mod']])
    })

    output$add_proj_ui <- renderUI({

      list(

        p('Projects are listed in order of days since their last modification.'),
        p('Additions are placed at the top of the recent projects list.'),

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

    #### ---------- CHOOSE SEARCH DIR ---------- ####

    search_dir <- reactiveValues(path = root_dirs[1]) ## in server?
    dir <- reactive(input$dir)  # in server module?

    callModule(selectDirServ, id = 'open', dir = dir(), search_dir = search_dir)

    # shinyDirChoose(
    #   input,
    #   'dir',
    #   roots = root_dirs,
    #   filetypes = c('Rproj', 'Rmd', 'R')
    # )
    #
    #
    # output$dir <- renderText({
    #   search_dir$path
    # })
    #
    # observeEvent(ignoreNULL = TRUE,
    #              eventExpr = {
    #                input$dir
    #              },
    #              handlerExpr = {
    #                if (!'path' %in% names(dir())) return()
    #
    #
    #                root <- root_dirs[dir()$root]
    #
    #                search_dir$path <-
    #                  file.path(root, paste0(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
    #              }
    # )

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
      all_proj$on_list <- all_proj$project %in% names(current_mru()[1:10])

      # all_proj$modified       <- paste0(
      #   all_proj$last_modified, ' (',
      #   all_proj$days_since_mod, ' days)'
      # )

      all_proj <- all_proj[order(all_proj$days_since_mod), ]
      all_proj[c(2:4, 1, 5)]

    })

    #### ---------- OPEN PROJECT ---------- ####

    observeEvent(input$open_btn, {
      choice <- sub('\\s.*$', '', input$open_proj_choice)
      choice <- project_data[project_data$project == choice, 'path']

      rstudioapi::openProject(choice, input$new_session)
    })

    #### ---------- ADD CHOSEN PROJECTS ---------- ####

    add_choices <- reactive({
      choices <- sub('\\s.*$', '', input$proj_add_names)

      all_proj()[all_proj()$project %in% choices, ]
    })

    output$choice_table <- renderTable({
      if (!is.null(input$proj_add_names)) {
        tmp_df <- add_choices()[-5]
        tmp_df[['path']] <- dirname(tmp_df[['path']])
        setNames(tmp_df, c('Project', HTML('Accessed'), HTML('Days'), 'Path'))
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
