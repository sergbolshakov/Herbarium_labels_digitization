# Модуль выбора директории и файлов
# - управляет выбором директории 
# - отображает список файлов изображений
# - обеспечивает фильтрацию и выбор файлов

#' UI модуля директории и файлов
#' @param id Идентификатор модуля
directory_files_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyDirButton(ns("dir_btn"),
                   "Выбрать папку с изображениями",
                   "Загрузить изображения"),
    uiOutput(ns("file_search_input")),
    div(
      DTOutput(ns("file_list")),
      style = "height: calc(100vh - 200px); overflow-y: hidden;"
    )
  )
}

#' Server модуля выбора директории и файлов
#' @param id Идентификатор модуля
#' @param data Реактивное значение с данными из data.tsv
directory_files_server <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    # Начальные реактивные значения
    rv <- reactiveValues(
      image_dir = NULL,
      filtered_files = NULL,
      current_index = 1
    )
    
    # Инициализация выбора директории
    volumes <- c(
      "Микология на DB-Store" = "\\\\DB-Store\\Микология\\MycoHerb_LE\\Оцифровка",
      getVolumes()()
    )
    shinyDirChoose(input, "dir_btn", roots = volumes)
    
    # Обработчик выбора директории
    observeEvent(input$dir_btn, {
      req(is.list(input$dir_btn))
      selected_path <- parseDirPath(volumes, input$dir_btn)
      rlang::try_fetch(
        {
          if (length(selected_path) > 0 && dir.exists(selected_path)) {
            rv$image_dir <- selected_path
          }
        },
        error = function(cnd) {
          showNotification("Невозможно получить доступ к выбранной папке!", 
                           type = "error")
        }
      )
    })
    
    # Получение списка файлов изображений
    image_files <- reactive({
      req(rv$image_dir)
      dir_ls(rv$image_dir) %>% 
        str_subset(regex("\\.(jpg|jpeg|png|gif|bmp|tif|tiff|webp)$", 
                         ignore_case = TRUE)) %>% 
        as.character()
    })
    
    # Форма для поиска создаётся только после получения списка файлов
    output$file_search_input <- renderUI({
      req(image_files())
      textInput(session$ns("file_search"), "Поиск по названию файла")
    })
    
    # Фильтрация файлов
    filtered_files <- reactive({
      files <- image_files()
      req(files)
      
      # Если file_search_input не существует или пустое, вернуть все файлы
      if (is.null(input$file_search) || !nzchar(input$file_search)) {
        return(files)
      }
      
      str_subset(files, regex(input$file_search, ignore_case = TRUE))
    })
    
    # Обновление отфильтрованных файлов
    observe({
      rv$filtered_files <- filtered_files()
    })
    
    # Прокси для таблицы
    dt_proxy <- dataTableProxy("file_list")
    
    # Отображение списка файлов
    output$file_list <- renderDT({
      req(rv$filtered_files, rv$image_dir)
      dir_path <- as.character(rv$image_dir)
      datatable(
        tibble(!!dir_path := path_file(rv$filtered_files)),
        selection = "single",
        options = list(
          searching = FALSE,
          paging = FALSE,
          scrollY = "calc(100vh - 250px)",
          info = FALSE,
          scrollToSelection = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Обновление выделения при изменении индекса
    observe({
      req(rv$current_index)
      selectRows(dt_proxy, rv$current_index)
    })
    
    # Обработчик выбора строки в таблице
    observeEvent(input$file_list_rows_selected, {
      rv$current_index <- input$file_list_rows_selected
    })
    
    # Возврат реактивных значений
    list(
      filtered_files = reactive({ rv$filtered_files }),
      selected_index = reactive({ rv$current_index }),
      set_index = function(index) {
        rv$current_index <- index
      }
    )
    
  })
  
}
