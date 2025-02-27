# Модуль просмотра изображений
# - отображает выбранное изображение
# - обеспечивает навигацию между изображениями

#' UI модуля просмотра изображений
#' @param id Идентификатор модуля
image_view_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      fluidRow(
        column(
          6,
          actionButton(ns("prev_image"),
                       "Назад",
                       class = "btn-primary"),
          align = "left"
        ),
        column(
          6,
          actionButton(ns("next_image"),
                       "Вперед",
                       class = "btn-primary"),
          align = "right",
        )
      ),
      style = "margin-bottom: 0px;"
    ),
    imageOutput(ns("image"), height = "auto")
  )
}

#' Server модуля просмотра изображений
#' @param id Идентификатор модуля
#' @param filtered_files Реактивное значение со списком файлов
#' @param selected_index Реактивное значение с индексом выбранного файла
#' @param set_index Функция для обновления индекса в модуле файлов
image_view_server <- function(id, filtered_files, selected_index, set_index) {
  
  moduleServer(id, function(input, output, session) {
   
    # Начальные реактивные значения
    rv <- reactiveValues(
      index = 1
    )
    
    # Обновление индекса при выборе файла
    observe({
      req(selected_index())
      rv$index <- selected_index()
    })
    
    # Навигация по изображениям
    observeEvent(input$prev_image, {
      req(filtered_files())
      if (rv$index > 1) {
        rv$index <- rv$index - 1
        # Пролистывание обновляет индекс в модуле файлов
        set_index(rv$index)
      }
    })
    
    observeEvent(input$next_image, {
      req(filtered_files())
      if (rv$index < length(filtered_files())) {
        rv$index <- rv$index + 1
        # Пролистывание обновляет индекс в модуле файлов
        set_index(rv$index)
      }
    })
    
    # Отображение изображения
    output$image <- renderImage({
      req(rv$index, filtered_files())
      list(
        src = filtered_files()[rv$index],
        contentType = str_c("image/", path_ext(filtered_files()[rv$index])),
        width = "100%",
        height = "auto"
      )
    }, deleteFile = FALSE)
    
    # Возврат текущего индекса
    reactive({ rv$index })
  })
  
}
