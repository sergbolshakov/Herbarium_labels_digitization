library(shiny)
library(bslib)
library(shinyjs)
library(shinyFiles)
library(fs)
library(DT)
library(tidyverse)
library(rlang)

ui <- page_fluid(
  theme = bs_theme(version = 5),
  useShinyjs(),
  
  layout_columns(
    card(directory_files_ui("files"),
         id = "files_card",
         full_screen = TRUE,
         style = "overflow: hidden;"),
    card(image_view_ui("image"),
         id = "image_card",
         full_screen = TRUE,
         style = "display: none;"),
    card(data_display_edit_ui("data"),
         id = "data_card",
         full_screen = TRUE,
         style = "display: none;"),
    col_widths = c(2, 8, 2),
    gap = "0"
  )
)

server <- function(input, output, session) {

  # Инициализация модуля выбора директории и файлов
  files <- directory_files_server("files")
  
  # Инициализация модуля просмотра изображений
  current_index <- image_view_server(
    "image",
    files$filtered_files,
    files$selected_index,
    files$set_index
  )
  
  # Инициализация модуля просмотра и редактирования данных
  data <- data_display_edit_server(
    "data",
    files$all_files,
    files$filtered_files,
    current_index,
    files$update_file,
    files$set_index
  )
  
  # После выбора директории включается отображение изображения и данных
  observeEvent(files$filtered_files(), {
    req(files$filtered_files())
    showElement("image_card")
    showElement("data_card")
  })
}

shinyApp(ui, server)
