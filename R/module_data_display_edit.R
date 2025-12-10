# Модуль просмотра и редактирования данных
# - отображает данные для выбранного файла
# - позволяет добавить или редактировать данные 
# - сохраняет изменения в файл данных на диске

#' UI модуля просмотра и редактирования данных
#' @param id Идентификатор модуля
data_display_edit_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        actionButton(ns("save_data"), 
                     "Сохранить", 
                     class = "btn-success", 
                     style = "display: none;"),
        align = "left"
      ),
      column(
        3,
        actionButton(ns("edit_data"), 
                     "Редактировать", 
                     class = "btn-warning"),
        align = "left"
      ),
      column(
        4,
        actionButton(ns("cancel_editing"), 
                     "Отменить", 
                     class = "btn-danger",
                     style = "display: none;"),
        align = "right"
      )
    ),
    div(
      uiOutput(ns("data_ui")),
      # Вписать по высоте и добавить scrollbar
      style = "height: calc(100vh - 100px); overflow-y: auto;"
    )
  )
}

#' Server модуля просмотра и редактирования данных
#' @param id Идентификатор модуля
#' @param data Реактивное значение с данными из data.tsv
#' @param filtered_files Реактивное значение со списком файлов
#' @param current_index Реактивное значение с текущим индексом выбранного файла
data_display_edit_server <- function(id, data, filtered_files, current_index) {
  
  moduleServer(id, function(input, output, session) {
    
    # Начальные реактивные значения
    rv <- reactiveValues(
      is_editing = FALSE,
      current_record = NULL
    )
    
    # Управление режимом редактирования кнопками
    observeEvent(input$edit_data, { rv$is_editing <- TRUE })
    observeEvent(input$cancel_editing, { rv$is_editing <- FALSE })
    
    # Генерация UI -------------------------------------------------------------
    output$data_ui <- renderUI({
      # Проверяю наличие списка файлов, индекса выбранного файла и данных
      req(filtered_files(), current_index(), data())
      
      # Получаю имя выбранного файла без расширения по его индексу
      current_file <- 
        filtered_files()[current_index()] %>%
        path_file() %>%
        path_ext_remove()
      
      # Ищу запись в данных с таким же названием файла
      record <- data() %>%
        filter(str_detect(current_file, catalogNumber))
      
      # Если запись для выбранного файла не найдена в данных
      if (nrow(record) == 0) {
        # Создаю новую пустую запись с такими же именами полей
        # и заполняю значения полей NA и именем выбранного файла
        record <- 
          data() %>%
          slice(0) %>%
          add_row(imageFile = current_file) %>%
          mutate(across(everything(), ~ NA_character_))
      }
      
      # Сохраняю текущую запись в реактивное значение
      rv$current_record <- record
      
      # Настройка отображения записи данных
      div(
        # В режиме редактирования
        if (rv$is_editing) {
          tagList(
            # Создаю для каждого поля записи формы для ввода данных
            map(names(record), ~ {
              # Определяю значения полей
              value <- case_when(
                # Автоматически заполняемые значения
                .x == "imageFile" ~ str_extract(current_file, "LE F-\\d{1,6}"),
                .x == "logCreatedWhen" ~ format(Sys.Date(), "%Y-%m-%d"),
                # Для остальных — существующее значение или пустая строка
                .default = coalesce(record[[.x]], "")
              )
              
              # Настройка форм для ввода данных
              if (.x == "catalogNumber") {
                # catalogNumber должен быть обязательно заполнен
                textAreaInput(
                  inputId = session$ns(.x),
                  label = span(
                    .x,
                    span("*", style = "color: red;")
                  ),
                  value = value,
                  placeholder = "Обязательное поле"
                )
              } else if (str_detect(.x, "^verbatim")) {
                # Все поля с оригинальным текстом выделены голубым
                tags$div(
                  style = "
                  border: 1px solid #007bff; 
                  border-radius: 2px; 
                  padding: 4px; 
                  background-color: #eaf4fb;
                  margin-bottom: 5px;
                  ",
                  textAreaInput(
                    inputId = session$ns(.x),
                    label = .x,
                    value = value,
                    placeholder = "Текстовое поле"
                  )
                )
              } else if (.x == "logCreatedWhen") {
                # Для logCreatedWhen ввод заблокирован с текущей датой
                tagAppendAttributes(
                  textAreaInput(
                    inputId = session$ns(.x),
                    label = .x,
                    value = value,
                    placeholder = "Введите значение"
                  ),
                  disabled = "disabled"
                )
              } else {
                # Для всех остальных полей форма для ввода обычная
                textAreaInput(
                  inputId = session$ns(.x),
                  label = .x,
                  value = value,
                  placeholder = "Введите значение"
                )
              }
            })
          )
          
        # В режиме просмотра
        } else {
         # Показываю только заполненные поля
          display_data <- 
            record %>%
            select(where(~ !is.na(.x) & .x != ""))
          
          # Если данных нет, показываю сообщение
          if (ncol(display_data) == 0) return(tags$i("Нет данных"))
          
          # Создаю список элементов для отображения данных
          tagList(
            # Контейнер для каждого поля
            imap(display_data, ~ tags$div(
              # Жирный заголовок
              tags$b(.y),
              # Значение с переносом длинных строк
              tags$div(.x, style = "word-wrap: break-word;"),
              # Отступ снизу для разделения полей
              style = "margin-bottom: 15px;"
            ))
          )
        },
        # Отступы от краев для всего контейнера
        style = "padding: 10px;"
      )
      
    })
    
    # Сохранение данных --------------------------------------------------------
    observeEvent(input$save_data, {
      # Проверяю наличие текущей записи и индекса выбранного файла
      req(rv$current_record, current_index())
      
      # Проверка заполнения обязательного поля
      if (is.null(input$catalogNumber) || str_trim(input$catalogNumber) == "") {
        showNotification("Поле 'catalogNumber' обязательно для заполнения!",
                         type = "error")
        return()
      }
      
      # Обработчик сохранения
      rlang::try_fetch(
        {
          # Создаю отредактированную запись:
          # 1. Прохожу по всем полям текущей записи
          # 2. Получаю новые значения из полей ввода
          # 3. Преобразую пустые значения в NA
          edited_record <- names(rv$current_record) %>%
            map_dfc(\(field_name) {
              value <- input[[field_name]]
              # Одноклеточный tibble с названием из имени поля ввода
              tibble(!!field_name := if_else(is.null(value) || value == "", 
                                             NA_character_,
                                             value))
            })
          
          # Обновляю или добавляю данные по ключу catalogNumber
          data(data() %>%
                 rows_upsert(edited_record, by = "catalogNumber"))
          
          # Сохраняю отредактированные данные в файл
          write_tsv(data(), "data.tsv", na = "", escape = "none")
          
          # Выключаю режим редактирования
          rv$is_editing <- FALSE
          
          # Уведомление о сохранении
          showNotification("Данные успешно сохранены", type = "message")
        },
        
        # Обработка ошибок при сохранении
        error = function(cnd) {
          showNotification(str_c("Ошибка сохранения: ", cnd$message),
                           type = "error")
        }
      )
    })
    
    # Управление видимостью кнопок ---------------------------------------------
    observe({
      toggle("save_data", condition = rv$is_editing)
      toggle("cancel_editing", condition = rv$is_editing)
      toggle("edit_data", condition = !rv$is_editing)
    })
    
    # Возврат статуса редактирования
    reactive({ rv$is_editing })
  })
}
