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
      style = "height: calc(100vh - 100px); overflow-y: auto;"
    )
  )
}

#' Server модуля просмотра и редактирования данных
#' @param id Идентификатор модуля
#' @param filtered_files Реактивное значение со списком файлов
#' @param current_index Реактивное значение с текущим индексом выбранного файла
#' @param update_file Функция обновления названия файла (из модуля выбора директории)
#' @param set_index Функция обновления выбранного индекса (из модуля выбора директории)
data_display_edit_server <- function(id,
                                     filtered_files,
                                     current_index,
                                     update_file,
                                     set_index) {
  
  moduleServer(id, function(input, output, session) {
    
    # Загрузка данных из data.tsv
    data <- reactiveVal()
    
    observe({
      # Обработчик возможных ошибок при загрузке файла
      try_fetch(
        {
          if (file_exists("data.tsv")) {
            data(read_tsv("data.tsv", 
                          col_types = cols(.default = col_character())))
          } else {
            showNotification("Файл data.tsv не найден в корне проекта!", 
                             type = "error")
          }
        },
        # Если при чтении файла возникла ошибка
        error = function(cnd) {
          showNotification(
            sprintf("Ошибка при загрузке data.tsv: %s", cnd$message),
            type = "error"
          )
        }
      )
    })
    
    # Начальные реактивные значения
    rv <- reactiveValues(
      is_editing = FALSE,
      current_record = NULL
    )
    
    # Управление режимом редактирования кнопками
    observeEvent(input$edit_data, { rv$is_editing <- TRUE })
    observeEvent(input$cancel_editing, { rv$is_editing <- FALSE })
    
    # Генерация UI =============================================================
    output$data_ui <- renderUI({
      # Проверка наличия списка файлов, индекса выбранного файла и данных
      req(filtered_files(), current_index(), data())
      
      # Получаю название выбранного файла без расширения по его индексу
      current_file <- 
        filtered_files()[current_index()] %>% 
        path_file() %>% 
        path_ext_remove()
      
      # Ищу запись в данных с таким же названием файла
      record <- 
        data() %>% 
        filter(imageFile == current_file)
      
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
                .x == "imageFile" ~ current_file,
                .x == "logCreatedWhen" ~ format(Sys.Date(), "%Y-%m-%d"),
                # Для остальных — существующее значение или пустая строка
                .default = coalesce(record[[.x]], "")
              )
              
              # Настройка форм для ввода данных
              if (.x == "catalogNumber") {
                # catalogNumber должен быть обязательно заполнен
                textAreaInput(
                  inputId = session$ns(.x),
                  label = span(.x, span("*", style = "color: red;")),
                  value = value,
                  placeholder = "Обязательное поле"
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
    
    # Сохранение данных и переименование файлов ================================
    observeEvent(input$save_data, {
      # Проверка наличия текущей записи и индекса выбранного файла
      req(rv$current_record, current_index())
      
      # Проверка заполнения обязательного поля
      if (is.null(input$catalogNumber) || str_trim(input$catalogNumber) == "") {
        showNotification("Поле 'catalogNumber' обязательно для заполнения!",
                         type = "error")
        return()
      }
      
      # Обработчик переименования файлов и сохранения данных
      try_fetch({
        # Обновление данных отредактированной записью --------------------------
        edited_record <- 
          names(rv$current_record) %>%
          map_dfc(\(field_name) {
            value <- input[[field_name]]
            tibble(!!field_name := if_else(is.null(value) || value == "", 
                                           NA_character_,
                                           value))
          })
        
        # Обновляю или добавляю данные по ключу imageFile
        data_updated <- data() %>%
          rows_upsert(edited_record, by = "imageFile")
        
        # Переименование файлов ------------------------------------------------
        current_file_path <- filtered_files()[current_index()]
        # По умолчанию новый путь равен старому
        current_file_new_path <- current_file_path
        current_file_name <- 
          path_file(current_file_path) %>% 
          path_ext_remove()
        current_file_dir <- path_dir(current_file_path)
        current_number <- str_trim(input$catalogNumber)
        new_base <- str_c("LE F-", current_number)
        three_word_pattern <- regex("^([A-Za-z]+)[ _]([A-Za-z-]+)[ _].+$")
        
        # Если название файла состоит из трёх слов
        if (str_detect(current_file_name, three_word_pattern)) {
          # Разбиваю название на части по пробелу или подчёркиванию
          parts <- str_split(current_file_name, "[ _]")[[1]]
          if (length(parts) >= 2) {
            prefix1 <- parts[1]
            prefix2 <- parts[2]
            candidate <- str_c(prefix1, " ", prefix2, " ", new_base)
          } else {
            candidate <- new_base
          }
          # Регулярное выражение для поиска файлов 
          # с теми же первыми двумя словами и номером образца
          pattern_candidate <- 
            regex(
              str_c("^", prefix1, "[ _]", prefix2, "[ _]", new_base, "(_\\d+)?$"),
              ignore_case = TRUE
            )
          
          # Поиск таких файлов
          existing_files <- 
            dir_ls(current_file_dir) %>% 
            keep(~ {
              file_names <- 
                path_file(.) %>% 
                path_ext_remove()
              str_detect(file_names, pattern_candidate)
            })
          if (!current_file_path %in% existing_files)
            existing_files <- c(existing_files, current_file_path)
          
          # Если файлы с таким номером уже есть
          if (length(existing_files) > 1) {
            # Сортировка чтобы сохранить исходный порядок
            ordered_files <- sort(existing_files)
            # Групповое переименование с добавлением суффиксов
            rename_map <- tibble(
              old = map_chr(ordered_files, ~ path_file(.) %>% path_ext_remove()),
              new = str_c(candidate, "_", seq_along(ordered_files))
            )
            walk2(ordered_files, rename_map$new, function(old_path, new_name) {
              ext <- path_ext(old_path)
              new_file_name <- str_c(new_name, ".", ext)
              new_path <- path(current_file_dir, new_file_name)
              file_move(old_path, new_path)
              # Обновление списка файлов в модуле выбора
              update_file(old_path, new_path)
              if (old_path == current_file_path) {
                current_file_new_path <<- new_path
              }
            })
            # Обновление данных
            data_updated <- 
              data_updated %>% 
              mutate(imageFile = map_chr(imageFile, function(fname) {
                new_val <- rename_map %>% filter(old == fname) %>% pull(new)
                if (length(new_val) == 0) fname else new_val
              }))
            
            # Если файл один такой, просто переименовываю его с сохранением первых двух слов
          } else {
            ext <- path_ext(current_file_path)
            new_file_full <- str_c(candidate, ".", ext)
            new_path <- path(current_file_dir, new_file_full)
            file_move(current_file_path, new_path)
            # Обновление списка файлов в модуле выбора
            update_file(current_file_path, new_path)
            # Обновление данных
            data_updated <- 
              data_updated %>% 
              mutate(imageFile = if_else(imageFile == current_file_name,
                                         candidate,
                                         imageFile))
            current_file_new_path <- new_path
          }
          
          # Сообщение о переименовании
          showNotification("Файл(ы) был(и) автоматически переименован(ы)",
                           type = "message")
          
        # Если название файла состоит одного или двух слов
        } else if (!str_detect(current_file_name, fixed(new_base))) {
          
          # Регулярное выражение для поиска файлов с таким же номером образца в названии
          regex_pattern <- regex(
            str_c("^LE\\s+F\\-", current_number, "(_\\d+)?$"),
            ignore_case = TRUE
          )
          # Поиск таких файлов
          existing_files <- 
            dir_ls(current_file_dir) %>% 
            keep(~ str_detect(path_file(.) %>% path_ext_remove(), regex_pattern))
          
          if (!current_file_path %in% existing_files)
            existing_files <- c(existing_files, current_file_path)
          
          # Если файлы с таким номером уже есть
          if (length(existing_files) > 1) {
            # Порядок файлов для группового переименования определяется по времени изменения
            file_info <- file_info(existing_files)
            order_idx <- order(file_info$modification_time)
            ordered_files <- existing_files[order_idx]
            
            # Групповое переименование с добавлением суффиксов
            rename_map <- tibble(
              old = map_chr(ordered_files, ~ path_file(.) %>% path_ext_remove()),
              new = str_c(new_base, "_", seq_along(ordered_files))
            )
            current_file_new_path <- NA_character_
            walk2(ordered_files, rename_map$new, function(old_path, new_name) {
              ext <- path_ext(old_path)
              new_file_name <- str_c(new_name, ".", ext)
              new_path <- path(current_file_dir, new_file_name)
              file_move(old_path, new_path)
              # Обновление списка файлов в модуле выбора
              update_file(old_path, new_path)
              if (old_path == current_file_path) {
                current_file_new_path <<- new_path
              }
            })
            if (is.na(current_file_new_path)) {
              current_file_new_path <- path(current_file_dir, str_c(
                new_base, ".", path_ext(current_file_path)
              ))
            }
            
            # Обновление данных
            data_updated <- 
              data_updated %>% 
              mutate(imageFile = map_chr(imageFile, function(fname) {
                new_val <- rename_map %>% filter(old == fname) %>% pull(new)
                if (length(new_val) == 0) fname else new_val
              }))
          } else {
            ext <- path_ext(current_file_path)
            new_file_full <- str_c(new_base, ".", ext)
            new_path <- path(current_file_dir, new_file_full)
            file_move(current_file_path, new_path)
            update_file(current_file_path, new_path)
            data_updated <- 
              data_updated %>% 
              mutate(imageFile = if_else(imageFile == current_file_name,
                                         new_base,
                                         imageFile))
            current_file_new_path <- new_path
          }
          # Сообщение о переименовании
          showNotification("Файл(ы) был(и) автоматически переименован(ы)",
                           type = "message")
        }
        
        # Обновление реактивных данных и сохранение в файл ---------------------
        data(data_updated)
        write_tsv(data_updated, "data.tsv", na = "", escape = "none")
        
        # Устанавливаю текущий индекс
        set_index(current_index())
        
        # Выключаю режим редактирования
        rv$is_editing <- FALSE
        
        # Сообщение о сохранении
        showNotification("Данные успешно сохранены", type = "message")
        
      },
      
      # Обработка ошибок при сохранении
      error = function(cnd) {
        showNotification(str_c("Ошибка сохранения: ", cnd$message),
                         type = "error")
      })
      
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
