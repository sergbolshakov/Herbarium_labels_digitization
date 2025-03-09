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
                                     all_files,
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
        new_base_name <- str_c("LE F-", current_number)
        three_word_name_pattern <- regex("^([A-Za-z]+)[ _]([A-Za-z-]+)[ _].+$")

        # Определение нового названия и паттерна для поиска связанных файлов
        # Если название файла состоит из трёх слов
        if (str_detect(current_file_name, three_word_name_pattern)) {
          # Разбиваю название на части по пробелу или подчёркиванию
          prefix <- str_split(current_file_name, "[ _]")[[1]]
          # Если в префиксе действительно есть не менее двух слов
          if (length(prefix) >= 2) {
            # Тогда они остаются в новом названии
            new_file_name <- str_c(prefix[1], " ", prefix[2], " ", new_base_name)
          # А если нет
          } else {
            # Тогда новое название будет равным полному номеру образца
            new_file_name <- new_base_name
          }
          # Регулярное выражение для поиска файлов
          # с такими же первыми двумя словами и полным номером образца
          new_file_name_pattern <- regex(
            str_c("^", prefix[1], "[ _]", prefix[2], "[ _]", new_base_name, "(_\\d+)?$"),
            ignore_case = TRUE
          )
        # Если название файла состоит одного или двух слов
        } else if (!str_detect(current_file_name, fixed(new_base_name))) {
          # Тогда новое название будет равным полному номеру образца
          new_file_name <- new_base_name
          # Регулярное выражение для поиска файлов с таким же номером образца в названии
          new_file_name_pattern <- regex(
            str_c("^LE\\s+F\\-", current_number, "(_\\d+)?$"),
            ignore_case = TRUE
          )
        # Если ничего не подходит, всё остаётся без изменений
        } else {
          new_file_name <- current_file_name
          new_file_name_pattern <- fixed(new_file_name)
        }

        # Получение списка всех файлов, которых нужно переименовать
        # Нужно выбрать файлы в названиях которых есть такой паттерн
        files_to_rename_1 <-
          all_files() %>%
          keep(~ {
            file_names <- path_file(.) %>% path_ext_remove()
            str_detect(file_names, new_file_name_pattern)
          })

        # Если в списке файлов нет с такими же названиями
        # Тогда в него надо добавить только те, чьи названия равны текущему
        files_to_rename_2 <- character(0)
        if (!current_file_path %in% files_to_rename_1) {
          files_to_rename_2 <-
            all_files() %>%
            keep(~ {
              file_names <- path_file(.) %>% path_ext_remove()
              file_names == current_file_name
            })
        }
        
        # Полный список файлов для переименования
        files_to_rename <- c(files_to_rename_1, files_to_rename_2)
        
        # Уникальные названия файлов для переименования
        unique_file_names_to_rename <-
          files_to_rename %>%
          path_ext_remove() %>%
          unique()

        # Проверка, нужно ли вообще переименовывать файлы
        all_files_already_named_correctly <- 
          all(
            map_lgl(files_to_rename, ~ {
              base_name <- path_file(.) %>% path_ext_remove()
              base_name == new_file_name
            })
          )
        
        # Если названия файлов не меняются, просто обновляю данные
        if (all_files_already_named_correctly) {
          data_updated <- data_updated
          
        # Если файлы нужно переименовать, 
        # и файлы с таким же номером в названии уже есть
        # устраиваю групповое переименование с добавлением суффиксов
        } else if (length(unique_file_names_to_rename) > 1) {
          # Порядок файлов для группового переименования определяется по времени изменения
          file_info <- file_info(files_to_rename)
          order_idx <- order(file_info$modification_time)
          ordered_files <- files_to_rename[order_idx]

          # (Уникальные) названия файлов без расширений
          base_names <- map_chr(ordered_files, ~ path_file(.) %>% path_ext_remove())
          unique_base_names <- unique(base_names)

          # Каждому уникальному названию соответствует номер группы
          base_name_to_group <- 
            setNames(
              seq_along(unique_base_names),
              unique_base_names
            )

          # Словарь для переименования:
          # файлы с одинаковым названием получают один и тот же суффикс
          rename_map <- tibble(
            file_path = ordered_files,
            old_base = base_names,
            group_suffix = map_int(base_names, ~ base_name_to_group[.]),
            new_name = str_c(new_file_name, "_", group_suffix)
          )

          # Групповое переименование
          walk(seq_len(nrow(rename_map)), function(i) {
            old_path <- rename_map$file_path[i]
            new_name <- rename_map$new_name[i]
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
          
          # Сообщение о переименовании
          showNotification("Файл(ы) был(и) автоматически переименован(ы)",
                           type = "message")
          
          # Обновление данных
          data_updated <-
            data_updated %>%
            mutate(imageFile = map_chr(imageFile, function(fname) {
              # Проверяю, есть ли такое название файла в словаре
              matches <- rename_map %>% filter(old_base == fname)
              if (nrow(matches) > 0) {
                # Беру первое совпадение, если их несколько
                return(matches$new_name[1])
              } else {
                # Если совпадений нет, оставляю исходное название
                return(fname)
              }
            }))
          
        # Если новое название уникальное, просто всех переименовываю
        } else {
          walk(files_to_rename, function(old_path) {
            ext <- path_ext(old_path)
            new_file_full <- str_c(new_file_name, ".", ext)
            new_path <- path(current_file_dir, new_file_full)
            file_move(old_path, new_path)
            # Обновление списка файлов в модуле выбора
            update_file(old_path, new_path)
            if (old_path == current_file_path) {
              current_file_new_path <<- new_path
            }
          })

          # Сообщение о переименовании
          showNotification("Файл(ы) был(и) автоматически переименован(ы)",
                           type = "message")

          # Обновление данных
          data_updated <-
            data_updated %>%
            mutate(imageFile = if_else(imageFile == current_file_name,
                                       new_file_name,
                                       imageFile))
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
