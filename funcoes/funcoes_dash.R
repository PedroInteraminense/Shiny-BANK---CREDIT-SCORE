info_tooltip <- function(mensagem = NULL, margin_top = 0, float = "right", lado_box = "left") {
  margin_top <- paste0("margin-top: ", margin_top, "px;")
  float <- paste0("float:", float, ";")
  style <- paste(float, margin_top)
  
  a(`data-toggle`="tooltip",
    `data-placement`=lado_box,
    `data-html`="true",
    style=style,
    class="tool-message",
    title=mensagem,
    tags$strong(icon("info-circle")))
}

# ----

box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE, 
                 background = NULL, width = 6, height = NULL, collapsible = FALSE, 
                 collapsed = FALSE, message = NULL, message_position = "right")  {
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    if (!is.null(message)) {
      messageTag <- a(`data-toggle`="tooltip",
                      `data-placement`= message_position,
                      `data-html`="true",
                      style= "float: left; margin-top: -1px; color: #2F4C9C; margin-left: -5px;",
                      class="tool-message",
                      title=message,
                      tags$strong(icon("info-circle")))
      
      titleTag <- h3(class = "box-title", messageTag, title)
    } else {
      titleTag <- h3(class = "box-title", title)
    }
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- div(class = "box-tools pull-right", tags$button(class = paste0("btn btn-box-tool"), 
                                                                   `data-widget` = "collapse", shiny::icon(collapseIcon)))
  }
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header", titleTag, collapseTag)
  }
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style)) 
      style, headerTag, div(class = "box-body", ...), if (!is.null(footer)) 
        div(class = "box-footer", footer)))
}


# ----

loader <- function(element) {
  element %>% 
    withSpinner(type = getOption("spinner.type", default = 6), color = "#050f29")
  
}


# ---------------


hctreemap2 <- 
  function (data, group_vars, size_var, color_var = NULL, ...) {
    assertthat::assert_that(is.data.frame(data))
    assertthat::assert_that(is.character(group_vars))
    assertthat::assert_that(is.character(size_var))
    if (!is.null(color_var)) 
      assertthat::assert_that(is.character(color_var))
    group_syms <- rlang::syms(group_vars)
    size_sym <- rlang::sym(size_var)
    color_sym <- rlang::sym(ifelse(is.null(color_var), size_var, 
                                   color_var))
    if (data %>% select(!!!group_syms) %>% map(unique) %>% unlist() %>% 
        anyDuplicated()) 
      stop("Treemap data uses same label at multiple levels.")
    data <- data %>% mutate_at(group_vars, as.character)
    name_cell <- function(..., depth) paste0(list(...), 1:depth, 
                                             collapse = "")
    data_at_depth <- function(depth) {
      data %>% group_by(!!!group_syms[1:depth]) %>% summarise(value = sum(!!size_sym), 
                                                              colorValue = sum(!!color_sym)) %>% ungroup() %>% 
        mutate(name = !!group_syms[[depth]], level = depth) %>% 
        mutate_at(group_vars[1:depth], as.character) %>% {
          if (depth == 1) 
            mutate(., id = paste0(name, 1))
          else {
            mutate(., parent = pmap_chr(list(!!!group_syms[1:depth - 
                                                             1]), name_cell, depth = depth - 1), id = paste0(parent, 
                                                                                                             name, depth))
          }
        }
    }
    
    treemap_df <- 1:length(group_vars) %>% map(data_at_depth) %>% 
      bind_rows()
    data_list <- treemap_df %>% highcharter::list_parse() %>% 
      purrr::map(~.[!is.na(.)])
    colorVals <- treemap_df %>% filter(level == length(group_vars)) %>% 
      pull(colorValue)
    highchart() %>% hc_add_series(data = data_list, type = "treemap", 
                                  allowDrillToNode = TRUE, ...) %>% hc_colorAxis(min = min(colorVals), 
                                                                                 max = max(colorVals), enabled = TRUE)
  }



