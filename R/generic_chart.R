#' @name generic_chart_server
#' 
#' @title Generic Chart Shiny Module (Server)
#' 
#' @description
#' Server-side logic for a fully generic and configurable charting module.
#' The module supports multiple plot types (time series, bars, stacked areas,
#' distributions, rankings, composition charts), time granularities,
#' aggregation statistics (sum or mean), optional error computation,
#' internationalization (i18n), and data export.
#'
#' The server:
#' \itemize{
#'   \item Normalizes and aggregates heterogeneous tabular data
#'   \item Handles time-based aggregation (monthly / yearly)
#'   \item Computes optional statistical errors (SD, SE, CI95)
#'   \item Dynamically dislay data with the appropriate Plotly chart type
#'   \item Provides tabular summaries and download capabilities
#' }
#'
#' @param id Character string. Module namespace identifier.
#' 
#' @param lang Optional language parameter.
#' Can be either:
#' \itemize{
#'   \item a character string (static language), or
#'   \item a reactive returning a character string (dynamic language)
#' }
#' If \code{NULL}, the current global language is used.
#' Default is \code{NULL}
#' 
#' @param df A reactive or static \code{data.frame} containing the input data.
#'
#' @param col_date Character string, name of the date column in \code{df}.
#'
#' @param col_group Character string, name of the grouping column (categorical variable).
#'
#' @param col_value Character string, name of the numeric value column.
#'
#' @param time_label Character string used as x-axis label for time-based charts.
#' Default is \code{"Date"}.
#'
#' @param value_label Character string used as y-axis label for numeric values.
#' Default is \code{"Value"}.
#'
#' @param group_label Character string used as label for grouping variables.
#' Default is \code{"Group"}.
#'
#' @param plot_types Optional character vector restricting the set of plot types available to the user.
#'
#' Supported values are:
#' \itemize{
#'   \item \code{"line"} – Multi-series line chart showing the evolution of each group over time.
#'   \item \code{"line_sum"} – Line chart displaying the total aggregated value (sum over all groups) through time.
#'   \item \code{"line_mean"} – Line chart displaying the mean value over time, with optional error bands.
#'   \item \code{"area_stack"} – Stacked area chart showing absolute contributions of each group over time.
#'   \item \code{"area_stack_pct"} – Stacked area chart normalized to percentages, highlighting relative group contributions over time.
#'   \item \code{"bar_mean"} – Bar chart showing mean values per time period, with optional error bars.
#'   \item \code{"bar_stack"} – Stacked bar chart displaying absolute group contributions per period.
#'   \item \code{"bar_stack_pct"} – Stacked bar chart normalized to percentages for relative comparison between groups.
#'   \item \code{"rank_sum"} – Ranking bar chart displaying the top 10 contributing groups based on total aggregated values.
#'   \item \code{"heatmap"} – Heatmap showing the intensity of values across time periods and groups.
#'   \item \code{"bubble"} – Bubble chart where bubble size represents value magnitude across time and groups.
#'   \item \code{"pie"} – Pie chart showing the overall distribution of values among groups.
#'   \item \code{"donut"} – Donut chart variant of the pie chart, emphasizing proportional composition.
#'   \item \code{"treemap"} – Treemap visualization showing hierarchical proportions of groups based on aggregated values.
#'   \item \code{"boxplot"} – Boxplot showing the distribution and variability of raw values per group.
#' }
#'
#' If \code{NULL}, the user can select among all plot types allowed by the aggregation statistic.
#'
#' @param plot_type_default Optional character string defining the default plot type selected in the plot type selector.
#' Supported values are the same as \code{plot_types}.
#' If \code{NULL}, the first available plot type is selected.
#' 
#' @param time_choices Character vector defining allowed time granularities.
#' Default is \code{c("month", "year")}.
#'
#' @param stat Character string defining the aggregation statistic.
#' Supported values are:
#' \itemize{
#'   \item \code{"sum"}
#'   \item \code{"mean"}
#' }
#' Default is \code{"sum"}.
#'
#' @param error Character string defining the default error type for mean-based plots. 
#' Supported values are:
#' \itemize{
#'   \item \code{"none"}
#'   \item \code{"sd"}
#'   \item \code{"se"}
#'   \item \code{"ci95"}
#' }
#' Default is \code{"none"}.
#' 
#' @param rank_number Integer. Specific to \code{rank_sum} mode.
#' Defines the number of ranked items to display.
#' Default is \code{10}.
#'
#' @param rank_target_id Optional character string. Specific to \code{rank_sum} mode.
#' Must match one of the values of the grouping column defined in \code{col_group}.
#' If \code{NULL}, the chart displays the top \code{rank_number} ranked items.
#' If provided, the chart centers the view around the targeted item and displays up to \code{rank_number} items around it.
#' Default is \code{NULL}.
#'
#' @param rank_target_color Optional character string. Specific to \code{rank_sum} mode.
#' Must be a valid R color name or hexadecimal color code.
#' Defines the fill color used to highlight the targeted item specified in \code{rank_target_id}.
#' Default is \code{"orange"}.
#'@export

generic_chart_server <- function(
    id,
    lang = NULL,
    df,
    col_date,
    col_group,
    col_value,
    time_label = "Date",
    value_label = "Value",
    group_label = "Group",
    plot_types = NULL,
    plot_type_default = NULL,
    time_choices = c("month","year"),
    stat = "sum",    
    error = "none",
    rank_number = 10,
    rank_target_id = NULL,
    rank_target_color = "orange"
  ){
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    INFO("Generic Chart server - START")
    
    # -------------------------------------------------------------------------
    # i18n handling
    # - Supports static language (lang = "en")
    # - Supports reactive language (lang = reactive)
    # -------------------------------------------------------------------------
    i18n_translator <- reactive({
      if(is.reactive(lang)){
        set_translation_language(lang())
      }else{
        if(!is.null(lang)) set_translation_language(lang)
      }
      translator()
    })
    
    # Simple wrapper around translator$t()
    i18n <- function(key){
      i18n_translator()$t(key)
    }
    
    # -------------------------------------------------------------------------
    # Utilitary function
    # - Compute mean and optional error bounds
    # - Used for mean-based plots with SD / SE / CI95
    # - Ongoing development
    # -------------------------------------------------------------------------
    compute_stats <- function(x, error_type) {
      x <- x[!is.na(x)]
      
      n <- length(x)
      
      if (n == 0) return(c(mean = NA_real_, ymin = NA_real_, ymax = NA_real_))
      
      m <- mean(x)
      
      if (error_type == "sd") {
        sdv <- sd(x)
        return(c(mean = m, ymin = m - sdv, ymax = m + sdv))
      } else if (error_type == "se") {
        sdv <- sd(x)
        se <- sdv / sqrt(n)
        return(c(mean = m, ymin = m - se, ymax = m + se))
      } else if (error_type == "ci95") {
        sdv <- sd(x)
        se <- sdv / sqrt(n)
        ci <- qnorm(0.975) * se
        return(c(mean = m, ymin = m - ci, ymax = m + ci))
      } else {
        return(c(mean = m, ymin = NA_real_, ymax = NA_real_))
      }
    }
    
    # -------------------------------------------------------------------------
    # Choices filtering (plot style, time granularity, error type)
    # -------------------------------------------------------------------------

    # Plot style
    all_plot_types <- setNames(
      c(
        "line","line_sum","line_mean",
        "area_stack","area_stack_pct",
        "bar_mean","bar_stack","bar_stack_pct",
        "rank_sum","heatmap","bubble",
        "pie","donut","treemap","boxplot"
      ),
      c(
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_LINE"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_LINE_TOTAL"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_LINE_MEAN"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_AREA"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_AREA_PERCENT"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_BAR_MEAN"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_BAR"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_BAR_PERCENT"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_RANK"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_HEATMAP"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_BUBBLE"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_PIE"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_DONUT"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_TREEMAP"),
        i18n("GENERIC_CHART_PLOT_STYLE_LABEL_BOXPLOT")
      )
    )
    
    # Allowed plot types depending on aggregation statistic
    plot_types_sum  <- c("line","line_sum","line_mean","area_stack","area_stack_pct",
                         "bar_mean","bar_stack","bar_stack_pct","rank_sum",
                         "heatmap","bubble","pie","donut","treemap","boxplot")
    
    plot_types_mean <- c("line","line_mean","bar_mean",
                         "heatmap","bubble","pie","donut","treemap","boxplot")
    
    if (stat == "sum") {
      allowed <- plot_types_sum
    } else if (stat == "mean") {
      allowed <- plot_types_mean
    } else {
      ERROR(i18n("GENERIC_CHART_MESSAGE_STAT"))
    }
    
    # Final list of selectable plot types
    plot_type_choices <- all_plot_types[all_plot_types %in% allowed]
    if (!is.null(plot_types)) {
      plot_type_choices <- plot_type_choices[plot_type_choices %in% plot_types]
    }
    
    #Time granularity
    granu_choices <- setNames(
      c("month","year"),
      c(
        i18n("GENERIC_CHART_PLOT_GRANU_LABEL_MONTH"),
        i18n("GENERIC_CHART_PLOT_GRANU_LABEL_YEAR")
        )
    )
    
    #Error bars and lines
    error_choices <- setNames(
      c("none","sd","se","ci95"),
      c(
        i18n("GENERIC_CHART_PLOT_ERROR_LABEL_NONE"),
        i18n("GENERIC_CHART_PLOT_ERROR_LABEL_SD"),
        i18n("GENERIC_CHART_PLOT_ERROR_LABEL_SE"),
        i18n("GENERIC_CHART_PLOT_ERROR_LABEL_CI95")
        )
    )
    
    # -------------------------------------------------------------------------
    # UI selectors (plot style, time granularity, error type)
    #  - Hide selector automatically when only one choice exists
    # -------------------------------------------------------------------------    
    
    #Plot style selector
    output$plot_style_wrapper <- renderUI({
      div(
        style = if (length(plot_type_choices) == 1) "display:none;" else NULL,
        selectInput(
          ns("plot_style"),
          paste0(i18n("GENERIC_CHART_PLOT_STYLE_TITLE"), " :"),
          choices  = plot_type_choices,
          selected = if (!is.null(plot_type_default)) plot_type_default else plot_type_choices[1]
        )
      )
    })
    
    #Time granularity selector
    output$granularity_wrapper <- renderUI({
      granu <- granu_choices[granu_choices %in% time_choices]
      div(
        style = if (length(granu) == 1) "display:none;" else NULL,
        selectInput(
          ns("granularity"),
          paste0(i18n("GENERIC_CHART_PLOT_GRANU_TITLE"), " :"),
          choices  = granu,
          selected = granu[2]
        )
      )
    })
    
    #Error selector
    output$error_wrapper <- renderUI({
      req(input$plot_style)
      div(
        style = if (!input$plot_style %in% c("line_sum","line_mean","bar_mean"))
          "display:none;" else NULL,
        selectInput(
          ns("error_type"),
          paste0(i18n("GENERIC_CHART_PLOT_ERROR_TITLE"), " :"),
          choices  = error_choices,
          selected = error
        )
      )
    })
    
    # -------------------------------------------------------------------------
    # Data formatting & aggregation
    # - Normalizes columns
    # - Applies time aggregation
    # - Computes sum or mean (+ errors)
    # -------------------------------------------------------------------------
    
    data_formatted <- reactive({
      req(df)
      gran <- ifelse(is.null(input$granularity), "year", input$granularity)
      err_type <- if (stat == "mean") {
        if (!is.null(input$error_type)) input$error_type else error
      } else {
        "none"
      }
      
      if(nrow(df)==0)return(NULL)
      
      df1 <- df |>
        rename(
          date  = !!rlang::sym(col_date),
          group = !!rlang::sym(col_group),
          raw_value = !!rlang::sym(col_value)   
        )
      switch(gran,
             "year" = {
               df1$period_date = as.Date(paste0(format(df1$date, "%Y"), "-01-01"))
               df1$period = format(df1$period_date, "%Y")
             },
             "month" = {
               df1$period_date = as.Date(paste0(format(df1$date, "%Y-%m"), "-01"))
               df1$period = format(df1$period_date, "%Y-%m")
             }
      )
      
      # Sum aggregation
      if (stat == "sum") {
        df_agg <- df1 |>
          group_by(period_date, period, group) |>
          summarise(value = sum(raw_value, na.rm = TRUE), .groups = "drop") |>
          arrange(period_date, group)
        
        df_agg$ymin <- NA_real_
        df_agg$ymax <- NA_real_
        return(df_agg)
      }
      
      # Mean + error aggregation
      df_stats <- df1 |>
        group_by(period_date, period, group) |>
        summarise(raws = list(raw_value), .groups = "drop") 
      
      df_stats <- df_stats |>
        rowwise() |>
        mutate(
          st = list(compute_stats(unlist(raws), err_type)),
          value = as.numeric(st["mean"]),
          ymin  = as.numeric(st["ymin"]),
          ymax  = as.numeric(st["ymax"])
        ) |>
        ungroup() |>
        select(period_date, period, group, value, ymin, ymax) |>
        arrange(period_date, group)
      
      return(df_stats)
    })
    
    # -------------------------------------------------------------------------
    # Plot generation logic
    # - Fills missing dates/groups
    # - Set axis label
    # - Generate appropriate Plotly chart
    # -------------------------------------------------------------------------
    
    plot_reactive <- reactive({
      
      d <- data_formatted()
      
      req(!is.null(d))
      req(nrow(d) > 0)
      
      style <- ifelse(
        is.null(input$plot_style), 
        if(is.null(plot_type_default)) names(plot_type_choices)[1] else plot_type_default, 
        input$plot_style
      )
      
      # Axis labels depending on plot family
      if (style %in% c("line","line_sum","line_mean","area_stack","area_stack_pct","bar_mean","bar_stack","bar_stack_pct")) {
        x_lab <- time_label; y_lab <- value_label
      } else if (style %in% c("heatmap","bubble")) {
        x_lab <- time_label; y_lab <- group_label
      } else if (style == "boxplot") {
        x_lab <- group_label; y_lab <- value_label
      } else {
        x_lab <- NULL; y_lab <- NULL
      }
      
      # Build full date × group grid to keep continuity
      full_seq <- if (input$granularity == "month") {
        seq.Date(from = min(d$period_date), to = max(d$period_date), by = "month")
      } else {
        seq.Date(from = min(d$period_date), to = max(d$period_date), by = "year")
      }
      
      all_groups <- sort(unique(d$group))
      full_grid <- expand.grid(period_date = full_seq, group = all_groups, stringsAsFactors = FALSE)
      d_full <- merge(full_grid, d, by = c("period_date", "group"), all.x = TRUE) |> 
                  arrange(period_date, group) |>
                  mutate(
                    period = ifelse(
                      format(period_date, "%m") == "01", 
                      format(period_date, "%Y"), 
                      format(period_date, "%Y-%m")
                    )
                  )
      
      stacked_modes <- c("bar_stack","bar_stack_pct","area_stack","area_stack_pct")
      d_plot <- d_full
      if (style %in% stacked_modes) d_plot$value <- ifelse(is.na(d_plot$value), 0, d_plot$value)
      
      needs_group_aggregation <- style %in% c("line_sum", "line_mean", "bar_mean")
      aggregator <- ifelse(style %in% c("line_sum","area_stack","bar_stack","bar_stack_pct","area_stack_pct"), "sum",
                           ifelse(style %in% c("line_mean","bar_mean"), "mean", NA_character_))
      
      if (style %in% c("pie","donut","treemap")) {
        d_synth <- d_full |>
          group_by(group) |>
          summarise(value = if (stat == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE), .groups = "drop")
        
        if (style == "pie") {
          p <- plotly::plot_ly(d_synth, labels = ~group, values = ~value, type = "pie")
        } else if (style == "donut") {
          p <- plotly::plot_ly(d_synth, labels = ~group, values = ~value, type = "pie", hole = 0.6)
        } else {
          p <- plotly::plot_ly(d_synth, type = "treemap", labels = ~group, parents = NA, values = ~value, textinfo = "label+value+percent parent")
        }
        return(p |> layout(hovermode = "x unified"))
      }
      
      if (style == "rank_sum") {
        req(stat == "sum")
        
        # Global ranking
        d_rank_all <- d_full |>
          group_by(group) |>
          summarise(
            value = sum(value, na.rm = TRUE),
            .groups = "drop"
          ) |>
          filter(!is.na(value)) |>
          arrange(desc(value)) |>
          mutate(
            rank = row_number(),
            pct  = value / sum(value, na.rm = TRUE)
          )
        
        # Selection logic
        if (is.null(rank_target_id)) {
          
          # Default behavior: Top N
          d_rank <- d_rank_all |>
            slice_head(n = rank_number)
          
        } else {
          
          # Targeted rank view
          req(rank_target_id %in% d_rank_all$group)
          
          target_rank <- d_rank_all |>
            filter(group == rank_target_id) |>
            pull(rank)
          
          half_window <- floor(rank_number / 2)
          
          start_rank <- max(1, target_rank - half_window)
          end_rank   <- start_rank + rank_number - 1
          
          if (end_rank > nrow(d_rank_all)) {
            end_rank   <- nrow(d_rank_all)
            start_rank <- max(1, end_rank - rank_number + 1)
          }
          
          d_rank <- d_rank_all |>
            filter(rank >= start_rank & rank <= end_rank)
        }
        
        # Color mapping (highlight target if needed)
        d_rank <- d_rank |>
          mutate(
            bar_color = "#1F77B4"
          )
        
        if (!is.null(rank_target_id)) {
          d_rank <- d_rank |>
            mutate(
              bar_color = if_else(
                group == rank_target_id,
                rank_target_color,
                bar_color
              )
            )
        }
        
        p <- plotly::plot_ly(
          data = d_rank,
          x = ~value,
          y = ~rank,
          type = "bar",
          orientation = "h",
          marker = list(color = d_rank$bar_color),
          text = ~paste0(
            group, "<br>",
            round(value, 2), " (",
            scales::percent(pct, accuracy = 0.1), ")"
          ),
          textposition = "auto",
          hoverinfo = "none"
        ) |>
          layout(
            showlegend = FALSE,
            uniformtext = list(minsize = 9, mode = "show"),
            yaxis = list(
              title = i18n("GENERIC_CHART_PLOT_RANK_TITLE"),
              autorange = "reversed",
              tickmode = "array",
              tickvals = d_rank$rank,
              ticktext = d_rank$rank
            ),
            xaxis = list(
              title = value_label,
              showgrid = FALSE,
              zeroline = FALSE,
              showticklabels = FALSE
            ),
            plot_bgcolor  = "rgba(0,0,0,0)",
            paper_bgcolor = "rgba(0,0,0,0)"
          )
        
        return(p)
      }
      
      if (style == "line") {
        
        p <- plotly::plot_ly(d_full, x = ~period_date, y = ~value, color = ~group, type = "scatter", mode = "lines+markers", connectgaps = FALSE)
      } else if (style == "line_sum") {
        d_agg <- d_full |> group_by(period_date) |> summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
        p <- plotly::plot_ly(d_agg, x = ~period_date, y = ~value, type = "scatter", mode = "lines+markers")
      } else if (style == "line_mean") {
        d_agg <- d_full |> group_by(period_date) |> summarise(value = mean(value, na.rm = TRUE), ymin = min(ymin, na.rm = TRUE), ymax = max(ymax, na.rm = TRUE), .groups = "drop")
        
        p <- plotly::plot_ly(data= d_agg,x = ~period_date, y = ~value, type = "scatter", name="mean",mode = "lines+markers", connectgaps = FALSE) |>
          plotly::add_ribbons(data = d_agg, x = ~period_date, ymin = ~ymin, ymax = ~ymax, fillcolor = "rgba(0,0,150,0.15)", line = list(color = "rgba(0,0,0,0)"), name = "error")
      } else if (style %in% c("bar_stack","bar_stack_pct")) {
        p <- plotly::plot_ly(d_plot, x = ~period_date, y = ~value, color = ~group, type = "bar") |> layout(barmode = "stack")
        if (style == "bar_stack_pct") p <- p |> layout(barnorm = "percent", yaxis = list(ticksuffix = "%"))
      } else if (style == "bar_mean") {
        d_agg <- d_full |> group_by(period_date) |> summarise(value = mean(value, na.rm = TRUE), ymin = min(ymin, na.rm = TRUE), ymax = max(ymax, na.rm = TRUE), .groups = "drop")
        p <- plotly::plot_ly(d_agg, x = ~period_date, y = ~value, type = "bar", error_y = list(type = "data", array = ~ (ymax - value), arrayminus = ~(value - ymin), visible = TRUE))
      } else if (style %in% c("area_stack","area_stack_pct")) {
        groupnorm_val <- if (style == "area_stack_pct") "percent" else NULL
        p <- plotly::plot_ly(d_plot, x = ~period_date, y = ~value, color = ~group, type = "scatter", mode = "none", stackgroup = "one", groupnorm = groupnorm_val, fill = "tonexty")
      } else if (style == "heatmap") {
        
        d_full$period <- factor(d_full$period, levels = sort(unique(d_full$period)))
        p <- plotly::plot_ly(d_full, x = ~period, y = ~group, z = ~value, type = "heatmap", colorscale = list(list(0,"#053061"), list(0.5,"#92C5DE"), list(0.9,"#FFFFBF"), list(1,"#F46D43")), reversescale = FALSE, zauto = TRUE, showscale = TRUE)
       } else if (style == "bubble") {
         d_b <- d_full |> mutate(display_value = ifelse(is.na(value), 0, value))
         maxv <- max(d_b$display_value, na.rm = TRUE)
         sizeref <- ifelse(is.finite(maxv) && maxv > 0, 2 * maxv / (50^2), 1)
      
         d_b <- d_b |> mutate(tooltip = paste0(group, "<br>", format(period_date, "%Y-%m-%d"), "<br>", ifelse(is.na(value), paste0("(",i18n("GENERIC_CHART_PLOT_BUBBLE_NO_DATA"),")"), format(round(value,2), nsmall=2))))
        p <- plotly::plot_ly(d_b, x = ~period_date, y = ~group, size = ~display_value, color = ~group, text = ~tooltip, hoverinfo = "text", type = "scatter", mode = "markers", marker = list(sizemode = "area", sizeref = sizeref, sizemin = 1))
       } else if (style == "boxplot") {
        
        gran <- ifelse(is.null(input$granularity), "year", input$granularity)
        df_raw <- df |>
          rename(date = !!rlang::sym(col_date), group = !!rlang::sym(col_group), raw_value = !!rlang::sym(col_value)) |>
          mutate(date = as.Date(date),
                 period_date = if (gran == "year") as.Date(paste0(format(date, "%Y"), "-01-01")) else as.Date(paste0(format(date, "%Y-%m"), "-01"))
          ) |>
          filter(group %in% all_groups)
        p <- plotly::plot_ly(df_raw, x = ~group, y = ~raw_value, type = "box", color = ~group, boxpoints = "all", jitter = 0.5, pointpos = 0)
      } else {
        p <- plotly::plot_ly() 
      }
      
      if (style == "heatmap") {
        p<-p |> layout(xaxis = list(title = xlab), yaxis = list(title = ylab), hovermode = "x unified")
      } else {
        
        if (input$granularity == "year") {
          p<-p |> layout(
            xaxis = list(
              title = x_lab,
              dtick = "M12",      
              tickformat = "%Y",
              rangeselector = list(
                buttons = list(
                  list(count = 3,  label = "3M", step = "month", stepmode = "backward"),
                  list(count = 6,  label = "6M", step = "month", stepmode = "backward"),
                  list(count = 1,  label = "1Y", step = "year",  stepmode = "backward"),
                  list(count = 2,  label = "2Y", step = "year",  stepmode = "backward"),
                  list(count = 3,  label = "3Y", step = "year",  stepmode = "backward"),
                  list(count = 5,  label = "5Y", step = "year",  stepmode = "backward"),
                  list(step = "year", stepmode = "todate", label = "YTD"),
                  list(step = "all", label = "All")
                )
              ),
              rangeslider = list(visible = FALSE)
            ),
            yaxis = list(title = y_lab),
            hovermode = "x unified"
          )
        } else {
          p<-p |> layout(
            xaxis = list(
              title = x_lab,
              dtick = "M1",     
              tickformat = "%Y-%m",    
              rangeselector = list(
                buttons = list(
                  list(count = 3,  label = "3M", step = "month", stepmode = "backward"),
                  list(count = 6,  label = "6M", step = "month", stepmode = "backward"),
                  list(count = 1,  label = "1Y", step = "year",  stepmode = "backward"),
                  list(count = 2,  label = "2Y", step = "year",  stepmode = "backward"),
                  list(count = 3,  label = "3Y", step = "year",  stepmode = "backward"),
                  list(count = 5,  label = "5Y", step = "year",  stepmode = "backward"),
                  list(step = "year", stepmode = "todate", label = "YTD"),
                  list(step = "all", label = "All")
                )
              ),
              rangeslider = list(visible = FALSE)
            ),
            yaxis = list(title = y_lab),
            hovermode = "x unified"
          )
        }
      }
      p
    })
    
    # -------------------------------------------------------------------------
    # Plot view
    # -------------------------------------------------------------------------

    output$plot <- plotly::renderPlotly({ 
      plot_reactive()
      })
    
    # -------------------------------------------------------------------------
    # Tabular view (pivoted by group)
    # -------------------------------------------------------------------------
    
    output$table <- DT::renderDT({
      dtab <- data_formatted()
      req("value" %in% names(dtab))
      
      dtab |> tidyr::pivot_wider(names_from = group, values_from = value) |>
        DT::datatable(extensions = "Buttons", options = list(dom = "Bfrtip"))
      })
    
    # -------------------------------------------------------------------------
    # Main box content UI (plot + table)
    # -------------------------------------------------------------------------
    
    output$result <- renderUI({
      tabsetPanel(
        tabPanel(
          i18n("GENERIC_CHART_TAB_TITLE_PLOT"), 
          
          if(is.null(data_formatted())){
            HTML(paste0("(",i18n("GENERIC_CHART_NO_DATA"),")"))
          }else{
            plotly::plotlyOutput(ns("plot")) |> 
            shinycssloaders::withSpinner(type = 4)
          }
        ),
        tabPanel(
          i18n("GENERIC_CHART_TAB_TITLE_STATISTICS"), 
          DT::DTOutput(ns("table")) |> 
            shinycssloaders::withSpinner(type = 4)
        )
      )
    })
 
    # -------------------------------------------------------------------------
    # Download handlers
    # -------------------------------------------------------------------------
       
    output$dl_html <- downloadHandler(
      filename = function() paste0("chart_", Sys.Date(), ".html"),
      content = function(file) {
        tmp <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(plot_reactive(), tmp, selfcontained = TRUE)
        file.copy(tmp, file)
      }
    )
    
    output$dl_png <- downloadHandler(
      filename = function() paste0("chart_", Sys.Date(), ".png"),
      content = function(file) {
        plotly::export(plot_reactive(), file = file)
      }
    )
    
    output$download_wrapper<- renderUI({
    tagList(
      downloadLink(ns("dl_png"),  i18n("GENERIC_CHART_DOWNLOAD_PNG")),
      tags$br(),
      downloadLink(ns("dl_html"), i18n("GENERIC_CHART_DOWNLOAD_HTML"))
    )
    })
    
    INFO("Generic chart server - END")
    
  })
  
}

#' @name generic_chart_ui
#' 
#' @title Generic Chart Shiny Module (UI)
#'
#' @description
#' User interface for the generic chart Shiny module.
#'
#' The UI provides:
#' \itemize{
#'   \item A configurable sidebar for plot options
#'   \item A main panel displaying interactive charts and summary tables
#'   \item Download controls for exporting charts (PNG / HTML)
#' }
#'
#' This UI is designed to work with \code{generic_chart_server} and relies on
#' \code{bs4Dash} layout components.
#'
#' @param id Character string. Module namespace identifier.
#'
#' @param title Character string, title displayed at the top of the chart box.
#'
#' @param sliderWidth A numeric value between 25 and 100. Sidebar opening width in percentage. 25\%, means the card sidebar will take 25\% of the card width, when opened. 
#' Default is \code{25}.

#'@export

generic_chart_ui <- function(id, title="",sliderWidth = 25) {
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = title,
      width = 12,
      
      dropdownMenu = bs4Dash::boxDropdown(
        icon = icon("download"),
        status = "primary",
        size = "sm",
        right = TRUE,
        uiOutput(ns("download_wrapper"))
      ),
      
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        width = sliderWidth,
        style = 'font-size:14px;',
        uiOutput(ns("plot_style_wrapper")),
        uiOutput(ns("granularity_wrapper")),
        uiOutput(ns("error_wrapper"))
      ),
      
      uiOutput(ns("result")),
      collapsible = FALSE,
      maximizable = TRUE
    )
  )
}
