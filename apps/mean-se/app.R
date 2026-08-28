library(shiny)


# Turn commas, spaces, or line breaks into a numeric vector.
parse_values <- function(text) {
  pieces <- unlist(strsplit(trimws(text), "[,;[:space:]]+"))
  pieces <- pieces[nzchar(pieces)]
  suppressWarnings(as.numeric(pieces))
}


# Keep displayed arithmetic readable while retaining full precision internally.
show_number <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits, trim = TRUE)
}


ui <- fluidPage(
  tags$head(
    tags$title("From Data to Standard Error"),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(HTML(
      "Shiny.addCustomMessageHandler('scrollLesson', function(message) {
         document.getElementById('lesson-panel').scrollIntoView({
           behavior: 'smooth', block: 'start'
         });
       });"
    ))
  ),

  div(
    class = "app-shell",

    div(
      class = "hero",
      div(class = "eyebrow", "BIOL 242L · Guided statistics explorer"),
      h1("From raw data to mean ± 2 SE"),
      p(
        "Work through what each summary means, where each number comes from,",
        "and how sample standard deviation is connected to standard error."
      )
    ),

    uiOutput("progress"),

    fluidRow(
      column(
        width = 4,
        div(
          class = "panel input-panel",
          h2("Your data"),
          p(class = "helper", "Enter at least two observations per group."),

          textInput("group1_name", "First group name", value = "Control"),
          textAreaInput(
            "group1_values",
            "First group values",
            value = "8, 10, 11, 9, 12",
            rows = 5,
            resize = "vertical"
          ),

          textInput("group2_name", "Second group name", value = "Treatment"),
          textAreaInput(
            "group2_values",
            "Second group values",
            value = "13, 15, 12, 16, 14",
            rows = 5,
            resize = "vertical"
          ),

          textInput("y_label", "Measurement name", value = "Response"),
          p(
            class = "input-note",
            "Separate values with commas, spaces, or new lines. Decimals and negative values are fine."
          )
        )
      ),

      column(
        width = 8,
        div(
          id = "lesson-panel",
          class = "panel lesson-panel",
          uiOutput("lesson"),
          uiOutput("navigation")
        )
      )
    ),

    div(
      class = "footer-note",
      "Your data stay in this browser tab; nothing is uploaded or saved."
    )
  )
)


server <- function(input, output, session) {

  current_step <- reactiveVal(1)
  step_names <- c("Inspect data", "Mean", "Standard deviation", "Standard error", "Graph")


  # Gather and check the two groups in one place so every lesson uses the same data.
  group_data <- reactive({
    names <- trimws(c(input$group1_name, input$group2_name))
    values <- list(
      parse_values(input$group1_values),
      parse_values(input$group2_values)
    )

    validate(
      need(all(nzchar(names)), "Give both groups a name."),
      need(names[1] != names[2], "Give the two groups different names."),
      need(all(lengths(values) >= 2), "Enter at least two values for each group."),
      need(all(vapply(values, function(x) all(is.finite(x)), logical(1))),
           "Use only numbers, separated by commas, spaces, or new lines.")
    )

    list(names = names, values = values)
  })


  # These are the quantities students uncover one step at a time.
  group_stats <- reactive({
    d <- group_data()

    stats <- data.frame(
      Group = d$names,
      n = lengths(d$values),
      Mean = vapply(d$values, mean, numeric(1)),
      SD = vapply(d$values, sd, numeric(1)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    stats$SE <- stats$SD / sqrt(stats$n)
    stats[["2 × SE"]] <- 2 * stats$SE
    stats
  })


  go_to_step <- function(step) {
    current_step(step)
    session$sendCustomMessage("scrollLesson", list())
  }

  observeEvent(input$next_step, go_to_step(min(5, current_step() + 1)))
  observeEvent(input$previous_step, go_to_step(max(1, current_step() - 1)))
  observeEvent(input$start_over, go_to_step(1))


  output$progress <- renderUI({
    step <- current_step()

    div(
      class = "progress-steps",
      lapply(seq_along(step_names), function(i) {
        state <- if (i < step) "complete" else if (i == step) "current" else "upcoming"
        div(
          class = paste("progress-step", state),
          span(class = "step-number", i),
          span(class = "step-name", step_names[i])
        )
      })
    )
  })


  output$raw_data_table <- renderTable({
    d <- group_data()
    rows <- seq_len(max(lengths(d$values)))
    pad <- function(x) c(x, rep(NA_real_, length(rows) - length(x)))

    table <- data.frame(Observation = rows, pad(d$values[[1]]), pad(d$values[[2]]),
                        check.names = FALSE)
    names(table)[2:3] <- d$names
    table
  }, striped = TRUE, bordered = FALSE, spacing = "s", na = "")


  output$mean_work <- renderUI({
    d <- group_data()

    tagList(lapply(seq_along(d$values), function(i) {
      x <- d$values[[i]]
      div(
        class = "calculation-card",
        h3(d$names[i]),
        p(class = "formula", "mean = sum of observations / n"),
        p(
          class = "arithmetic",
          paste0(
            "(", paste(show_number(x), collapse = " + "), ") / ",
            length(x), " = ", show_number(mean(x))
          )
        )
      )
    }))
  })


  deviation_table <- function(i) {
    d <- group_data()
    x <- d$values[[i]]
    deviations <- x - mean(x)

    table <- data.frame(
      Value = x,
      `Value − mean` = deviations,
      `(Value − mean)²` = deviations^2,
      check.names = FALSE
    )
    table[] <- lapply(table, round, digits = 3)
    table
  }

  output$sd_table_1 <- renderTable(
    deviation_table(1), striped = TRUE, bordered = FALSE, spacing = "s"
  )
  output$sd_table_2 <- renderTable(
    deviation_table(2), striped = TRUE, bordered = FALSE, spacing = "s"
  )


  output$sd_work <- renderUI({
    d <- group_data()
    s <- group_stats()

    tagList(lapply(seq_along(d$values), function(i) {
      squared_sum <- sum((d$values[[i]] - s$Mean[i])^2)
      div(
        class = "calculation-card",
        h3(d$names[i]),
        p(class = "formula", "SD = √[sum of squared deviations / (n − 1)]"),
        p(
          class = "arithmetic",
          paste0(
            "√(", show_number(squared_sum), " / ", s$n[i] - 1, ") = ",
            show_number(s$SD[i])
          )
        )
      )
    }))
  })


  output$se_work <- renderUI({
    s <- group_stats()

    tagList(lapply(seq_len(nrow(s)), function(i) {
      div(
        class = "calculation-card",
        h3(s$Group[i]),
        p(class = "formula", "SE = SD / √n"),
        p(
          class = "arithmetic",
          paste0(
            show_number(s$SD[i]), " / √", s$n[i], " = ", show_number(s$SE[i])
          )
        ),
        p(
          "For the graph: 2 × SE = ",
          strong(show_number(2 * s$SE[i]))
        )
      )
    }))
  })


  output$final_summary <- renderTable({
    s <- group_stats()
    data.frame(
      Group = s$Group,
      n = s$n,
      Mean = round(s$Mean, 3),
      SD = round(s$SD, 3),
      SE = round(s$SE, 3),
      `Mean − 2 SE` = round(s$Mean - 2 * s$SE, 3),
      `Mean + 2 SE` = round(s$Mean + 2 * s$SE, 3),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")


  output$final_plot <- renderPlot({
    d <- group_data()
    s <- group_stats()
    lower <- s$Mean - 2 * s$SE
    upper <- s$Mean + 2 * s$SE

    y_range <- range(c(0, unlist(d$values), lower, upper))
    padding <- diff(y_range) * 0.12
    if (padding == 0) padding <- 1
    y_limits <- c(
      y_range[1] - if (y_range[1] < 0) padding else 0,
      y_range[2] + padding
    )

    old_par <- par(mar = c(5, 5, 2, 1), las = 1)
    on.exit(par(old_par))

    centers <- barplot(
      height = s$Mean,
      names.arg = s$Group,
      col = c("#2a7f77", "#d97941"),
      border = NA,
      width = 0.68,
      space = 0.5,
      ylim = y_limits,
      ylab = input$y_label,
      cex.names = 1.05,
      cex.lab = 1.05,
      axes = FALSE
    )
    axis(2)
    abline(h = 0, col = "#475569", lwd = 1)

    arrows(
      x0 = centers, y0 = lower,
      x1 = centers, y1 = upper,
      angle = 90, code = 3, length = 0.07,
      lwd = 2, col = "#172033"
    )

    if (isTRUE(input$show_points)) {
      for (i in seq_along(d$values)) {
        x <- d$values[[i]]
        offsets <- if (length(x) == 1) 0 else seq(-0.11, 0.11, length.out = length(x))
        points(
          centers[i] + offsets, x,
          pch = 21, bg = "white", col = "#172033", cex = 1.15, lwd = 1.2
        )
      }
    }
  }, res = 110, height = 470)


  output$lesson <- renderUI({
    # Calling this here makes invalid input produce one clear message in the lesson panel.
    group_data()

    switch(
      as.character(current_step()),

      "1" = tagList(
        div(class = "step-kicker", "Step 1 of 5"),
        h2("Inspect the observations"),
        p(
          "These raw values are the evidence. Before calculating anything, check that",
          "each row represents one independent observation and that the values are in the same units."
        ),
        div(
          class = "thinking-prompt",
          strong("Pause and predict"),
          p("Which group do you expect to have the larger mean? Which looks more spread out?")
        ),
        tableOutput("raw_data_table")
      ),

      "2" = tagList(
        div(class = "step-kicker", "Step 2 of 5"),
        h2("Find the mean"),
        p(
          "The mean is the balance point of a group: add all observations, then divide by",
          "the number of observations."
        ),
        div(class = "calculation-grid", uiOutput("mean_work")),
        div(
          class = "concept-note",
          strong("What the mean tells us"),
          p("It describes the center of these observations, but not how widely they vary.")
        )
      ),

      "3" = tagList(
        div(class = "step-kicker", "Step 3 of 5"),
        h2("Measure spread with sample standard deviation"),
        p(
          "First find every observation's distance from its group mean. Squaring makes all",
          "distances positive and gives more weight to observations farther from the mean."
        ),
        fluidRow(
          column(6, h3(group_data()$names[1]), tableOutput("sd_table_1")),
          column(6, h3(group_data()$names[2]), tableOutput("sd_table_2"))
        ),
        p(
          "Add the squared deviations, divide by n − 1 for a sample, and take the square root",
          "to return to the original units."
        ),
        div(class = "calculation-grid", uiOutput("sd_work")),
        div(
          class = "concept-note",
          strong("What SD tells us"),
          p("SD describes variation among the individual observations in a group.")
        )
      ),

      "4" = tagList(
        div(class = "step-kicker", "Step 4 of 5"),
        h2("Connect SD to standard error"),
        p(
          "A sample mean is an estimate. If we repeatedly collected new samples, their means",
          "would differ. Standard error describes the expected spread of those sample means."
        ),
        div(class = "equation", "SE = SD / √n"),
        div(class = "calculation-grid", uiOutput("se_work")),
        div(
          class = "comparison-box",
          div(
            h3("SD"),
            p("Spread among individual observations"),
            p(class = "units", "Same units as the data")
          ),
          div(class = "comparison-arrow", "→"),
          div(
            h3("SE"),
            p("Precision of the estimated mean"),
            p(class = "units", "Gets smaller as n increases")
          )
        ),
        tags$details(
          class = "thinking-prompt",
          tags$summary("Think first: what happens to SE if n becomes four times larger?"),
          p("√n doubles, so SE becomes half as large—if SD stays the same.")
        )
      ),

      "5" = tagList(
        div(class = "step-kicker", "Step 5 of 5"),
        h2("Graph each mean ± 2 SE"),
        p(
          "The bar height is the group mean. Each error bar extends two standard errors below",
          "and above that mean. Keep the raw points visible so the summary stays connected to the data."
        ),
        checkboxInput("show_points", "Show the raw observations", value = TRUE),
        plotOutput("final_plot"),
        tableOutput("final_summary"),
        div(
          class = "concept-note",
          strong("Interpret with care"),
          p(
            "Mean ± 2 SE is often close to a 95% confidence interval under familiar assumptions,",
            "but it is not automatically an exact 95% interval. Overlapping error bars are also not",
            "a hypothesis test."
          )
        )
      )
    )
  })


  output$navigation <- renderUI({
    step <- current_step()

    div(
      class = "lesson-navigation",
      if (step > 1) actionButton("previous_step", "Back", class = "button-secondary"),
      if (step < 5) {
        actionButton(
          "next_step",
          paste("Next:", step_names[step + 1]),
          class = "button-primary"
        )
      } else {
        actionButton("start_over", "Start over", class = "button-primary")
      }
    )
  })
}


shinyApp(ui, server)
