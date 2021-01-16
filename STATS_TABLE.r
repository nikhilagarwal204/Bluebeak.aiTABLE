install.packages("reactable")

data <- read.csv("https://raw.githubusercontent.com/nikhilagarwal204/Bluebeak.aiTABLE/main/twitter_followers.csv",
                 stringsAsFactors = FALSE)

library(reactable)
reactable(data)                 

reactable(
  data,
  defaultSorted = "exclusive_followers_pct",
  columns = list(
    account = colDef(
      name = "Account",
      format = colFormat(prefix = "@")
    ),
    followers = colDef(
      name = "Followers",
      defaultSortOrder = "desc",
      format = colFormat(separators = TRUE)
    ),
    exclusive_followers_pct = colDef(
      name = "Exclusive Followers",
      defaultSortOrder = "desc",
      format = colFormat(percent = TRUE, digits = 1)
    )
  )
)

library(htmltools)
# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

reactable(
  data,
  defaultSorted = "exclusive_followers_pct",
  columns = list(
    account = colDef(
      name = "Account",
      format = colFormat(prefix = "@")
    ),
    followers = colDef(
      name = "Followers",
      defaultSortOrder = "desc",
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        width <- paste0(value * 100 / max(data$followers), "%")
        # Add thousands separators
        value <- format(value, big.mark = ",")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      # And left-align the columns
      align = "left"
    ),
    exclusive_followers_pct = colDef(
      name = "Exclusive Followers",
      defaultSortOrder = "desc",
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        # Format as percentages with 1 decimal place
        value <- paste0(format(value * 100, nsmall = 1), "%")
        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
      },
      # And left-align the columns
      align = "left"
    )
  )
)

reactable(
  data,
  defaultSorted = "exclusive_followers_pct",
  columns = list(
    account = colDef(
      name = "Account",
      format = colFormat(prefix = "@")
    ),
    followers = colDef(
      name = "Followers",
      defaultSortOrder = "desc",
      cell = function(value) {
        width <- paste0(value * 100 / max(data$followers), "%")
        value <- format(value, big.mark = ",")
        # Fix each label using the width of the widest number (incl. thousands separators)
        value <- format(value, width = 9, justify = "right")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      align = "left",
      # Use the operating system's default monospace font, and
      # preserve white space to prevent it from being collapsed by default
      style = list(fontFamily = "monospace", whiteSpace = "pre")
    ),
    exclusive_followers_pct = colDef(
      name = "Exclusive Followers",
      defaultSortOrder = "desc",
      cell = function(value) {
        value <- paste0(format(value * 100, nsmall = 1), "%")
        # Fix width here to align single and double-digit percentages
        value <- format(value, width = 5, justify = "right")
        bar_chart(value, width = value, fill = "#fc5185", background = "#e1e1e1")
      },
      align = "left",
      style = list(fontFamily = "monospace", whiteSpace = "pre")
    )
  )
)

reactable(
  data,
  pagination = FALSE,
  defaultSorted = "exclusive_followers_pct",
  columns = list(
    account = colDef(
      name = "Account",
      format = colFormat(prefix = "@")
    ),
    followers = colDef(
      name = "Followers",
      defaultSortOrder = "desc",
      cell = function(value) {
        width <- paste0(value * 100 / max(data$followers), "%")
        value <- format(value, big.mark = ",")
        value <- format(value, width = 9, justify = "right")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      align = "left",
      style = list(fontFamily = "monospace", whiteSpace = "pre")
    ),
    exclusive_followers_pct = colDef(
      name = "Exclusive Followers",
      defaultSortOrder = "desc",
      # Format and render the cell with a JavaScript render function
      cell = JS("function(cellInfo) {
        // Format as a percentage with 1 decimal place
        const pct = (cellInfo.value * 100).toFixed(1) + '%'
        // Fix width of numeric labels
        let value = pct.padStart(5)
        // Show percent sign on first row only
        if (cellInfo.viewIndex > 0) {
          value = value.replace('%', ' ')
        }
        // Render bar chart
        return (
          '<div style=\"display: flex; align-items: center;\">' +
            '<span style=\"font-family: monospace; white-space: pre;\">' + value + '</span>' +
            '<div style=\"flex-grow: 1; margin-left: 6px; height: 14px; background-color: #e1e1e1\">' +
              '<div style=\"height: 100%; width: ' + pct + '; background-color: #fc5185\"></div>' +
            '</div>' +
          '</div>'
        )
      }"),
      # Render this column as HTML
      html = TRUE,
      align = "left"
    )
  ),
  compact = TRUE,
)