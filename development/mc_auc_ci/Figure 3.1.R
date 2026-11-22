
# Figure 3.1 - ProcEx Package Architecture
# No additional packages required

library(grid)

# Save files to the current working directory
output_dir <- getwd()

pdf_file <- file.path(
  output_dir,
  "ProcEx_Figure_3_1_Architecture.pdf"
)

png_file <- file.path(
  output_dir,
  "ProcEx_Figure_3_1_Architecture.png"
)

# Colours
ink <- "#1E293B"
blue <- "#174A79"
soft_blue <- "#E8F2FA"
soft_teal <- "#E8F5F3"
soft_grey <- "#F2F5F8"
line_col <- "#9AA8B5"

# Draw a rounded box
node <- function(x, y, w, h, label,
                 fill = soft_blue,
                 border = "#C9DDEB",
                 font_size = 10.5,
                 text_color = blue,
                 bold = TRUE) {

  grid.roundrect(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(w, "npc"),
    height = unit(h, "npc"),
    r = unit(0.09, "inches"),
    gp = gpar(
      fill = fill,
      col = border,
      lwd = 1
    )
  )

  grid.text(
    label,
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    gp = gpar(
      col = text_color,
      fontfamily = "sans",
      fontsize = font_size,
      fontface = if (bold) "bold" else "plain"
    )
  )
}

# Draw arrows and connecting lines
connector <- function(x, y,
                      directed = TRUE,
                      dashed = FALSE) {

  grid.lines(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    arrow = if (directed) {
      arrow(
        type = "closed",
        length = unit(0.075, "inches")
      )
    } else {
      NULL
    },
    gp = gpar(
      col = line_col,
      lwd = 1.25,
      lty = if (dashed) "dashed" else "solid"
    )
  )
}

# Main diagram
make_figure <- function() {

  grid.newpage()

  grid.rect(
    gp = gpar(fill = "white", col = NA)
  )

  # Title
  grid.text(
    "ProcEx Package Architecture and Functional Workflow",
    x = 0.5,
    y = 0.971,
    gp = gpar(
      fontsize = 19,
      fontface = "bold",
      col = ink
    )
  )

  grid.text(
    "Modular R package for classification evaluation and AUC uncertainty",
    x = 0.5,
    y = 0.933,
    gp = gpar(
      fontsize = 11,
      col = "#5D6B79"
    )
  )

  # Input
  node(
    0.50, 0.846, 0.52, 0.098,
    "Classification Model Outputs\nTrue class labels + predicted probabilities",
    fill = "#DCECF9",
    font_size = 13
  )

  # Shared input connections
  connector(
    c(0.50, 0.50),
    c(0.797, 0.753),
    directed = FALSE
  )

  connector(
    c(0.12, 0.88),
    c(0.753, 0.753),
    directed = FALSE
  )

  xs <- c(0.12, 0.31, 0.50, 0.69, 0.88)

  for (x in xs) {
    connector(
      c(x, x),
      c(0.753, 0.695)
    )
  }

  # Six main ProcEx functions
  node(
    0.12, 0.640, 0.172, 0.100,
    "validate_classification_\ndata()",
    font_size = 10
  )

  node(
    0.31, 0.640, 0.172, 0.100,
    "mc_roc()",
    font_size = 11.5
  )

  node(
    0.50, 0.640, 0.172, 0.100,
    "profile_outliers()",
    font_size = 11
  )

  node(
    0.69, 0.640, 0.172, 0.100,
    "mc_auc_ci()",
    font_size = 11.5
  )

  node(
    0.88, 0.640, 0.172, 0.100,
    "mc_auc_compare()",
    font_size = 10.7
  )

  # Function outputs
  for (x in xs) {
    connector(
      c(x, x),
      c(0.590, 0.536)
    )
  }

  node(
    0.12, 0.478, 0.172, 0.116,
    "Validation status\nErrors and reliability\nwarnings",
    fill = soft_grey,
    font_size = 10,
    text_color = ink,
    bold = FALSE
  )

  node(
    0.31, 0.478, 0.172, 0.116,
    "Multiclass ROC object\nOverall and pairwise\nAUC results",
    fill = soft_grey,
    font_size = 10,
    text_color = ink,
    bold = FALSE
  )

  node(
    0.50, 0.478, 0.172, 0.116,
    "Outlier flags\nData-quality findings",
    fill = soft_grey,
    font_size = 10,
    text_color = ink,
    bold = FALSE
  )

  node(
    0.69, 0.478, 0.172, 0.116,
    "Original AUC\nBootstrap SE and\npercentile CI",
    fill = soft_grey,
    font_size = 10,
    text_color = ink,
    bold = FALSE
  )

  node(
    0.88, 0.478, 0.172, 0.116,
    "AUC difference\nConfidence interval\nand p-value",
    fill = soft_grey,
    font_size = 10,
    text_color = ink,
    bold = FALSE
  )

  # Missing-value handling
  # Requires an mc_roc object
  connector(
    c(0.31, 0.31),
    c(0.420, 0.365)
  )

  node(
    0.31, 0.323, 0.172, 0.084,
    "handle_missing()",
    fill = soft_teal,
    border = "#BBDDD8",
    font_size = 11
  )

  connector(
    c(0.31, 0.31),
    c(0.281, 0.246)
  )

  node(
    0.31, 0.204, 0.172, 0.084,
    "Updated ROC object\n+ missing-data summary",
    fill = soft_grey,
    font_size = 9.7,
    text_color = ink,
    bold = FALSE
  )

  # Optional plotting
  node(
    0.67, 0.142, 0.55, 0.092,
    paste0(
      "Optional plotting and interpretation\n",
      "Pairwise ROC / AUC heatmap   |   ",
      "Bootstrap AUC   |   Model comparison"
    ),
    fill = "#DCECF9",
    font_size = 10.2
  )

  # Supported plotting connections
  connector(
    c(0.397, 0.407, 0.407, 0.425),
    c(0.440, 0.440, 0.226, 0.188),
    dashed = TRUE
  )

  connector(
    c(0.69, 0.69),
    c(0.420, 0.188),
    dashed = TRUE
  )

  connector(
    c(0.88, 0.88),
    c(0.420, 0.188),
    dashed = TRUE
  )

  # Technical note
  grid.text(
    paste0(
      "Note: mc_roc() validates internally. ",
      "mc_auc_ci() accepts labels and probabilities directly.\n",
      "mc_auc_compare() compares two models on the same observations; ",
      "handle_missing() requires an mc_roc object."
    ),
    x = 0.5,
    y = 0.049,
    gp = gpar(
      fontsize = 8.8,
      col = "#5D6B79"
    )
  )
}

# Export vector PDF
pdf(
  pdf_file,
  width = 13,
  height = 8.4,
  family = "Helvetica",
  useDingbats = FALSE,
  bg = "white"
)

make_figure()
dev.off()

# Export high-resolution PNG
png(
  png_file,
  width = 3900,
  height = 2520,
  res = 300,
  bg = "white"
)

make_figure()
dev.off()

# Display saved file locations
cat(
  "Done! Files saved in:\n",
  output_dir,
  "\n\n",
  sep = ""
)

cat("PDF: ", pdf_file, "\n", sep = "")
cat("PNG: ", png_file, "\n", sep = "")

