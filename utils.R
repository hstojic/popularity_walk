# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# this script contains ggplot theme and color specifications 
# used in plotting figures, as well as some useful functions for plotting

# here we use extrafont package to be able to use non-standard R fonts
# if its the first time this package is used, you will first need to 
# run: library(extrafont); font_import()
# this will try to import fonts available on your OS, hopefully Arial as well
# and then this code will work without issues


# ----------------------------------------------------------------------
# Variables and functions
# ----------------------------------------------------------------------

# parameters
loadfonts()
fontSetup <- "Arial"
fontSize <- 2.25 
pointSize <- 1
lineSize <- 0.5
themeFontSize <- 6
mt <- 1
inch <- 0.3937008
cm <- 2.54

# single column: 8.8cm
height <- 6*inch
width <- 8.8*inch
fd_1c_1x0.33 <- c(height, 0.33*width)
fd_1c_1x0.5 <- c(height, 0.5*width)
fd_1c_1x0.66 <- c(height, 0.66*width)
fd_1c_1x1 <- c(height, width)
fd_1c_1.5x1 <- c(1.5*height, width)


# 1.5 column: 11.4cm
base <- 1.48
fd_1.5c_1x0.3 <- c(base, base)
fd_1.5c_2x0.3 <- c(2*base, base)
fd_1.5c_1x1 <- c(base, 3*base)

# two column: 18cm
fd_2c_1x0.25 <- c(1.5, 1.7)



# ----------------------------------------------------------------------
# Ggplot themes
# ----------------------------------------------------------------------

# theme with font sizes adjusted for plotting figures with tikz device
mytheme <- 
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(lineend = 4, linetype = 1),
        axis.ticks.y = element_line(lineend = 4, linetype = 1),
        axis.ticks = element_line (colour = "black", size = 0.3), 
        axis.text = element_text(size = themeFontSize, colour = "black"),
        axis.text.x = element_text(vjust = 0.5),
        axis.title = element_text(size = themeFontSize + 2),
        axis.title.y = element_text(vjust = 1.8),
        axis.title.x = element_text(vjust = -.8),
        legend.title = element_blank(),
        legend.justification = c(1,0),
        legend.position = c(1,0),
        legend.text = element_text(size = themeFontSize),
        legend.key = element_rect(fill = "#FFFFFF"),
        legend.key.height = unit(0.8,"line"),
        strip.text = element_text(size = themeFontSize + 2),
        strip.background = element_rect(fill = "#FFFFFF"),
        text = element_text(family = fontSetup),
        validate = TRUE
    )


# ----------------------------------------------------------------------
# Color palettes
# ----------------------------------------------------------------------

# Color-blind friendly color combinations

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# ----------------------------------------------------------------------
# Other utilities
# ----------------------------------------------------------------------

# function for saving the figures
saveFig <- function(fig, dir, name, dims, cairo = FALSE) {
    if (is.na(dir)) {
        dirName <- ""
    } else {
        dirName <- dir
    }
    filepath <- file.path(dir, paste0(name, ".pdf"))
    if (!cairo) {
        # default pdf device
        pdf(filepath, 
            height = dims[1], width = dims[2], 
            pointsize = themeFontSize,
            colormodel = "cmyk",
            # colormodel = "gray",
            # family
            # fonts
            onefile = FALSE, 
            useDingbats = FALSE
        );
        print(fig)
        dev.off()
        embed_fonts(filepath)
    } else {
        # or cairo, easier for embedding sometimes
        cairo_pdf(filepath,
            height = dims[1], width = dims[2], 
            pointsize = themeFontSize,
            onefile = FALSE, 
            family = fontSetup, 
            fallback_resolution = 1500
        )
        myprint(plot[[names(plot)[1]]])
        dev.off()    
    }
}
