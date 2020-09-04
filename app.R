library(shiny)
library(shinydashboard)

library(MASS)

library(ggplot2)
library(RColorBrewer)
library(wesanderson)


# Define UI for application
ui <- dashboardPage(
    dashboardHeader(title="Tiles"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            box(
                plotOutput("plot"),
                downloadButton("download", "Save"),  
            ),
            box(
                title="Parameters",
                sliderInput("n_rows", "Number of rows", 1, 100, 15),
                sliderInput("n_cols", "Number of columns", 1, 100, 15),
                sliderInput("aspect_ratio", "Aspect", -1, 1, 0, step=.1),
                sliderInput("linewidth", "Width of grid lines", 0., 10., 2., step=0.1),
                sliderInput("patch_size", "Patch size (2step)", 0, 1, .5, step=0.01),
                selectInput("color_algorithm", "Algorithm for color choice", c("2step", "random", "precision")),
                selectInput("color_palette", "Color palette", c("RColorBrewer", "all viridis", "viridis", "cividis", "magma", "inferno", "plasma", "Reds", "wesanderson", "R (without greys)")),
                actionButton("resample_colors", "Resample colors", icon("refresh"))
            )
        )
    )
)


# Define server logic
server <- function(input, output) {
    output$plot <- renderPlot({
        
      validate(
        need((input$color_algorithm != "precision") | (input$n_rows * input$n_cols < 100), "n_rows * n_cols should be < 100 when color_algorithm is precision")
      )
      
        # trigger color resampling if "Resample colors" button is clicked
        input$resample_colors
        
        # retrieve parameters
        n_rows <- input$n_rows
        n_cols <- input$n_cols
        aspect_ratio <- 10 ^ input$aspect_ratio
        linewidth <- input$linewidth
        patch_size <- input$patch_size
        color_algorithm <- input$color_algorithm
        color_palette <- input$color_palette
        
        # setup data.frame of coords
        coords <- data.frame(rows = integer(), cols = integer(), z = integer())
        for (row in 1:n_rows) {
            for (col in 1:n_cols) {
                new_coords = data.frame(rows = row, cols = col, z = (row - 1) * n_cols + (col - 1))
                coords <- rbind(coords, new_coords)
            }
        }
        
        # setup colors
        candidate_colors <- choose_candidate_colors(color_palette)
        picked_colors <- pick_colors(color_algorithm, candidate_colors, coords, patch_size)
        
        # plot
        ggplot(coords, aes(x=cols, y=rows)) + 
            theme_void() +
            theme(
                legend.position = "none", 
                plot.margin=grid::unit(c(0,0,0,0), "points")
            ) + 
            geom_tile(aes(fill = z)) +
            scale_fill_gradientn(colours = picked_colors) +
            geom_hline(yintercept = (0:n_rows) + 0.5, colour = "white", size = linewidth) + 
            geom_vline(xintercept = (0:n_cols) + 0.5, colour = "white", size = linewidth) + 
            coord_fixed(
                ratio = aspect_ratio,
                #xlim = c(1, n_cols), 
                #ylim = c(1, n_rows)
            )
        
    })
    
    output$download <- downloadHandler(
      filename = function() {
        "tiles.pdf"
      },
      content = function(file) {
        ggsave(file)
      }
    )
}


choose_candidate_colors <- function(color_palette) {
    if (color_palette == "R (without greys)")
        candidate_colors <- Filter(function(x) !startsWith(x, "grey"), colors())
    else if (color_palette == "RColorBrewer") {
        pals <- brewer.pal.info
        candidate_colors <- unlist(
            by(pals, 1:nrow(pals), 
               function(row) brewer.pal(as.integer(row["maxcolors"]), rownames(row)), simplify=TRUE)
        )
    } else if (color_palette == "Reds") {
      candidate_colors <- brewer.pal(9, "Reds")
    } else if (color_palette == "viridis") {
        candidate_colors = viridisLite::viridis(20)
    } else if (color_palette == "cividis") {
        candidate_colors = viridisLite::cividis(20)
    } else if (color_palette == "magma") {
        candidate_colors = viridisLite::magma(20)
    } else if (color_palette == "inferno") {
        candidate_colors = viridisLite::inferno(20)
    } else if (color_palette == "plasma") {
        candidate_colors = viridisLite::plasma(20)
    } else if (color_palette == "all viridis") {
        candidate_colors = c(
            viridisLite::viridis(10),
            # viridisLite::cividis(10),
            viridisLite::magma(10),
            viridisLite::inferno(10),
            viridisLite::plasma(10)
        )
    } else if (color_palette == "wesanderson") {
        candidate_colors <- unlist(wes_palettes)
    }
    
  return(candidate_colors)
}

set.seed(42)

pick_colors <- function(color_algorithm, candidate_colors, coords, patch_size) {
  
    if (color_algorithm == "random") {
        colours <- sample(candidate_colors, nrow(coords), replace = TRUE)
    } else if (color_algorithm == "precision") {
        
        dists <- as.matrix(
            dist(coords[, c("rows", "cols")])
        )
        
        precision <- .25 * (dists == 1) + 1. * (dists == 0)
        covariance <- solve(precision)
        
        mu = rep(0., nrow(coords)) + .5
        tile_colors <- as.data.frame(t(
            mvrnorm(3, mu, covariance)
        ))
        colnames(tile_colors) = c("red", "green", "blue")
        tile_colors[tile_colors < 0] <- 0
        tile_colors[tile_colors > 1] <- 1
        tile_colors <- apply(tile_colors, 1, rgb2)
        
        colours <- tile_colors
        
    } else if (color_algorithm == "2step") {

        coords["colours"] <- sample(candidate_colors, nrow(coords), replace = TRUE)
        for (tile_id in 1:nrow(coords)) {
                
            row <- coords[tile_id, "rows"]
            col <- coords[tile_id, "cols"]
            
            n_rows <- max(coords["rows"])
            n_cols <- max(coords["cols"])
            
            chosen_neighbor <- sample(c("top", "right", "bottom", "left"), 1)
            if (chosen_neighbor == "top") {
                rown <- row - 1
                coln <- col
                if (rown == 0) 
                    rown <- n_rows
            } else if (chosen_neighbor == "bottom") {
                rown <- row + 1
                coln <- col
                if (rown == n_rows + 1) 
                    rown <- 1
            } else if (chosen_neighbor == "left") {
                rown <- row
                coln <- col - 1
                if (coln == 0) 
                    coln <- n_cols
            } else if (chosen_neighbor == "right") {
                rown <- row
                coln <- col + 1
                if (coln == n_cols + 1) 
                    coln <- 1
            }
            
            colourn <- coords[(coords$rows == rown) & (coords$cols == coln),]$colours
            p = patch_size
            if (rbinom(1, 1, p) == 1) {
                coords[(coords$rows == row) & (coords$cols == col),]$colours <- colourn
            }
            #print(coords[(coords$rows == row) & (coords$cols == col),]$colours)
            #print("---")
            
            colours <- coords$colours
            
        }
        
    }
    return(colours)
}


rgb2 <- function(x) {rgb(x[[1]], x[[2]], x[[3]])}


# Run the application 
shinyApp(ui = ui, server = server)
