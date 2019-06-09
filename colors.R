library(RColorBrewer)


#### categorical colors
# found here: https://stackoverflow.com/a/44727682/496209
plotlyColors <- c('#1f77b4',
                  '#ff7f0e',
                  '#2ca02c',
                  '#d62728',
                  '#9467bd',
                  '#8c564b',
                  '#e377c2',
                  '#7f7f7f',
                  '#bcbd22',
                  '#17becf')

categoricalDark2Colors8 <- brewer.pal(8, "Dark2")
categoricalSet2Colors9 <- brewer.pal(9, "Set1")
categoricalDark2ExColors9 <- c(categoricalDark2Colors8, "#80b1d3")

# generate here: http://vrl.cs.brown.edu/color
#firstColor <- "#256676"
#secondColor <- "#7FDC64"


# sequential colors
sequentialGreens9 <- brewer.pal(9, "Greens")
sequentialReds9 <- brewer.pal(9, "Reds")
sequentialReds8 <- brewer.pal(8, "Reds")


# diverging colors
divergingGreenRedSemanticColorScale11 <- c(brewer.pal(11, "RdBu")[1:5], brewer.pal(11, "PRGn")[6:11])
divergingGreenPurpleColorScale11 <- brewer.pal(11, "PRGn")

divergingContinuousGreenRedSemanticColorScale <- c("#CD0000", "#F2BFBF", "#F5F5F5", "#D3DCC4", "#517212")


# colorscale generation functions
divergingContinuousColorscale <- function (colormap) {
  list(
    list(0, colormap[1]),
    list(0.495, colormap[2]),
    list(0.5, colormap[3]),
    list(0.505, colormap[4]),
    list(1, colormap[5])
  )
}

divergingSegmentedColorscale <- function (colorMap) {
  list(
    list(0, colorMap[1]), 
    list(0.1, colorMap[1]),
    list(0.10000001, colorMap[2]), 
    list(0.25, colorMap[2]), 
    list(0.25000001, colorMap[3]), 
    list(0.35, colorMap[3]), 
    list(0.35000001, colorMap[4]), 
    list(0.45, colorMap[4]), 
    list(0.45000001, colorMap[5]), 
    list(0.499, colorMap[5]), 
    list(0.5, colorMap[6]), 
    list(0.501, colorMap[7]), 
    list(0.54999999, colorMap[7]), 
    list(0.55, colorMap[8]),
    list(0.64999999, colorMap[8]),
    list(0.65, colorMap[9]),
    list(0.74999999, colorMap[9]),
    list(0.75, colorMap[10]),
    list(0.89999999, colorMap[10]),
    list(0.9, colorMap[11]),
    list(1, colorMap[11])
  )
}


sequentialColorscale <- function (colormap) {
  list(
    list(0, colormap[1]),
    list(0.03, colormap[1]),
    list(0.03000001, colormap[2]),
    list(0.08, colormap[2]),
    list(0.08000001, colormap[3]),
    list(0.15, colormap[3]),
    list(0.15000001, colormap[4]),
    list(0.25, colormap[4]),
    list(0.25000001, colormap[5]),
    list(0.36, colormap[5]),
    list(0.36000001, colormap[6]),
    list(0.47, colormap[6]),
    list(0.47000001, colormap[7]),
    list(0.60, colormap[7]),
    list(0.60000001, colormap[8]),
    list(0.77, colormap[8]),
    list(0.77000001, colormap[9]),
    list(1, colormap[9])
  )
}