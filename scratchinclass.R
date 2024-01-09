# test
rm(list = ls())

new_seeds <- c(5:13)[sample.int(n = 9, size = 1)]
length(c(5:13))

new_seeds <- c(5:13)[sample.int(n =100, size = 2)]

new_seeds <- c(5:13)[sample.int(size = 2)]

library(readxl)

data <- read_xlsx("InClassNotes/exercise.xlsx")
data <- read_xlsx("InClassNotes/exercise.xlsx",skip = 3)[-1,]
data[2:11] <-sapply(data[2:11], as.numeric)
colnames(data) <- tolower(colnames(data))

install.packages("palmerpenguins")
library(palmerpenguins)

library(ggplot2)
ggplot(penguins,
       aes(
         x = bill_length_mm,
         y = bill_depth_mm,
         color = species,
         shape = species,
         label = species,
         group = species
       )) + 
  stat_ellipse() + 
  geom_point() + 
  geom_label(
    data = penguins |> group_by(species) |> summarize(across(c(bill_length_mm, bill_depth_mm), mean, na.rm=T)),
    alpha = 0.8
  ) + 
  xlab("Bill Length [mm]") +
  ylab("Bill Depth [mm]") + 
  ggtitle("Relationship of Species, Bill Length, and Bill Depth",
          "Penguins observed near Palmer Station, Antarctica, 2007-2009") +
  theme_bw() + 
  theme(legend.position = 'none')

  
