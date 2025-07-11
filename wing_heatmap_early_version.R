library(MASS)
library(geomorph)
library(rgl)
library(rglwidget)

# load specimens wings
All_Wing <-readland.tps("F:/Camille Le Roy/Stage M2 Camille_2017/Morphometry/All_wings analysis/FOUR_wings.tps")
All_ID <-read.csv("F:/Camille Le Roy/Stage M2 Camille_2017/Morphometry/All_wings analysis/All_ID.csv",h=T,sep=";")


## Plotting a wing shape (found your ID in 'All_ID')
n = 178
plot(All_Wing[,,n], pch=16, type = 'l', lwd = 2, col='black', cex=0.7, axes=F, xlab='',ylab='', asp=T, main = paste(All_ID[n,2], All_ID[n,5]))


############# function to plot one Forewing  #############

plot_FW <- function(x) { 
  mp = plot(All_Wing[,,x], pch=16, type = 'l', lwd = 2, col='black', cex=0.7, axes=F, xlab='',ylab='', asp=T, main = paste(All_ID[x,2], All_ID[x,5]))
  return(mp)
}
plot_FW(51)

############# function to plot one Hindwing ############# 

# plot_HW <- function(x) {
#   mp = plot(All_Wing[,,174+x], pch=16, type = 'l', lwd = 2, col='black', cex=0.7, axes=F, xlab='',ylab='', asp=T, main = paste(All_ID[174+x,2], All_ID[174+x,5]))
#   return(mp)
# }
# plot_HW(5)

############# saving a plot serie of all Forewings (one wing at a time)
for(i in 1:172){
  mp <- plot_FW(i)
  pdf(paste0('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/01_FW_plotSerie/',
             paste0(All_ID[i,2], '_', All_ID[i,5]),'.pdf'))
  plot_FW(i)
  dev.off()
}

############# saving a plot serie of all Hindwings (one wing at a time)
for(i in 1:172){
  mp <- plot_HW(i)
  pdf(paste0('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/01_HW_plotSerie/',
             paste0(All_ID[172+i,2], '_', All_ID[172+i,5]),'.pdf'))
  plot_HW(i)
  dev.off()
}
pdf
for (i in 172:344){
  mp <- plot(All_Wing[,,i], pch=16, type = 'l', lwd = 2, col='black', cex=0.7, axes=F, xlab='',ylab='', asp=T, main = paste(All_ID[i,2], All_ID[i,5]))
  pdf(paste0('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/01_HW_plotSerie/',
             paste0(All_ID[i,2], '_', All_ID[i,5]),'.pdf'))
  plot(All_Wing[,,i], pch=16, type = 'l', lwd = 2, col='black', cex=0.7, axes=F, xlab='',ylab='', asp=T, main = paste(All_ID[i,2], All_ID[i,5]))
  dev.off()
}


### Install EBImage package 
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")
library(EBImage)
library(autoimage)

### read an image
img <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/MODEL_achilles31bis_FWL.png')
dim(img)
display(img)

### transform the image into a conventional array
img_arr <- imageData(img)

View(t(img_arr[1:100,1:100,1]))

# Clearing the unwanted pixels
# i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
# in channel 1, i.e. [,,1]
# test
img_arr2 = img_arr[1:100,1:100,1]
red <- img_arr2[11,9] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(img_arr2)) {
  for (j in 1:ncol(img_arr2)) {
    if (img_arr2[i,j] != red &
        img_arr2[i,j] != 0.0000000 &
        img_arr2[i,j] != 1.0000000 ) {img_arr2[i,j] = 0.0000000}
  }
}




# function to flip the wing in it correct position
flip_wing <- function(x){
  x2 <- apply(x, 1, rev)
  x2 <- t(x2)
  return(x2)
}



####################### Building HeatMap - All Forewings ####################### 

# 1) load initial image, convert into matrix, flip to correct position
Hmap <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/MODEL_achilles31bis_FWL.png')
Hmap <- imageData(Hmap)[,,1]
Hmap <- flip_wing(Hmap)

# 2) insert a value for the background
for (i in 1:nrow(Hmap)) {
  for (j in 1:ncol(Hmap)) {
    if(Hmap[i,j] == 1) {Hmap[i,j] = 0 }
  }
}

# 3) load next image
Hmap_1 <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/wings_for_heatmap/001_achilles02_FWL.png')
Hmap_1 <- imageData(Hmap_1)[,,1]
Hmap_1 <- flip_wing(Hmap_1)

# 4) clear the unwanted pixels, i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
red <- Hmap_1[46,10] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if (Hmap_1[i,j] != red &
        Hmap_1[i,j] != 0.0000000 &
        Hmap_1[i,j] != 1.0000000 ) {Hmap_1[i,j] = 0.0000000}
  }
}

# 5) compare pixels between matrix
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if(Hmap_1[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
  }
}

# 6) process for all images

path = 'F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/wings_for_heatmap/'
img_wings = dir(path, pattern= glob2rx("*.png"))
# looks in the specified folder and create a vector containing the names of all the .png files

for (n in 2:length(img_wings)){
  # step 3
  img <- readImage(paste0(path, img_wings[n]))
  img <- imageData(img)[,,1]
  img <- flip_wing(img)
  # step 4
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if (img[i,j] != red &
          img[i,j] != 0.0000000 &
          img[i,j] != 1.0000000 ) {img[i,j] = 0.0000000 }
    }
  }
  # step 5
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if(img[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
    }
  }
  print(paste(c(round((n/length(img_wings))*100, digits=2), "%", " done"), collapse=""))
}

autoimage(x = seq(1, dim(Hmap)[1], by = 1), y = seq(1, dim(Hmap)[2], by = 1), z = Hmap, asp = T, col=fields::tim.colors(126), xlab='damage frequency', ylab='')

# View(Hmap[1:100,1:100])
# View(Hmap_1[1:100,1:100])

max(Hmap) * 100 / 126


# exported as pdf in 12 x 7 inches




####################### Building HeatMap - Forewings Left ####################### 


# 1) load initial image, convert into matrix, flip to correct position
Hmap <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/MODEL_achilles31bis_FWL.png')
Hmap <- imageData(Hmap)[,,1]
Hmap <- flip_wing(Hmap)

# 2) insert a value for the background
for (i in 1:nrow(Hmap)) {
  for (j in 1:ncol(Hmap)) {
    if(Hmap[i,j] == 1) {Hmap[i,j] = 0 }
  }
}

# 3) load next image
Hmap_1 <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/FWL_for_heatmap/001_achilles02_FWL.png')
Hmap_1 <- imageData(Hmap_1)[,,1]
Hmap_1 <- flip_wing(Hmap_1)

# 4) clear the unwanted pixels, i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
red <- Hmap_1[46,10] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if (Hmap_1[i,j] != red &
        Hmap_1[i,j] != 0.0000000 &
        Hmap_1[i,j] != 1.0000000 ) {Hmap_1[i,j] = 0.0000000}
  }
}

# 5) compare pixels between matrix
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if(Hmap_1[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
  }
}

# 6) process for all images

path = 'F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/FWL_for_heatmap/'
img_wings = dir(path, pattern= glob2rx("*.png"))
# looks in the specified folder and create a vector containing the names of all the .png files

for (n in 2:length(img_wings)){
  # step 3
  img <- readImage(paste0(path, img_wings[n]))
  img <- imageData(img)[,,1]
  img <- flip_wing(img)
  # step 4
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if (img[i,j] != red &
          img[i,j] != 0.0000000 &
          img[i,j] != 1.0000000 ) {img[i,j] = 0.0000000 }
    }
  }
  # step 5
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if(img[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
    }
  }
  print(paste(c(round((n/length(img_wings))*100, digits=2), "%", " done"), collapse=""))
}

autoimage(x = seq(1, dim(Hmap)[1], by = 1), y = seq(1, dim(Hmap)[2], by = 1), z = Hmap, asp = T, col=fields::tim.colors(126), xlab='damage frequency', ylab='')

max(Hmap) * 100 / 63











####################### Building HeatMap - Forewings Right ####################### 


# 1) load initial image, convert into matrix, flip to correct position
Hmap <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/MODEL_achilles31bis_FWL.png')
Hmap <- imageData(Hmap)[,,1]
Hmap <- flip_wing(Hmap)

# 2) insert a value for the background
for (i in 1:nrow(Hmap)) {
  for (j in 1:ncol(Hmap)) {
    if(Hmap[i,j] == 1) {Hmap[i,j] = 0 }
  }
}

# 3) load next image
Hmap_1 <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/FWR_for_heatmap/002_achilles02_FWR.png')
Hmap_1 <- imageData(Hmap_1)[,,1]
Hmap_1 <- flip_wing(Hmap_1)

# 4) clear the unwanted pixels, i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
red <- Hmap_1[46,10] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if (Hmap_1[i,j] != red &
        Hmap_1[i,j] != 0.0000000 &
        Hmap_1[i,j] != 1.0000000 ) {Hmap_1[i,j] = 0.0000000}
  }
}

# 5) compare pixels between matrix
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if(Hmap_1[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
  }
}

# 6) process for all images

path = 'F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/FWR_for_heatmap/'
img_wings = dir(path, pattern= glob2rx("*.png"))
# looks in the specified folder and create a vector containing the names of all the .png files

for (n in 2:length(img_wings)){
  # step 3
  img <- readImage(paste0(path, img_wings[n]))
  img <- imageData(img)[,,1]
  img <- flip_wing(img)
  # step 4
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if (img[i,j] != red &
          img[i,j] != 0.0000000 &
          img[i,j] != 1.0000000 ) {img[i,j] = 0.0000000 }
    }
  }
  # step 5
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if(img[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
    }
  }
  print(paste(c(round((n/length(img_wings))*100, digits=2), "%", " done"), collapse=""))
}

autoimage(x = seq(1, dim(Hmap)[1], by = 1), y = seq(1, dim(Hmap)[2], by = 1), z = Hmap, asp = T, col=fields::tim.colors(126), xlab='damage frequency', ylab='')

max(Hmap) * 100 / 63








####################### Building HeatMap - All Hindwings ####################### 

# 1) load initial image, convert into matrix, flip to correct position
Hmap <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/hindwings_for_heatmap/040_achilles31bis_HWL.png')
Hmap <- imageData(Hmap)[,,1]
Hmap <- flip_wing(Hmap)

# 2) insert a value for the background
for (i in 1:nrow(Hmap)) {
  for (j in 1:ncol(Hmap)) {
    if(Hmap[i,j] == 1) {Hmap[i,j] = 0 }
  }
}

# 3) load next image
Hmap_1 <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/hindwings_for_heatmap/001_achilles02_HWL.png')
Hmap_1 <- imageData(Hmap_1)[,,1]
Hmap_1 <- flip_wing(Hmap_1)

# 4) clear the unwanted pixels, i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
red <- Hmap_1[62,6] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if (Hmap_1[i,j] != red &
        Hmap_1[i,j] != 0.0000000 &
        Hmap_1[i,j] != 1.0000000 ) {Hmap_1[i,j] = 0.0000000}
  }
}

# 5) compare pixels between matrix
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if(Hmap_1[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
  }
}

# 6) process for all images

path = 'F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/hindwings_for_heatmap/'
img_wings = dir(path, pattern= glob2rx("*.png"))
# looks in the specified folder and create a vector containing the names of all the .png files

for (n in 2:length(img_wings)){
  # step 3
  img <- readImage(paste0(path, img_wings[n]))
  img <- imageData(img)[,,1]
  img <- flip_wing(img)
  # step 4
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if (img[i,j] != red &
          img[i,j] != 0.0000000 &
          img[i,j] != 1.0000000 ) {img[i,j] = 0.0000000 }
    }
  }
  # step 5
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if(img[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
    }
  }
  print(paste(c(round((n/length(img_wings))*100, digits=2), "%", " done"), collapse=""))
}

autoimage(x = seq(1, dim(Hmap)[1], by = 1), y = seq(1, dim(Hmap)[2], by = 1), z = Hmap, asp = T, col=fields::tim.colors(126), xlab='damage frequency', ylab='')

# View(Hmap[1:100,1:100])
# View(Hmap_1[1:100,1:100])

max(Hmap) * 100 / 126


# exported as pdf in 12 x 7 inches






####################### Building HeatMap - Hindwings Left ####################### 


# 1) load initial image, convert into matrix, flip to correct position
Hmap <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/hindwings_for_heatmap/040_achilles31bis_HWL.png')
Hmap <- imageData(Hmap)[,,1]
Hmap <- flip_wing(Hmap)

# 2) insert a value for the background
for (i in 1:nrow(Hmap)) {
  for (j in 1:ncol(Hmap)) {
    if(Hmap[i,j] == 1) {Hmap[i,j] = 0 }
  }
}

# 3) load next image
Hmap_1 <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/HWL_for_heatmap/001_achilles02_HWL.png')
Hmap_1 <- imageData(Hmap_1)[,,1]
Hmap_1 <- flip_wing(Hmap_1)

# 4) clear the unwanted pixels, i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
red <- Hmap_1[62,6] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if (Hmap_1[i,j] != red &
        Hmap_1[i,j] != 0.0000000 &
        Hmap_1[i,j] != 1.0000000 ) {Hmap_1[i,j] = 0.0000000}
  }
}

# 5) compare pixels between matrix
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if(Hmap_1[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
  }
}

# 6) process for all images

path = 'F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/HWL_for_heatmap/'
img_wings = dir(path, pattern= glob2rx("*.png"))
# looks in the specified folder and create a vector containing the names of all the .png files

for (n in 2:length(img_wings)){
  # step 3
  img <- readImage(paste0(path, img_wings[n]))
  img <- imageData(img)[,,1]
  img <- flip_wing(img)
  # step 4
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if (img[i,j] != red &
          img[i,j] != 0.0000000 &
          img[i,j] != 1.0000000 ) {img[i,j] = 0.0000000 }
    }
  }
  # step 5
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if(img[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
    }
  }
  print(paste(c(round((n/length(img_wings))*100, digits=2), "%", " done"), collapse=""))
}

autoimage(x = seq(1, dim(Hmap)[1], by = 1), y = seq(1, dim(Hmap)[2], by = 1), z = Hmap, asp = T, col=fields::tim.colors(126), xlab='damage frequency', ylab='')

max(Hmap) * 100 / 63











####################### Building HeatMap - Hindwings Right ####################### 


# 1) load initial image, convert into matrix, flip to correct position
Hmap <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/hindwings_for_heatmap/041_achilles31bis_HWR.png')
Hmap <- imageData(Hmap)[,,1]
Hmap <- flip_wing(Hmap)

# 2) insert a value for the background
for (i in 1:nrow(Hmap)) {
  for (j in 1:ncol(Hmap)) {
    if(Hmap[i,j] == 1) {Hmap[i,j] = 0 }
  }
}

# 3) load next image
Hmap_1 <- readImage('F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/HWR_for_heatmap/002_achilles02_HWR.png')
Hmap_1 <- imageData(Hmap_1)[,,1]
Hmap_1 <- flip_wing(Hmap_1)

# 4) clear the unwanted pixels, i.e. removing all values that differ from RED (0.7294118), BLACK (0.0000000) and background (1.0000000)
red <- Hmap_1[62,6] # because the red color (0.7294118) is not recognize 
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if (Hmap_1[i,j] != red &
        Hmap_1[i,j] != 0.0000000 &
        Hmap_1[i,j] != 1.0000000 ) {Hmap_1[i,j] = 0.0000000}
  }
}

# 5) compare pixels between matrix
for (i in 1:nrow(Hmap_1)) {
  for (j in 1:ncol(Hmap_1)) {
    if(Hmap_1[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
  }
}

# 6) process for all images

path = 'F:/Camille Le Roy/PhD_Camille/09-Wing Damage and Flight/heatmap_wing_damge/Hindwings Heat Map/HWR_for_heatmap/'
img_wings = dir(path, pattern= glob2rx("*.png"))
# looks in the specified folder and create a vector containing the names of all the .png files

for (n in 2:length(img_wings)){
  # step 3
  img <- readImage(paste0(path, img_wings[n]))
  img <- imageData(img)[,,1]
  img <- flip_wing(img)
  # step 4
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if (img[i,j] != red &
          img[i,j] != 0.0000000 &
          img[i,j] != 1.0000000 ) {img[i,j] = 0.0000000 }
    }
  }
  # step 5
  for (i in 1:nrow(img)) {
    for (j in 1:ncol(img)) {
      if(img[i,j] == red) {Hmap[i,j] = Hmap[i,j] + 1 }
    }
  }
  print(paste(c(round((n/length(img_wings))*100, digits=2), "%", " done"), collapse=""))
}

autoimage(x = seq(1, dim(Hmap)[1], by = 1), y = seq(1, dim(Hmap)[2], by = 1), z = Hmap, asp = T, col=fields::tim.colors(126), xlab='damage frequency', ylab='')

max(Hmap) * 100 / 63








