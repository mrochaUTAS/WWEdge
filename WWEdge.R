### Internal Checking area calculations ######
### Author: Manuel Rocha
##############################################
#install.packages("BiocManager", dependencies = T)
#BiocManager::install("EBImage")
library("EBImage")
library("stringr")
library("AFM")
library("dplyr")
library("ggplot2")
library("grid")
library("GGally")
library("extendedForest")
###################################################################
### Step 1 : Total Area and checks area measurement of dried wedges
###################################################################

### Set working directory DESK HARD DISK
pathName_GW <- "D:/wedges_backup_change_resolution/wedges/green_w_low_resolution/single_pics_mod1/"
pathName_DW <- "D:/wedges_backup_change_resolution/wedges/dried_w_low_resolution/single_mod1/"

### Set working directory SSD
pathName_GW <- "D:/Area_Detection/green_w_low_resolution/single_pics/"
pathName_DW <- "D:/Area_Detection/dried_w_low_resolution/single_pics/"

### Set working directory DESK HARD DISK
pathName_GW <- "E:/green_w_low_resolution/single_pics_mod1/"
pathName_DW <- "E:/dried_w_low_resolution/single_mod1/"

scoring <- read.csv("Scoring.csv")
scoring$treeID<- paste(scoring$site2,"_",scoring$Tree,"_",scoring$wedge,"_",scoring$side,sep="")
scoring$treeID2<- paste(scoring$site2,"_",scoring$Tree,"_",scoring$wedge,sep="")


SideCorrection <- read.csv("Wedge_Side_corrections.csv")
SideCorrection$TreeID <- paste(SideCorrection$site,"_",SideCorrection$tree,"_",SideCorrection$wedge,"_",SideCorrection$side2, sep="")


filelist_GW <- list.files(path = pathName_GW , pattern = ".jpg")
filelist_DW <- list.files(path = pathName_DW , pattern = ".jpg")

filelist_GW <- filelist_GW[str_detect(filelist_GW, "_A_")] # filter by A

wedge_full_ID <- unlist((strsplit(filelist_GW[-c(142)], "_"))) ## exlude record 142. need to check
greenID1 <- c()
greenID2 <- c()
greenID3 <- c()
greenID4 <- c()
for (i in seq(1,length(wedge_full_ID),4)) {
  IDss <- wedge_full_ID[i]
  greenID1 <- c(greenID1,IDss)
}
for (i in seq(2,length(wedge_full_ID),4)) {
  IDss <- wedge_full_ID[i]
  greenID2 <- c(greenID2,IDss)
}
for (i in seq(3,length(wedge_full_ID),4)) {
  IDss <- wedge_full_ID[i]
  greenID3 <- c(greenID3,IDss)
}
for (i in seq(4,length(wedge_full_ID),4)) {
  IDss <- wedge_full_ID[i]
  greenID4 <- c(greenID4,IDss)
}
wedgeDF_ID <- cbind.data.frame(site=greenID1,tree=greenID2,wedge=greenID3,side=greenID4,idd=1:length(greenID1))
wedgeDF_ID$tree <- as.numeric(as.character(wedgeDF_ID$tree))
wedgeDF_ID$treeID2 <- paste(wedgeDF_ID$site,"_",wedgeDF_ID$tree,"_",wedgeDF_ID$wedge, sep="")
wedgeDF_ID2 <- merge(wedgeDF_ID,scoring, by="treeID2")
wedgeDF_ID2 <- wedgeDF_ID2[order(-wedgeDF_ID2$Score_wedgeA, wedgeDF_ID2$Treatment, wedgeDF_ID2$site),]

# ### Run only for subsampling ####################################################################
# subsample1 <- c(
#   "F_44_A",  "F_12_A",  "G_32_A",  "G_44_A",  "U_27_A",  "U_23_A", ### score 5-6
#   "F_46_A",  "F_5_A",  "G_24_A",  "G_16_A",  "U_2_A",  "U_48_A", ### score 2-4 outerwood
#   "F_37_A",  "F_17_A",  "G_42_A",  "G_2_A",  "U_29_A",  "U_13_A", ### score 2-4 corewood
#   "F_40_A",  "F_26_A",  "G_22_A",  "G_43_A",  "U_7_A",  "U_22_A" ### score 1
#   
# )
# wedgeDF_ID2 <- wedgeDF_ID2[which(wedgeDF_ID2$treeID2 %in% subsample1),]
# wedgeDF_ID2 <- wedgeDF_ID2[order(match(wedgeDF_ID2$treeID2,subsample1)),]
# #################################################################################################


### Run only for subsampling ####################################################################
# subsample1 <- c(
#   "F_1_A",
#   "F_3_A",
#   "F_4_A",
#   "F_5_A",
#   "F_6_A",
#   "F_7_A",
#   "F_8_A",
#   "F_10_A",
#   "F_11_A",
#   "F_12_A",
#   "F_13_A",
#   "F_14_A",
#   "F_15_A",
#   "F_16_A",
#   "F_17_A",
#   "F_18_A",
#   "F_19_A",
#   "F_20_A",
#   "F_21_A",
#   "F_22_A",
#   "F_24_A",
#   "F_25_A",
#   "F_26_A",
#   "F_27_A",
#   "F_28_A",
#   "F_29_A",
#   "F_30_A",
#   "F_40_A",
#   "F_46_A",
#   "G_2_A",
#   "U_2_A",
#   "U_7_A",
#   "U_13_A",
#   "U_29_A"
# )

subsample1 <- c(
  
  # Score 2
  "F_6_A",
  "F_8_A",
  "F_14_A",
  "F_15_A",
  "F_16_A",
  
  # Score 3
  "F_29_A",
  "F_18_A",
  "F_34_A",
  "F_35_A",
  "F_38_A",
  
  # Score 4
  "F_5_A",
  "F_17_A",
  "F_31_A",
  "F_37_A",
  "F_46_A",
  
  # Score 5
  "F_13_A",
  "F_19_A",
  "F_40_A",
  "G_43_A",
  "F_48_A",
  
  # Score 6
  "F_7_A",
  "F_25_A",
  "F_33_A",
  "G_4_A",
  "G_22_A"
)


wedgeDF_ID2 <- wedgeDF_ID2[which(wedgeDF_ID2$treeID2 %in% subsample1),]
wedgeDF_ID2 <- wedgeDF_ID2[order(match(wedgeDF_ID2$treeID2,subsample1)),]
#################################################################################################


selection <- wedgeDF_ID2$idd

GW_Data <- c() # Area
DW_Data1 <- c() # Area
DW_Data2 <- c() # Checks
DW_Data3 <- c() # Radial and Tangential shrinkage
DW_Data4 <- c() # wedge hat area
DW_Data5 <- c() # lengths of L-side , R-side  edges (using dried weges) 
DW_Data6 <- c() # wedge triangle (green and dried)
DW_Data7 <- c() # Local curvature profiles (L-side and R-side using dried wedges)
CLength <- c()

# select1 <- c(50:51) # progress in batches

filelist_GW <- filelist_GW[-c(142)]

pdf(file=paste("D:/wedges_backup_change_resolution/wedges/automated_process.pdf", sep = ""), width = 7, height = 9, paper = "a4" ) # saving plots in pdf file


for (i in selection) { # select1  
  
  if (filelist_GW[i] %in% filelist_DW) {
    
    ### Reading GW image
    pic0 <- readImage( paste(pathName_GW,filelist_GW[i],sep="") ) #  "F7A_s1_mod.jpg"
    pic01 <- resize(pic0,2040)
    
    ####################################
    ####  Green wedge area calculation
    ####################################
    
    ### keeping original picture
    pic <- gblur(resize(pic0[,,1],2040), sigma = 5)
    
    ### Removing unused objects
    #rm(list=c("pic0"))
    
    ### Changing to grayscale
    colorMode(pic) <- Grayscale
    # display(pic)
    
    #### Pic preparation
    pic_mod <- pic
    pic_mod <- pic_mod*1.5
    #display(pic_mod)
    
    ### Thresholding a standard photo (black background)
    thr3 <- thresh(pic_mod, w=1019, h=5, offset=0.15)
    #display(thr3, all=T)
    
    ### Filling holes and removing small artifacts
    nmaskf = fillHull(dilate(thr3, makeBrush(3, shape='disc'))) ## 15 does not show checks
    dmap = distmap(nmaskf)
    #display(dmap, all=T, method="raster")
    
    ### Detecting independent objects by neighbours distance
    nmask = watershed(dmap, tolerance = 5, ext=1)
    # display( combine(colorLabels(getFrame(nmask, 1)),
    #                  toRGB(getFrame(pic0, 1))), all=TRUE )
    
    ### Computing objects features (Area, perimeter and location xy)
    area_wedge <- as.data.frame(computeFeatures.shape(nmask, pic01) )
    filter_obj <- area_wedge[which(area_wedge$s.area<=20000),]
    nmask[nmask %in% as.numeric(rownames(filter_obj))] <- 0
    area_wedge <- as.data.frame(computeFeatures.shape(nmask, pic01)) 
    xy_wedge <- as.data.frame(computeFeatures.moment(nmask, pic01))
    wedge_full <- cbind.data.frame(area_wedge,xy_wedge)
    wedge_full$treeID <- paste(filelist_GW[i],sep = "")
    GW_Data <- rbind.data.frame(GW_Data,wedge_full)
    
    
    ### Corner detection and vertical alignment
    x = nmask
    contours2 = ocontour(bwlabel(x))
    local_curv01 = localCurvature(x=contours2[[1]], h=50) ### 
    # plot(c(local_curv01$curvature))
    
    findPeaks<-  function (x, m = 3){
      shape <- diff(sign(diff(x, na.pad = FALSE)))
      pks <- sapply(which(shape < 0), FUN = function(i){
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
      })
      pks <- unlist(pks)
      pks
    }
    
    valleys1 <- findPeaks(-local_curv01$curvature, m=500)
    corners1 <-  local_curv01$contour[valleys1,]

    
    ##### Display
    # pic0_GS1_area <- resize(pic0,2040)
    # nmask_GS1_area <- resize(nmask,2040)
    
    ### selecting pith (click wedge pith position on the plot)
    # display(pic01, all=TRUE, method="raster" ) #
    # text(x = 600, y = 150,
    #      label = paste("i=",i," ",filelist_GW[i],"",sep=""),
    #                    adj = c(0,1), col = "white", cex = 2.6)
    # 
    # p <- locator(4, type = "l", pch = 3, col = "red")
    # p <- cbind.data.frame(pith_x=p$x[1],
    #                       pith_y=p$y[1],
    #                       mid_x=p$x[2],
    #                       mid_y=p$y[2],
    #                       left_x=p$x[3],
    #                       left_y=p$y[3],
    #                       right_x=p$x[4],
    #                       right_y=p$y[4]
    # )
    # p$treeID <- paste(filelist_GW[i],sep = "")
    # p$type <- "Green"
    # DW_Data3 <- rbind.data.frame(DW_Data3,p)
    
    ############### Automatic detection of shrinkage points##############################
    xxx <- imageData(nmask)
    xxx1 <- as.data.frame(which(nmask==1, arr.ind=TRUE))
    # left <- c(  min(xxx1$row), min( xxx1[which(xxx1$row==min(xxx1$row)),]$col )  ) #Left
    # right <- c(  max(xxx1$row), min( xxx1[which(xxx1$row==max(xxx1$row)),]$col )  ) #Right
    # pith <- c( ( xxx1[which(xxx1$col==max(xxx1$col)),]$row )[as.integer( (  length(xxx1[which(xxx1$col==max(xxx1$col)),]$row)/2  ))] , max(xxx1$col)   ) #Pith
    
    left <- corners1[1,]
    pith <- corners1[2,]
    right <- corners1[3,]
    
    mid_x <- as.integer(sqrt((left[1]-right[1])^2 + (left[2]-right[2])^2)/2+left[1])
    mid_y <- min(which(xxx[mid_x,]==1)) ## mid point located at the top of the wedges
    mid <- c(mid_x, mid_y)
    p <- cbind.data.frame(pith_x=as.integer(pith[1]),
                          pith_y=as.integer(pith[2]),
                          mid_x=as.integer(mid[1]),
                          mid_y=as.integer(mid[2]),
                          left_x=as.integer(left[1]),
                          left_y=as.integer(left[2]),
                          right_x=as.integer(right[1]),
                          right_y=as.integer(right[2])
    )
    p$treeID <- paste(filelist_GW[i],sep = "")
    p$type <- "Green"
    DW_Data3 <- rbind.data.frame(DW_Data3,p)
    
    
    # ### Corner detection method 1
    # display(pic01, all=TRUE, method="raster" )
    # points(pith[1], pith[2] , col = "red", pch = 13, cex = 1.5)
    # points(left[1], left[2] , col = "red", pch = 13, cex = 1.5)
    # points(right[1], right[2] , col = "red", pch = 13, cex = 1.5)
    # points(mid[1], mid[2] , col = "red", pch = 13, cex = 1.5)
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of pith, mid, left and right positions ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(filelist_GW[i]," Green",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # 
    # ### Corner detection method 2
    # display(pic01, all=TRUE, method="raster" )
    # points(corners1[2,1], corners1[2,2] , col = "red", pch = 13, cex = 1.5)
    # points(corners1[1,1], corners1[1,2] , col = "red", pch = 13, cex = 1.5)
    # points(corners1[3,1], corners1[3,2] , col = "red", pch = 13, cex = 1.5)
    # points(mid[1], mid[2] , col = "red", pch = 13, cex = 1.5)
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of pith, mid, left and right positions ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(filelist_GW[i]," Green",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    
    
    
    
    
    ####################################################################################
    
    
    
    ##### Automatic calculation of wedge hat area #######################################
    #display(nmask)
    x = nmask
    contours1 = as.data.frame(ocontour(x)[[1]])
    x1=contours1[min(which(contours1$V1==(p$left_x))),]$V1
    y1=contours1[min(which(contours1$V1==(p$left_x))),]$V2
    x2=contours1[max(which(contours1$V1==(p$right_x))) ,]$V1
    y2=contours1[max(which(contours1$V1==(p$right_x))) ,]$V2
    m=(y2-y1)/(x2-x1)
    x_range <- (x1+1):(x2-1)
    y_output <- as.integer(m*x_range - m*x1 + y1)
    line_AB <- cbind.data.frame(V1=x_range,V2=y_output)
    contours2 <- rbind.data.frame(contours1[1:min(which(contours1$V1==(p$left_x))) ,],
                                  line_AB,
                                  contours1[max(which(contours1$V1==(p$right_x))):nrow(contours1) ,])
    pos2 = array(0, dim(x))
    pos2[as.matrix(contours2)]  = 1
    pos2 <- fillHull(pos2)
    
    # display(colorLabels(nmask+pos2), all=TRUE, method = "raster")
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of wedge hat area ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(filelist_GW[i]," Green",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    
    
    wedge_hat_area <- as.data.frame(computeFeatures.shape(pos2, pic01))
    wedge_hat_area$treeID <- paste(filelist_GW[i],sep = "")
    wedge_hat_area$type <- "Green"
    wedge_hat_area$type2 <- "hat"
    DW_Data4 <- rbind.data.frame(DW_Data4,wedge_hat_area)
    
    
    greenWedgeSegmentation <- nmask+pos2
    
    # Triangle lenght segment LEFT (Left to Pith) 
    x1=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V1
    y1=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V2
    x2=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y2=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    line_AB1 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos3 = array(0, dim(x))
    pos3[as.matrix(line_AB1)]  = 1
    seg_LEFT <- as.data.frame(computeFeatures.shape(pos3, pic01))
    seg_LEFT$treeID <- paste(filelist_GW[i],sep = "")
    seg_LEFT$type <- "Green"
    seg_LEFT$type2 <- "Segment_L"
    
    # Triangle lenght segment RIGHT (Pith to Right)
    x1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    x2=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V1
    y2=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V2
    line_AB2 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos4 = array(0, dim(x))
    pos4[as.matrix(line_AB2)]  = 1
    seg_RIGHT <- as.data.frame(computeFeatures.shape(pos4, pic01))
    seg_RIGHT$treeID <- paste(filelist_GW[i],sep = "")
    seg_RIGHT$type <- "Green"
    seg_RIGHT$type2 <- "Segment_R"
    
    # Triangle lenght segment TOP (Right to Left)
    x1=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V1
    y1=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V2
    x2=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V1
    y2=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V2
    line_AB3 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos5 = array(0, dim(x))
    pos5[as.matrix(line_AB3)]  = 1
    seg_TOP <- as.data.frame(computeFeatures.shape(pos5, pic01))
    seg_TOP$treeID <- paste(filelist_GW[i],sep = "")
    seg_TOP$type <- "Green"
    seg_TOP$type2 <- "Segment_T"
    
    # Triangle lenght segment MID (pith to middle of top segment - within the triangular section)
    mid_pos <- as.integer(nrow(line_AB3)/2)
    x1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    x2 <- line_AB3[mid_pos]$x
    y2 <- line_AB3[mid_pos]$y
    line_AB4 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos6 = array(0, dim(x))
    pos6[as.matrix(line_AB4)]  = 1
    seg_MID <- as.data.frame(computeFeatures.shape(pos6, pic01))
    seg_MID$treeID <- paste(filelist_GW[i],sep = "")
    seg_MID$type <- "Green"
    seg_MID$type2 <- "Segment_M"
    
    ### Triangle Area
    contours3 <- rbind.data.frame(line_AB1,
                                  line_AB2,
                                  line_AB3
    )
    pos7 = array(0, dim(x))
    pos7[as.matrix(contours3)]  = 1
    pos7 <- fillHull(pos7)
    tri_area <-  as.data.frame(computeFeatures.shape(pos7, pic01))
    tri_area$treeID <- paste(filelist_GW[i],sep = "")
    tri_area$type <- "Green"
    tri_area$type2 <- "Area"
    
    ### Pith to top mid point
    seg_Pith_to_Top <- getBresenham2DSegment(pith[1],pith[2],mid[1],mid[2])
    pos7 = array(0, dim(x))
    pos7[as.matrix(seg_Pith_to_Top)]  = 1
    seg_PT <- as.data.frame(computeFeatures.shape(pos7, pic01))
    seg_PT$treeID <- paste(filelist_GW[i],sep = "")
    seg_PT$type <- "Green"
    seg_PT$type2 <- "seg_Pith_to_Top"
    
    DW_Data6 <- rbind.data.frame(DW_Data6,
                                 seg_LEFT,
                                 seg_RIGHT,
                                 seg_TOP,
                                 seg_MID,
                                 seg_PT,
                                 tri_area) 
    
    # display(colorLabels(pos6 + nmask), all=TRUE)
    
    ####################################################################################
    

    
    ####################################################
    ### Dried wedges - Wood interal check calculations
    ####################################################
    
    ### Reading DW-S1 image
    wedge_ID <- (strsplit(filelist_GW[i], "_"))[[1]][1:3]
    
    ### side correction
    if ( paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1",sep = "") %in% SideCorrection$TreeID ) {
      DW_s1 <- paste(pathName_DW,wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep = "")
      DW_s2 <- paste(pathName_DW,wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
    } else { 
      DW_s1 <- paste(pathName_DW,wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
      DW_s2 <- paste(pathName_DW,wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep = "")
    }
    
    
    pic0 <- readImage( DW_s1 ) #  "F7A_s1_mod.jpg"
    pic01 <- resize(pic0,2040)
    pic02 <- resize(pic0,2040)
    
    #####################################
    ####  Dried wedge area calculation
    #####################################
    
    ### keeping original picture
    pic <- gblur( resize(pic0[,,1],2040) , sigma = 5)
    
    ### Removing unused objects
    #rm(list=c("pic0"))
    
    ### Changing to grayscale
    colorMode(pic) <- Grayscale
    # display(pic)
    
    #### Pic preparation
    pic_mod <- pic
    pic_mod <- pic_mod*1.5
    #display(pic_mod)
    
    ### Thresholding a standard photo (black background)
    thr3 <- thresh(pic_mod, w=1019, h=1, offset=0.15)
    #display(thr3, all=T)
    
    ### Filling holes and removing small artifacts
    nmaskf = fillHull(dilate(thr3, makeBrush(3, shape='disc'))) ## 15 does not show checks
    dmap = distmap(nmaskf)
    #display(dmap, all=T, method="raster")
    
    ### Detecting independent objects by neighbours distance
    nmask = watershed(dmap, tolerance = 1, ext=50)
    # display( combine(colorLabels(getFrame(nmask, 1)),
    #                  toRGB(getFrame(pic0, 1))), all=TRUE )
    
    ### Computing objects features (Area, perimeter and location xy)
    area_wedge <- as.data.frame(computeFeatures.shape(nmask, pic01) )
    filter_obj <- area_wedge[which(area_wedge$s.area<=20000),]
    nmask[nmask %in% as.numeric(rownames(filter_obj))] <- 0
    area_wedge <- as.data.frame(computeFeatures.shape(nmask, pic01)) 
    xy_wedge <- as.data.frame(computeFeatures.moment(nmask, pic01))
    wedge_full <- cbind.data.frame(area_wedge,xy_wedge)
    wedge_full$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep = "")
    DW_Data1 <- rbind.data.frame(DW_Data1,wedge_full)
    
    nmaskD1 <- nmask
    
    ### Corner detection and vertical alignment
    x = nmask
    contours2 = ocontour(bwlabel(x))
    local_curv01 = localCurvature(x=contours2[[1]], h=50) ### 
    # plot(c(local_curv01$curvature))
    
    findPeaks<-  function (x, m = 3){
      shape <- diff(sign(diff(x, na.pad = FALSE)))
      pks <- sapply(which(shape < 0), FUN = function(i){
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
      })
      pks <- unlist(pks)
      pks
    }
    
    valleys1 <- findPeaks(-local_curv01$curvature, m=500)
    corners1 <-  local_curv01$contour[valleys1,]
    
    
    ### selecting pith (click wedge pith position on the plot)
    # display(pic01, all=TRUE, method="raster") #
    # text(x = 600, y = 150,
    #      label = paste("i=",i," ",wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg"," Dried",sep=""),
    #      adj = c(0,1), col = "white", cex = 2.6)
    # 
    # p <- locator(4, type = "l", pch = 3, col = "red")
    # p <- cbind.data.frame(pith_x=p$x[1],
    #                       pith_y=p$y[1],
    #                       mid_x=p$x[2],
    #                       mid_y=p$y[2],
    #                       left_x=p$x[3],
    #                       left_y=p$y[3],
    #                       right_x=p$x[4],
    #                       right_y=p$y[4]
    # )
    # p$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep = "")
    # p$type <- "Dried"
    # DW_Data3 <- rbind.data.frame(DW_Data3,p)
    
    ############### Automatic detection of shrinkage points##############################
    xxx <- imageData(nmask)
    xxx1 <- as.data.frame(which(nmask==1, arr.ind=TRUE))
    # left <- c(  min(xxx1$row), min( xxx1[which(xxx1$row==min(xxx1$row)),]$col )  ) #Left
    # right <- c(  max(xxx1$row), min( xxx1[which(xxx1$row==max(xxx1$row)),]$col )  ) #Right
    # pith <- c( ( xxx1[which(xxx1$col==max(xxx1$col)),]$row )[as.integer( (  length(xxx1[which(xxx1$col==max(xxx1$col)),]$row)/2  ))] , max(xxx1$col)   ) #Pith
    
    left <- corners1[1,]
    pith <- corners1[2,]
    right <- corners1[3,]
    
    mid_x <- as.integer(sqrt((left[1]-right[1])^2 + (left[2]-right[2])^2)/2+left[1])
    mid_y <- min(which(xxx[mid_x,]==1)) 
    mid <- c(mid_x, mid_y)
    p <- cbind.data.frame(pith_x=as.integer(pith[1]),
                          pith_y=as.integer(pith[2]),
                          mid_x=as.integer(mid[1]),
                          mid_y=as.integer(mid[2]),
                          left_x=as.integer(left[1]),
                          left_y=as.integer(left[2]),
                          right_x=as.integer(right[1]),
                          right_y=as.integer(right[2])
    )
    p$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    p$type <- "Dried"
    DW_Data3 <- rbind.data.frame(DW_Data3,p)
    
    
    # display(pic01, all=TRUE, method="raster" )
    # points(pith[1], pith[2] , col = "red", pch = 13, cex = 1.5)
    # points(left[1], left[2] , col = "red", pch = 13, cex = 1.5)
    # points(right[1], right[2] , col = "red", pch = 13, cex = 1.5)
    # points(mid[1], mid[2] , col = "red", pch = 13, cex = 1.5)
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of pith, mid, left and right positions ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg"," Dried",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    
    
    ####################################################################################
    
    
    
    ##### Automatic calculation of wedge hat area #######################################
    #display(nmask)
    x = nmask
    contours1 = as.data.frame(ocontour(x)[[1]])
    x1=contours1[min(which(contours1$V1==(p$left_x))),]$V1
    y1=contours1[min(which(contours1$V1==(p$left_x))),]$V2
    x2=contours1[max(which(contours1$V1==(p$right_x))) ,]$V1
    y2=contours1[max(which(contours1$V1==(p$right_x))) ,]$V2
    m=(y2-y1)/(x2-x1)
    x_range <- (x1+1):(x2-1)
    y_output <- as.integer(m*x_range - m*x1 + y1)
    line_AB <- cbind.data.frame(V1=x_range,V2=y_output)
    contours2 <- rbind.data.frame(contours1[1:min(which(contours1$V1==(p$left_x))) ,],
                                  line_AB,
                                  contours1[max(which(contours1$V1==(p$right_x))):nrow(contours1) ,])
    pos2 = array(0, dim(x))
    pos2[as.matrix(contours2)]  = 1
    pos2 <- fillHull(pos2)
    
    ### Section
    contours3 <- rbind.data.frame(line_AB,
                                  contours1[min(which(contours1$V1==(p$left_x))):min(which(contours1$V2==(p$pith_y))) ,],
                                  contours1[min(which(contours1$V2==(p$pith_y))):max(which(contours1$V1==(p$right_x))) ,])
    pos10 = array(0, dim(x))
    pos10[as.matrix(contours3)]  = 1
    pos10 <- fillHull(pos10)
    sec_area <-  as.data.frame(computeFeatures.shape(pos10, pic01))
    sec_area$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    sec_area$type <- "Dried"
    sec_area$type2 <- "section"
    
    
    # display(colorLabels(nmask+pos2), all=TRUE, method="raster")
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of wedge hat ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg"," Dried",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    
    ### Wedge hat 
    wedge_hat_area <- as.data.frame(computeFeatures.shape(pos2, pic01))
    wedge_hat_area$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    wedge_hat_area$type <- "Dried"
    wedge_hat_area$type2 <- "hat"
    
    DW_Data4 <- rbind.data.frame(DW_Data4,wedge_hat_area)
    
    
    driedWedgeSegmentation1 <- nmask+pos2
    
    
    # Triangle lenght segment LEFT (Left to Pith) 
    x1=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V1
    y1=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V2
    x2=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y2=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    line_AB1 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos3 = array(0, dim(x))
    pos3[as.matrix(line_AB1)]  = 1
    seg_LEFT <- as.data.frame(computeFeatures.shape(pos3, pic01))
    seg_LEFT$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    seg_LEFT$type <- "Dried"
    seg_LEFT$type2 <- "Segment_L"
    
    
    
    
    #########################################################
    ### Depth collapse measurement
    #########################################################
    mL= - ((y2-y1)/(x2-x1))^(-1)
    res2 <- c()
    nmask_test <- nmaskD1*0
    
    for (kk in seq(50,nrow(line_AB1)-50,10) )  { # seq(50,(nrow(line_AB1)-50),10)
          x1=line_AB1[kk,]$x
          y1=line_AB1[kk,]$y
          iindex=0
    while(iindex<100) {
      iindex=iindex+1
      xx2=(x1+iindex)
      yL= mL*xx2 - mL*x1 + y1
      yL_nmask <- nmaskD1[xx2,yL]
      res1 <- cbind.data.frame(x2=xx2, y2=as.integer(yL), yL_nmask=yL_nmask, lineid=kk)
      res2 <- rbind(res2,res1)
    }
          
          line1 <- getBresenham2DSegment(x1,y1,xx2,as.integer(yL))
          pos9 = array(0, dim(nmaskD1))
          pos9[as.matrix(line1)]  = 1
          nmask_test <- nmask_test + nmaskD1 + pos9
          
    }
    
    
    CLengthL <- res2[which(res2$yL_nmask==0),] %>%
      dplyr::group_by(lineid) %>%
      dplyr::summarise(
        number_auto = sum(!is.na(yL_nmask))
      )
    
    
    CLengthL$side <- "Left"
    CLengthL$TLength <- nrow(line_AB1)
    CLengthL$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    
    # display(colorLabels(nmask_test+pos3))
    
    ###############################################################
    ###############################################################
    
    
    # Triangle lenght segment RIGHT (Pith to Right)
    x1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    x2=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V1
    y2=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V2
    line_AB2 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos4 = array(0, dim(x))
    pos4[as.matrix(line_AB2)]  = 1
    seg_RIGHT <- as.data.frame(computeFeatures.shape(pos4, pic01))
    seg_RIGHT$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    seg_RIGHT$type <- "Dried"
    seg_RIGHT$type2 <- "Segment_R"
    
    
    #########################################################
    ### Depth collapse measurement
    #########################################################
    mL= - ((y2-y1)/(x2-x1))^(-1)
    res2 <- c()
    nmask_test <- nmaskD1*0
    
    for (kk in seq(50,nrow(line_AB2)-50,10) )  { # seq(50,(nrow(line_AB2)-50),10)
      x1=line_AB2[kk,]$x
      y1=line_AB2[kk,]$y
      iindex=0
      while(iindex<100) {
        iindex=iindex+1
        xx2=(x1-iindex)
        yL= mL*xx2 - mL*x1 + y1
        yL_nmask <- nmaskD1[xx2,yL]
        res1 <- cbind.data.frame(x2=xx2, y2=as.integer(yL), yL_nmask=yL_nmask, lineid=kk)
        res2 <- rbind(res2,res1)
      }
      
      line1 <- getBresenham2DSegment(x1,y1,xx2,as.integer(yL))
      pos9 = array(0, dim(nmaskD1))
      pos9[as.matrix(line1)]  = 1
      nmask_test <- nmask_test + nmaskD1 + pos9
      
    }
    
    
    CLengthR <- res2[which(res2$yL_nmask==0),] %>%
      dplyr::group_by(lineid) %>%
      dplyr::summarise(
        number_auto = sum(!is.na(yL_nmask))
      )
    
    
    CLengthR$side <- "Right"
    CLengthR$TLength <- nrow(line_AB2)
    CLengthR$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="") 
    
    # display(colorLabels(nmask_test+pos4))
    
    CLength <- rbind.data.frame(CLength, CLengthL, CLengthR)
    
    ###############################################################
    ###############################################################
    
    # Triangle lenght segment TOP (Right to Left)
    x1=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V1
    y1=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V2
    x2=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V1
    y2=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V2
    line_AB3 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos5 = array(0, dim(x))
    pos5[as.matrix(line_AB3)]  = 1
    seg_TOP <- as.data.frame(computeFeatures.shape(pos5, pic01))
    seg_TOP$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    seg_TOP$type <- "Dried"
    seg_TOP$type2 <- "Segment_T"
    
    contours3 <- rbind.data.frame(line_AB1,
                                  line_AB2,
                                  line_AB3)
    
    # Triangle lenght segment MID (pith to middle of top segment)
    mid_pos <- as.integer(nrow(line_AB3)/2)
    x1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    x2 <- line_AB3[mid_pos]$x
    y2 <- line_AB3[mid_pos]$y
    line_AB4 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos6 = array(0, dim(x))
    pos6[as.matrix(line_AB4)]  = 1
    seg_MID <- as.data.frame(computeFeatures.shape(pos6, pic01))
    seg_MID$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    seg_MID$type <- "Dried"
    seg_MID$type2 <- "Segment_M"
    
    ### Triangle Area
    contours3 <- rbind.data.frame(line_AB1,
                                  line_AB2,
                                  line_AB3
    )
    pos7 = array(0, dim(x))
    pos7[as.matrix(contours3)]  = 1
    pos7 <- fillHull(pos7)
    tri_area <-  as.data.frame(computeFeatures.shape(pos7, pic01))
    tri_area$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    tri_area$type <- "Dried"
    tri_area$type2 <- "Area"
    
    
    ### SectionB  - Section within delimited by triangular section
    pos11 = pos10*pos7
    secB_area <-  as.data.frame(computeFeatures.shape(pos11, pic01))
    secB_area$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    secB_area$type <- "Dried"
    secB_area$type2 <- "sectionB"
    
    ### Pith to top mid point
    seg_Pith_to_Top <- getBresenham2DSegment(pith[1],pith[2],mid[1],mid[2])
    pos8 = array(0, dim(x))
    pos8[as.matrix(seg_Pith_to_Top)]  = 1
    seg_PT <- as.data.frame(computeFeatures.shape(pos8, pic01))
    seg_PT$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    seg_PT$type <- "Dried"
    seg_PT$type2 <- "seg_Pith_to_Top"
    
    DW_Data6 <- rbind.data.frame(DW_Data6,
                                 seg_LEFT,
                                 seg_RIGHT,
                                 seg_TOP,
                                 seg_MID,
                                 seg_PT,
                                 tri_area,
                                 sec_area,
                                 secB_area,
                                 wedge_hat_area)
    
    
    #display(pos6)
    
    
    ####################################################################################
    
    
    ############ Automatic calculation of linear contour's lenght left and right side (dried wedges) ##############################
    left_length <- nrow(contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y) ):which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),])
    right_length <- nrow(contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)):which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),])
    LR_edges <- cbind.data.frame(left_length=left_length,right_length=right_length)
    LR_edges$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    LR_edges$type <- "Dried"
    DW_Data5 <- rbind.data.frame(DW_Data5,LR_edges)
    #####################################################################################################################
    
    
    
    ###### Autocamtic calculation of linear local curvature ##################################################################
    contours2 = ocontour(bwlabel(x))
    local_curv01 = localCurvature(x=contours2[[1]], h=35) ### option1 h=25 , option2 h=35 <-- (looks better)
    
    ### Plotting linear curvature intensity
    lc_plotDF <- as.data.frame(local_curv01)
    lc_plotDF2 <- as.data.frame(local_curv01)
    
    lc_plotDF$curvature <- NA
    lc_plotDF[(which(lc_plotDF2$contour.1==(p$left_x) & lc_plotDF2$contour.2==(p$left_y) )):(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y))),3] <- 0
    lc_plotDF[(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y) )):(which(lc_plotDF2$contour.1==(p$right_x) & lc_plotDF2$contour.2==(p$right_y))),3] <- 0
    lc_plotDF[(which(lc_plotDF2$contour.1==(p$left_x) & lc_plotDF2$contour.2==(p$left_y) )+40):(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y))-40),3] <- lc_plotDF2[(which(lc_plotDF2$contour.1==(p$left_x) & lc_plotDF2$contour.2==(p$left_y) )+40):(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y))-40),]$curvature 
    lc_plotDF[(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y) )+40):(which(lc_plotDF2$contour.1==(p$right_x) & lc_plotDF2$contour.2==(p$right_y))-40),3] <- lc_plotDF2[(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y) )+40):(which(lc_plotDF2$contour.1==(p$right_x) & lc_plotDF2$contour.2==(p$right_y))-40),]$curvature
    lc_plotDF[(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y))-10):(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y))+10),3] <- NA
    
    
    # lc_plotDF[lc_plotDF2$curvature<=(0.003),]$curvature <- 1
    # lc_plotDF[lc_plotDF2$curvature>(0.003),]$curvature <- 2
    
    
    # P <- ggplot(lc_plotDF, aes(x=contour.1, y=contour.2, colour=curvature)) +
    #   annotation_custom(rasterGrob(pic01, 
    #                                width = unit(1,"npc"),
    #                                height = unit(1,"npc")), -Inf, Inf, -Inf, Inf) +
    #   geom_path(size=0.5) +
    #   scale_y_reverse(expand = c(0, 0),limits = c(2640,0)) +
    #   scale_x_continuous(expand = c(0, 0),limits = c(0,2040))+
    #   ggtitle("") + # Local curvature 1)  LC<=0.02   2) LC>0.02
    #   scale_colour_gradient(low = "yellow", high = "red", na.value = NA)  +
    #   # scale_colour_gradientn(colours=rev(rainbow(3))) +
    #   theme_dark()
    
    left_segment <- lc_plotDF[(which(lc_plotDF2$contour.1==(p$left_x) & lc_plotDF2$contour.2==(p$left_y) )):(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y))),]$curvature
    right_segment <- lc_plotDF[(which(lc_plotDF2$contour.1==(p$pith_x) & lc_plotDF2$contour.2==(p$pith_y) )):(which(lc_plotDF2$contour.1==(p$right_x) & lc_plotDF2$contour.2==(p$right_y))),]$curvature
    
    # vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.8, "npc"), 
    #                            just=c("left","top"), 
    #                            y=0.5, x=0.1)
    # 
    # par(mfrow=c(2,2))
    # plot(rev(left_segment), type = "l", main="Local curvature pith-bark LEFT side") # profile from pith to bark
    # abline(h=0.00, col="blue",lty = 3)
    # plot(right_segment, type = "l", main="Local curvature",
    #      cex.axis=1.5, cex.main=1.5, cex.lab=1.5,
    #      xlab="Contour from pith (pixels)", ylab="Local curvature") # profile from pith to bark
    # abline(h=0.00, col="blue",lty = 3)
    # 
    # 
    # print(P, vp=vp.BottomRight)
    # par(mfrow=c(1,1))
    
    left_segmentDF <- lc_plotDF2[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y) ):which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]
    right_segmentDF <- lc_plotDF2[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)):which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]
    left_segmentDF$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    left_segmentDF$type <- "left_segment"
    right_segmentDF$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep="")
    right_segmentDF$type <- "right_segment"
    
    ### Calculating distance point-to-point ########
    length_Pith_to_check <- c()
    for (j in 1:nrow(left_segmentDF)) {
      
      pith_to_check <-  nrow(getBresenham2DSegment(pith[1],pith[2],
                                                   as.integer(left_segmentDF[j,]$contour.1),as.integer(left_segmentDF[j,]$contour.2)))
      length_Pith_to_check <- c(length_Pith_to_check,pith_to_check)
    }
    left_segmentDF$dist_pith <- length_Pith_to_check
    ##################################################################
    
    ### Calculating distance point-to-point  ########
    length_Pith_to_check <- c()
    for (j in 1:nrow(right_segmentDF)) {
      
      pith_to_check <-  nrow(getBresenham2DSegment(pith[1],pith[2],
                                                   as.integer(right_segmentDF[j,]$contour.1),as.integer(right_segmentDF[j,]$contour.2)))
      length_Pith_to_check <- c(length_Pith_to_check,pith_to_check)
    }
    right_segmentDF$dist_pith <- length_Pith_to_check
    ##################################################################
    
    
    DW_Data7 <- rbind.data.frame(DW_Data7,left_segmentDF,right_segmentDF)
    
    ##########################################################################################################################
    
    
    
    ### Display 
    # pic0_DS1_area <- resize(pic0,2040)
    # nmask_DS1_area <- resize(nmask,2040)
    
    
    ########################################
    ####  Dried wedge checking calculation
    ########################################
    
    
    # .rs.restartR() # restar R session
    
    
    pic02 <- resize(pic0[,,1], 5100)
    #pic02OR <- resize(pic0, 5100)
    # pic02[nmask==0] <- 0
    pic_mod2 <- pic02
    colorMode(pic_mod2) <- Grayscale
    #display(pic_mod2)
    
    ### Thresholding
    thr2 <- thresh(1-pic_mod2, w=22*2, h=5*2, offset=0.20)
    
    ### making the mask bigger to cover the edges
    kern = makeBrush(61*2, shape='disc')
    logo_dilate = erode(nmask, kern)
    thr2[resize(logo_dilate, 5100)==0] <- 0
    #display(thr2, all=TRUE)
    
    # ##########################################################################
    # ### Kmeans method
    # image01 <- (1-pic_mod2)
    # # display(image01)
    # image01_mat <- as.vector(image01)
    # k=7
    # set.seed(1)
    # KMC = kmeans(image01_mat, centers=k, iter.max=1000)
    # str(KMC)
    # wedgeKMC = as.numeric(KMC$cluster)
    # hist(wedgeKMC)
    # KMC$centers
    # clust_sel <- c(4,2)
    # wedgeKMC <- ifelse(wedgeKMC %in% clust_sel, 1, 0)
    # dim(wedgeKMC) = c(nrow(image01), ncol(image01))
    # # image(wedgeKMC, axes=FALSE, col=rainbow(k))
    # 
    # display(colorLabels(wedgeKMC))
    # wedgeKMC[logo_dilate==0] <- 0
    # ### Removing noise
    # nmaskf2 = fillHull(opening(wedgeKMC, makeBrush(1, shape='diamond'))) ## 3
    # dmap2 = distmap(nmaskf2)
    # # display(dmap2, all=T)
    # ### Detecting individual objects
    # nmask2 = watershed(dmap2, tolerance = 1, ext=5)
    # display(colorLabels(nmask2), all=T)
    # pic.out1 <- paintObjects(nmask2, pic01, opac = c(1, 1), col = c("red", "red"))
    # display(pic.out1, all=TRUE)
    # ###########################################################################
    
    
    ### Removing noise
    nmaskf2 = fillHull(opening(thr2, makeBrush(3, shape='diamond'))) ## 3
    dmap2 = distmap(nmaskf2)
    # display(dmap2, all=T)
    nmask2 = watershed(dmap2, tolerance = 1, ext=5)
    area_checks <- as.data.frame(computeFeatures.shape(nmask2, pic02) )
    xy_checks <- as.data.frame(computeFeatures.moment(nmask2, pic02) )
    filter_obj <- area_checks[which( (area_checks$s.area<=40) | (abs(xy_checks$m.theta)<=0.55) | xy_checks$m.eccentricity<=0.89 ) ,]
    nmask2[nmask2 %in% as.numeric(rownames(filter_obj))] <- 0
    
    
    # pic.out5 <- paintObjects( (nmask3), (pic02), opac = c(0, 1), col = c(NA, "red"), thick = TRUE, closed = TRUE)
    # display(pic.out5)
    
    dmap3 = distmap(nmask2)
    ### Detecting individual objects
    nmask3 = watershed(dmap3, tolerance = 3, ext=13)
    
    # display(colorLabels(getFrame(nmask3, 1)), all=TRUE)
    
    ### Computing objects features (Area, perimeter and moment)
    if (sum(nmask3)!=0) {
      area_checks <- as.data.frame(computeFeatures.shape(nmask3, pic02) )
      xy_checks <- as.data.frame(computeFeatures.moment(nmask3, pic02))
      filter_obj <- area_checks[which( (area_checks$s.area<=40) ) ,]
      nmask3[nmask3 %in% as.numeric(rownames(filter_obj))] <- 0
      area_checks <- as.data.frame(computeFeatures.shape(nmask3, pic02))
      xy_checks <- as.data.frame(computeFeatures.moment(nmask3, pic02))
      checks_full <- cbind.data.frame(area_checks,xy_checks)
    }
    
    if (sum(nmask3)!=0) {
      
      ### Calculating distance from pith to checks -  for each check ########
      length_Pith_to_check <- c()
      for (j in 1:nrow(checks_full)) {
        
        pith_to_check <-  nrow(getBresenham2DSegment(pith[1],pith[2],
                                                     as.integer(checks_full[j,]$m.cx),as.integer(checks_full[j,]$m.cy)))
        length_Pith_to_check <- c(length_Pith_to_check,pith_to_check)
      }
      ##################################################################
      
      checks_full$pith_to_check <- length_Pith_to_check
      checks_full$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep = "")
      DW_Data2 <- rbind.data.frame(DW_Data2,checks_full)
    }
    
    
    ##### Display
    # pic0_DS1_checks <- resize(pic0,2040)
    # nmask_DS1_checks <- resize(nmask3,2040)
    

    
    
    ### Reading DW-S2 image
    # DW_s2 <- paste(pathName_DW,wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
    pic0 <- readImage( DW_s2 ) #  "F7A_s1_mod.jpg"
    pic01 <- resize(pic0,2040)
    pic02 <- resize(pic0,2040)
    
    #####################################
    ####  Dried wedge area calculation
    #####################################
    
    ### keeping original picture
    pic <- gblur( resize(pic0[,,1],2040) , sigma = 5)
    
    ### Removing unused objects
    #rm(list=c("pic0"))
    
    ### Changing to grayscale
    colorMode(pic) <- Grayscale
    # display(pic)
    
    #### Pic preparation
    pic_mod <- pic
    pic_mod <- pic_mod*1.5
    #display(pic_mod)
    
    ### Thresholding a standard photo (black background)
    thr3 <- thresh(pic_mod, w=1019, h=1, offset=0.15)
    #display(thr3, all=T)
    
    ### Filling holes and removing small artifacts
    nmaskf = fillHull(dilate(thr3, makeBrush(3, shape='disc'))) ## 15 does not show checks
    dmap = distmap(nmaskf)
    #display(dmap, all=T, method="raster")
    
    ### Detecting independent objects by neighbours distance
    nmask = watershed(dmap, tolerance = 1, ext=50)
    # display( combine(colorLabels(getFrame(nmask, 1)),
    #                  toRGB(getFrame(pic0, 1))), all=TRUE )
    
    ### Computing objects features (Area, perimeter and location xy)
    area_wedge <- as.data.frame(computeFeatures.shape(nmask, pic01) )
    filter_obj <- area_wedge[which(area_wedge$s.area<=20000),]
    nmask[nmask %in% as.numeric(rownames(filter_obj))] <- 0
    area_wedge <- as.data.frame(computeFeatures.shape(nmask, pic01)) 
    xy_wedge <- as.data.frame(computeFeatures.moment(nmask, pic01))
    wedge_full <- cbind.data.frame(area_wedge,xy_wedge)
    wedge_full$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
    DW_Data1 <- rbind.data.frame(DW_Data1,wedge_full)
    
    
    ### Corner detection and vertical alignment
    x = nmask
    contours2 = ocontour(bwlabel(x))
    local_curv01 = localCurvature(x=contours2[[1]], h=50) ### 
    plot(c(local_curv01$curvature))
    
    findPeaks<-  function (x, m = 3){
      shape <- diff(sign(diff(x, na.pad = FALSE)))
      pks <- sapply(which(shape < 0), FUN = function(i){
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
      })
      pks <- unlist(pks)
      pks
    }
    
    valleys1 <- findPeaks(-local_curv01$curvature, m=500)
    corners1 <-  local_curv01$contour[valleys1,]
    
    ##### Display
    # pic0_DS2_area <- resize(pic0,2040)
    # nmask_DS2_area <- resize(nmask,2040)
    
    
    ### selecting pith (click wedge pith position on the plot)
    # display(pic01, all=TRUE, method="raster") #
    # text(x = 600, y = 150,
    #      label = paste("i=",i," ",wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg"," Dried",sep=""),
    #      adj = c(0,1), col = "white", cex = 2.6)
    # 
    # p <- locator(4, type = "l", pch = 3, col = "red")
    # p <- cbind.data.frame(pith_x=p$x[1],
    #                       pith_y=p$y[1],
    #                       mid_x=p$x[2],
    #                       mid_y=p$y[2],
    #                       left_x=p$x[3],
    #                       left_y=p$y[3],
    #                       right_x=p$x[4],
    #                       right_y=p$y[4]
    # )
    # p$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
    # p$type <- "Dried"
    # DW_Data3 <- rbind.data.frame(DW_Data3,p)
    
    
    ############### Automatic detection of shrinkage points##############################
    xxx <- imageData(nmask)
    xxx1 <- as.data.frame(which(nmask==1, arr.ind=TRUE))
    # left <- c(  min(xxx1$row), min( xxx1[which(xxx1$row==min(xxx1$row)),]$col )  ) #Left
    # right <- c(  max(xxx1$row), min( xxx1[which(xxx1$row==max(xxx1$row)),]$col )  ) #Right
    # pith <- c( ( xxx1[which(xxx1$col==max(xxx1$col)),]$row )[as.integer( (  length(xxx1[which(xxx1$col==max(xxx1$col)),]$row)/2  ))] , max(xxx1$col)   ) #Pith
    
    left <- corners1[1,]
    pith <- corners1[2,]
    right <- corners1[3,]
    
    mid_x <- as.integer(sqrt((left[1]-right[1])^2 + (left[2]-right[2])^2)/2+left[1])
    mid_y <- min(which(xxx[mid_x,]==1)) 
    mid <- c(mid_x, mid_y)
    p <- cbind.data.frame(pith_x=as.integer(pith[1]),
                          pith_y=as.integer(pith[2]),
                          mid_x=as.integer(mid[1]),
                          mid_y=as.integer(mid[2]),
                          left_x=as.integer(left[1]),
                          left_y=as.integer(left[2]),
                          right_x=as.integer(right[1]),
                          right_y=as.integer(right[2])
    )
    p$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    p$type <- "Dried"
    DW_Data3 <- rbind.data.frame(DW_Data3,p)
    
    # display(pic01, all=TRUE, method="raster" )
    # points(pith[1], pith[2] , col = "red", pch = 13, cex = 1.5)
    # points(left[1], left[2] , col = "red", pch = 13, cex = 1.5)
    # points(right[1], right[2] , col = "red", pch = 13, cex = 1.5)
    # points(mid[1], mid[2] , col = "red", pch = 13, cex = 1.5)
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of pith, mid, left and right positions ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg"," Dried",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    
    
    
    ####################################################################################
    
    
    
    ##### Automatic calculation of wedge hat area #######################################
    #display(nmask)
    x = nmask
    contours1 = as.data.frame(ocontour(x)[[1]])
    x1=contours1[min(which(contours1$V1==(p$left_x))),]$V1
    y1=contours1[min(which(contours1$V1==(p$left_x))),]$V2
    x2=contours1[max(which(contours1$V1==(p$right_x))) ,]$V1
    y2=contours1[max(which(contours1$V1==(p$right_x))) ,]$V2
    m=(y2-y1)/(x2-x1)
    x_range <- (x1+1):(x2-1)
    y_output <- as.integer(m*x_range - m*x1 + y1)
    line_AB <- cbind.data.frame(V1=x_range,V2=y_output)
    contours2 <- rbind.data.frame(contours1[1:min(which(contours1$V1==(p$left_x))) ,],
                                  line_AB,
                                  contours1[max(which(contours1$V1==(p$right_x))):nrow(contours1) ,])
    pos2 = array(0, dim(x))
    pos2[as.matrix(contours2)]  = 1
    pos2 <- fillHull(pos2)
    
    # display(colorLabels(nmask+pos2), all=TRUE, method="raster")
    # text(x = 300, y = 50,
    #      label = paste("Automatic detection of wedge hat ",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    # text(x = 300, y = 150,
    #      label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg"," Dried",sep=""),
    #      adj = c(0,1), col = "white", cex = 1.0)
    
    wedge_hat_area <- as.data.frame(computeFeatures.shape(pos2, pic01))
    wedge_hat_area$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    wedge_hat_area$type <- "Dried"
    DW_Data4 <- rbind.data.frame(DW_Data4,wedge_hat_area)
    
    
    driedWedgeSegmentation2 <- nmask+pos2
    
    # Triangle lenght segment LEFT (Left to Pith) 
    x1=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V1
    y1=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V2
    x2=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y2=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    line_AB1 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos3 = array(0, dim(x))
    pos3[as.matrix(line_AB1)]  = 1
    seg_LEFT <- as.data.frame(computeFeatures.shape(pos3, pic01))
    seg_LEFT$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    seg_LEFT$type <- "Dried"
    seg_LEFT$type2 <- "Segment_L"
    
    # Triangle lenght segment RIGHT (Pith to Right)
    x1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    x2=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V1
    y2=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V2
    line_AB2 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos4 = array(0, dim(x))
    pos4[as.matrix(line_AB2)]  = 1
    seg_RIGHT <- as.data.frame(computeFeatures.shape(pos4, pic01))
    seg_RIGHT$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    seg_RIGHT$type <- "Dried"
    seg_RIGHT$type2 <- "Segment_R"
    
    # Triangle lenght segment TOP (Right to Left)
    x1=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V1
    y1=contours1[which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]$V2
    x2=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V1
    y2=contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y)),]$V2
    line_AB3 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos5 = array(0, dim(x))
    pos5[as.matrix(line_AB3)]  = 1
    seg_TOP <- as.data.frame(computeFeatures.shape(pos5, pic01))
    seg_TOP$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    seg_TOP$type <- "Dried"
    seg_TOP$type2 <- "Segment_T"
    
    contours3 <- rbind.data.frame(line_AB1,
                                  line_AB2,
                                  line_AB3)
    # Triangle lenght segment MID (pith to middle of top segment)
    mid_pos <- as.integer(nrow(line_AB3)/2)
    x1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V1
    y1=contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]$V2
    x2 <- line_AB3[mid_pos]$x
    y2 <- line_AB3[mid_pos]$y
    line_AB4 <- getBresenham2DSegment(x1,y1,x2,y2)
    pos6 = array(0, dim(x))
    pos6[as.matrix(line_AB4)]  = 1
    seg_MID <- as.data.frame(computeFeatures.shape(pos6, pic01))
    seg_MID$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    seg_MID$type <- "Dried"
    seg_MID$type2 <- "Segment_M"
    
    ### Triangle Area
    contours3 <- rbind.data.frame(line_AB1,
                                  line_AB2,
                                  line_AB3
    )
    pos7 = array(0, dim(x))
    pos7[as.matrix(contours3)]  = 1
    pos7 <- fillHull(pos7)
    tri_area <-  as.data.frame(computeFeatures.shape(pos7, pic01))
    tri_area$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    tri_area$type <- "Dried"
    tri_area$type2 <- "Area"
    
    ### Pith to top mid point
    seg_Pith_to_Top <- getBresenham2DSegment(pith[1],pith[2],mid[1],mid[2])
    pos7 = array(0, dim(x))
    pos7[as.matrix(seg_Pith_to_Top)]  = 1
    seg_PT <- as.data.frame(computeFeatures.shape(pos7, pic01))
    seg_PT$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    seg_PT$type <- "Dried"
    seg_PT$type2 <- "seg_Pith_to_Top"
    
    DW_Data6 <- rbind.data.frame(DW_Data6,
                                 seg_LEFT,
                                 seg_RIGHT,
                                 seg_TOP,
                                 seg_MID,
                                 seg_PT,
                                 tri_area)
    
    #display(pos6)
    
    ####################################################################################
    
    
    
    ############ Automatic calculation of linear lenght left and right side (dried wedges) ##############################
    left_length <- nrow(contours1[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y) ):which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),])
    right_length <- nrow(contours1[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)):which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),])
    LR_edges <- cbind.data.frame(left_length=left_length,right_length=right_length)
    LR_edges$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    LR_edges$type <- "Dried"
    DW_Data5 <- rbind.data.frame(DW_Data5,LR_edges)
    #####################################################################################################################
    
    
    
    ###### Autocamtic calculation of linear local curvature ##################################################################
    contours2 = ocontour(bwlabel(x))
    local_curv01 = localCurvature(x=contours2[[1]], h=11)
    
    ### Plotting linear curvature intensity
    lc_plotDF <- as.data.frame(local_curv01)
    lc_plotDF2 <- as.data.frame(local_curv01)
    
    lc_plotDF$curvature2 <- 0
    lc_plotDF[lc_plotDF$curvature<=0.02,]$curvature2 <- 1
    lc_plotDF[lc_plotDF$curvature>0.02,]$curvature2 <- 2
    
    # P <- ggplot(lc_plotDF, aes(x=contour.1, y=contour.2, colour=as.factor(curvature2))) +
    #   geom_point() +
    #   scale_y_reverse() +
    #   ggtitle("Local curvature 1)  LC<=0.02   2) LC>0.02") +
    #   theme_dark()
    
    left_segment <- local_curv01$curvature[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y) ):which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y))]
    right_segment <- local_curv01$curvature[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)):which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y))]
    
    # vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(0.8, "npc"), 
    #                            just=c("left","top"), 
    #                            y=0.5, x=0.1)
    
    # par(mfrow=c(2,2))
    # plot(rev(left_segment), type = "l", main="Local curvature pith-bark LEFT side") # profile from pith to bark
    # abline(h=0.02, col="blue",lty = 3)
    # plot(right_segment, type = "l", main="Local curvature pith-bark RIGHT side") # profile from pith to bark
    # abline(h=0.02, col="blue",lty = 3)
    # 
    # print(P, vp=vp.BottomRight)
    # par(mfrow=c(1,1))
    
    left_segmentDF <- lc_plotDF2[which(contours1$V1==(p$left_x) & contours1$V2==(p$left_y) ):which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)),]
    right_segmentDF <- lc_plotDF2[which(contours1$V1==(p$pith_x) & contours1$V2==(p$pith_y)):which(contours1$V1==(p$right_x) & contours1$V2==(p$right_y)),]
    left_segmentDF$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    left_segmentDF$type <- "left_segment"
    right_segmentDF$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep="")
    right_segmentDF$type <- "right_segment"
    
    ### Calculating distance point-to-point ########
    length_Pith_to_check <- c()
    for (j in 1:nrow(left_segmentDF)) {
      
      pith_to_check <-  nrow(getBresenham2DSegment(pith[1],pith[2],
                                                   as.integer(left_segmentDF[j,]$contour.1),as.integer(left_segmentDF[j,]$contour.2)))
      length_Pith_to_check <- c(length_Pith_to_check,pith_to_check)
    }
    left_segmentDF$dist_pith <- length_Pith_to_check
    ##################################################################
    
    ### Calculating distance point-to-point  ########
    length_Pith_to_check <- c()
    for (j in 1:nrow(right_segmentDF)) {
      
      pith_to_check <-  nrow(getBresenham2DSegment(pith[1],pith[2],
                                                   as.integer(right_segmentDF[j,]$contour.1),as.integer(right_segmentDF[j,]$contour.2)))
      length_Pith_to_check <- c(length_Pith_to_check,pith_to_check)
    }
    right_segmentDF$dist_pith <- length_Pith_to_check
    ##################################################################
    
    DW_Data7 <- rbind.data.frame(DW_Data7,left_segmentDF,right_segmentDF)
    
    ##########################################################################################################################
    
    
    
    ########################################
    ####  Dried wedge checking calculation
    ########################################
    
    pic02 <- resize(pic0[,,1], 5100)
    #pic02OR <- resize(pic0, 5100)
    # pic02[nmask==0] <- 0
    pic_mod2 <- pic02
    colorMode(pic_mod2) <- Grayscale
    #display(pic_mod2)
    
    ### Thresholding
    thr2 <- thresh(1-pic_mod2, w=22*2, h=5*2, offset=0.20)
    
    ### making the mask bigger to cover the edges
    kern = makeBrush(61*2, shape='disc')
    logo_dilate = erode(nmask, kern)
    thr2[resize(logo_dilate, 5100)==0] <- 0
    #display(thr2, all=TRUE)
    
    # ##########################################################################
    # ### Kmeans method
    # image01 <- (1-pic_mod2)
    # # display(image01)
    # image01_mat <- as.vector(image01)
    # k=7
    # set.seed(1)
    # KMC = kmeans(image01_mat, centers=k, iter.max=1000)
    # str(KMC)
    # wedgeKMC = as.numeric(KMC$cluster)
    # hist(wedgeKMC)
    # KMC$centers
    # clust_sel <- c(4,2)
    # wedgeKMC <- ifelse(wedgeKMC %in% clust_sel, 1, 0)
    # dim(wedgeKMC) = c(nrow(image01), ncol(image01))
    # # image(wedgeKMC, axes=FALSE, col=rainbow(k))
    # 
    # display(colorLabels(wedgeKMC))
    # wedgeKMC[logo_dilate==0] <- 0
    # ### Removing noise
    # nmaskf2 = fillHull(opening(wedgeKMC, makeBrush(1, shape='diamond'))) ## 3
    # dmap2 = distmap(nmaskf2)
    # # display(dmap2, all=T)
    # ### Detecting individual objects
    # nmask2 = watershed(dmap2, tolerance = 1, ext=5)
    # display(colorLabels(nmask2), all=T)
    # pic.out1 <- paintObjects(nmask2, pic01, opac = c(1, 1), col = c("red", "red"))
    # display(pic.out1, all=TRUE)
    # ###########################################################################
    
    
    ### Removing noise
    nmaskf2 = fillHull(opening(thr2, makeBrush(3, shape='diamond'))) ## 3
    dmap2 = distmap(nmaskf2)
    # display(dmap2, all=T)
    nmask2 = watershed(dmap2, tolerance = 1, ext=5)
    area_checks <- as.data.frame(computeFeatures.shape(nmask2, pic02) )
    xy_checks <- as.data.frame(computeFeatures.moment(nmask2, pic02) )
    filter_obj <- area_checks[which( (area_checks$s.area<=40) | (abs(xy_checks$m.theta)<=0.55) | xy_checks$m.eccentricity<=0.89 ) ,]
    nmask2[nmask2 %in% as.numeric(rownames(filter_obj))] <- 0
    
    
    # pic.out5 <- paintObjects( (nmask2), (pic02), opac = c(0, 1), col = c(NA, "red"), thick = TRUE, closed = TRUE)
    # display(pic.out5)
    
    dmap3 = distmap(nmask2)
    ### Detecting individual objects
    nmask3 = watershed(dmap3, tolerance = 3, ext=13)
    
    # display(colorLabels(getFrame(nmask3, 1)), all=TRUE)
    
    ### Computing objects features (Area, perimeter and moment)
    if (sum(nmask3)!=0) {
      area_checks <- as.data.frame(computeFeatures.shape(nmask3, pic02) )
      xy_checks <- as.data.frame(computeFeatures.moment(nmask3, pic02))
      filter_obj <- area_checks[which( (area_checks$s.area<=40) ) ,]
      nmask3[nmask3 %in% as.numeric(rownames(filter_obj))] <- 0
      area_checks <- as.data.frame(computeFeatures.shape(nmask3, pic02))
      xy_checks <- as.data.frame(computeFeatures.moment(nmask3, pic02))
      checks_full <- cbind.data.frame(area_checks,xy_checks)
    }
    
    if (sum(nmask3)!=0) {
      
      ### Calculating distance from pith to checks -  for each check ########
      length_Pith_to_check <- c()
      for (j in 1:nrow(checks_full)) {
        
        pith_to_check <-  nrow(getBresenham2DSegment(pith[1],pith[2],
                                                     as.integer(checks_full[j,]$m.cx),as.integer(checks_full[j,]$m.cy)))
        length_Pith_to_check <- c(length_Pith_to_check,pith_to_check)
      }
      ##################################################################
      
      checks_full$pith_to_check <- length_Pith_to_check
      checks_full$treeID <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
      DW_Data2 <- rbind.data.frame(DW_Data2,checks_full)
    }
    
    
    
    ##### Display
    # pic0_DS2_checks <- resize(pic0,2040)
    # nmask_DS2_checks <- resize(nmask3,2040)
    
    
    
    # pic.out1 <- paintObjects(nmask_GS1_area, pic0_GS1_area, opac = c(1, 1), col = c("red", NA), thick = TRUE, closed = TRUE)
    # pic.out2 <- paintObjects(nmask_DS1_area, pic0_DS1_area, opac = c(1, 1), col = c("red", NA), thick = TRUE, closed = TRUE)
    # pic.out3 <- paintObjects(nmask_DS2_area, pic0_DS2_area, opac = c(1, 1), col = c("red", NA), thick = TRUE, closed = TRUE)
    # pic.out4 <- paintObjects(nmask_DS1_checks, pic0_DS1_checks, opac = c(1, 1), col = c("red", "red"), thick = TRUE, closed = TRUE)
    # pic.out5 <- paintObjects(nmask_DS2_checks, pic0_DS2_checks, opac = c(1, 1), col = c("red", "red"), thick = TRUE, closed = TRUE)
    # 
    # ### Overlaping green wedge and dried wedge mask
    # ### diff for translation
    # Diff_x <- DW_Data1[which(DW_Data1$treeID==filelist_GW[i]),]$m.cx-GW_Data[which(GW_Data$treeID==filelist_GW[i]),]$m.cx
    # Diff_y <- DW_Data1[which(DW_Data1$treeID==filelist_GW[i]),]$m.cy-GW_Data[which(GW_Data$treeID==filelist_GW[i]),]$m.cy
    # ### Diff for rotation
    # A_diff <- DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Dried"),]$pith_x - DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Dried"),]$mid_x
    # B_diff <- DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Dried"),]$pith_y - DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Dried"),]$mid_y
    # angle1 <- tan(A_diff/B_diff)*180/pi
    # A_diff_GW <- DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Green"),]$pith_x - DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Green"),]$mid_x
    # B_diff_GW <- DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Green"),]$pith_y - DW_Data3[which(DW_Data3$treeID==filelist_GW[i] & DW_Data3$type=="Green"),]$mid_y
    # angle2 <- tan(A_diff_GW/B_diff_GW)*180/pi
    # img_translate = translate(nmask_DS1_area, c(-Diff_x,-Diff_y), bg.col = "black")
    # img_rotate_dried = EBImage::rotate(img_translate, angle1, output.dim = c(nrow(img_translate), ncol(img_translate)), bg.col = "black")
    # img_rotate_green = EBImage::rotate(pic0_GS1_area, angle2, output.dim = c(nrow(img_translate), ncol(img_translate)), bg.col = "black")
    # pic.out6 = paintObjects(img_rotate_dried, toRGB(img_rotate_green), col=c("red", "yellow"), opac=c(1, 0.3), thick=TRUE)
    # 
    # 
    # blackMask <- nmask_GS1_area
    # blackMask[blackMask==1] <- 0
    # 
    # comb1 <- combine(
    #   pic0_GS1_area,
    #   colorLabels(greenWedgeSegmentation),
    #   pic.out6,
    # 
    #   pic0_DS1_area,
    #   colorLabels(driedWedgeSegmentation1+nmask_DS1_checks),
    #   toRGB(blackMask),
    # 
    #   pic0_DS2_area,
    #   colorLabels(driedWedgeSegmentation2+nmask_DS2_checks),
    #   toRGB(blackMask)
    #   )
    # 
    # 
    # 
    # GreenA1_hat <- DW_Data4[DW_Data4$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data4$type=="Green",]$s.area
    # DriedA1_hat <- DW_Data4[DW_Data4$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data4$type=="Dried",]$s.area
    # DriedA2_hat <- DW_Data4[DW_Data4$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data4$type=="Dried",]$s.area
    # 
    # GreenA1 <- GW_Data[GW_Data$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),]$s.area - GreenA1_hat
    # DriedA1 <- DW_Data1[DW_Data1$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),]$s.area - DriedA1_hat
    # DriedA2 <- DW_Data1[DW_Data1$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = ""),]$s.area - DriedA2_hat
    # 
    # GreenA1_tri <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green"  & DW_Data6$type2=="Area",]$s.area
    # DriedA1_tri <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Area",]$s.area
    # DriedA2_tri <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Area",]$s.area
    # 
    # GreenA1_tri_L <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green"  & DW_Data6$type2=="Segment_L",]$s.area
    # GreenA1_tri_R <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green" & DW_Data6$type2=="Segment_R",]$s.area
    # GreenA1_tri_T <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green" & DW_Data6$type2=="Segment_T",]$s.area
    # GreenA1_tri_M <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green" & DW_Data6$type2=="Segment_M",]$s.area
    # 
    # 
    # DriedA1_tri_L <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried"  & DW_Data6$type2=="Segment_L",]$s.area
    # DriedA1_tri_R <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_R",]$s.area
    # DriedA1_tri_T <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_T",]$s.area
    # DriedA1_tri_M <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_M",]$s.area
    # 
    # DriedA2_tri_L <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried"  & DW_Data6$type2=="Segment_L",]$s.area
    # DriedA2_tri_R <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_R",]$s.area
    # DriedA2_tri_T <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_T",]$s.area
    # DriedA2_tri_M <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_M",]$s.area
    # 
    # 
    # DriedA1_checks <- nrow(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),])
    # DriedA1_area <- sum(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),]$s.area)
    # DriedA2_checks <- nrow(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = ""),])
    # DriedA2_area <- sum(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = ""),]$s.area)
    # 
    # SN_s1 <- round( (GreenA1_tri-DriedA1_tri)/GreenA1_tri*100, 1)
    # SG_s1 <- round( (GreenA1-DriedA1)/GreenA1*100, 1)
    # #SR_s1 <- round( ((GreenA1_tri_R-DriedA1_tri_R)/GreenA1_tri_R + (GreenA1_tri_L-DriedA1_tri_L)/GreenA1_tri_L)/2  *100, 1)
    # SR_s1 <- round( (GreenA1_tri_M-DriedA1_tri_M)/GreenA1_tri_M  *100, 1)
    # 
    # ST_s1 <- round( (GreenA1_tri_T-DriedA1_tri_T)/GreenA1_tri_T*100, 1)
    # AN_s1 <- round(SR_s1/ST_s1,2)
    # CO_s1 <- round(SG_s1-SN_s1,2)
    # 
    # SN_s2 <- round( (GreenA1_tri-DriedA2_tri)/GreenA1_tri*100, 1)
    # SG_s2 <- round( (GreenA1-DriedA2)/GreenA1*100, 1)
    # #SR_s2 <- round( ((GreenA1_tri_R-DriedA2_tri_R)/GreenA1_tri_R + (GreenA1_tri_L-DriedA2_tri_L)/GreenA1_tri_L)/2  *100, 1)
    # SR_s2 <- round( (GreenA1_tri_M-DriedA2_tri_M)/GreenA1_tri_M  *100, 1)
    # ST_s2 <- round( (GreenA1_tri_T-DriedA2_tri_T)/GreenA1_tri_T*100, 1)
    # AN_s2 <- round(SR_s2/ST_s2,2)
    # CO_s2 <- round(SG_s2-SN_s2,2)
    # 
    # score_s1 <- scoring[scoring$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1",sep = ""),]$Score_wedgeA
    # 
    # 
    # img_Tile <- tile(comb1,3)
    # 
    # display(img_Tile, method = "raster") #
    # # Wedges IDs
    # text(x = 2700, y = 100, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Green - ",
    #                                       as.character(wedgeDF_ID2[wedgeDF_ID2$treeID2==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],sep = ""),]$Treatment),sep = ""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 2700, y = 2700, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Dried Side 1",sep = ""), adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 2700, y = 5350, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Dried Side 2",sep = ""), adj = c(0,1), col = "white", cex = 0.6)
    # 
    # text(x = 700, y = 100, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Green",sep = ""), adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 4700, y = 100, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Green",sep = ""), adj = c(0,1), col = "white", cex = 0.6)
    # 
    # text(x = 700, y = 2700, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Dried Side 1",sep = ""), adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 700, y = 5350, label = paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3]," Dried Side 2",sep = ""), adj = c(0,1), col = "white", cex = 0.6)
    # 
    # 
    # text(x = 700, y = 200, label = "Original photo", adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 2700, y = 200, label = "Image segmentation", adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 4700, y = 200, label = "Green-dried overlap", adj = c(0,1), col = "white", cex = 0.6)
    # 
    # text(x = 700, y = 2800, label = "Original photo", adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 2700, y = 2800, label = "Image segmentation", adj = c(0,1), col = "white", cex = 0.6)
    # 
    # text(x = 700, y = 5450, label = "Original photo", adj = c(0,1), col = "white", cex = 0.6)
    # text(x = 2700, y = 5450, label = "Image segmentation", adj = c(0,1), col = "white", cex = 0.6)
    # 
    # 
    # # Gross shrinkage
    # text(x = 4400, y = 2850,
    #      label = paste("Shrinkage side 1 :",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # 
    # # Gross shrinkage
    # text(x = 4400, y = 3100,
    #      label = paste("Gross = ", SG_s1," %",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Net shrinkage
    # text(x = 4400, y = 3250,
    #      label = paste("Net = ", SN_s1," %",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Radial shrinkage
    # text(x = 4400, y = 3400,
    #      label = paste("Radial = ", SR_s1," %",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Tangential shrinkage
    # text(x = 4400, y = 3550,
    #      label = paste("Tangential = ", ST_s1," %",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # #  Anisotropy
    # text(x = 4400, y = 3700,
    #      label = paste("Anisotropy = ", AN_s1," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # COllapse
    # text(x = 4400, y = 3850,
    #      label = paste("Collapse = ", CO_s1," %",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # 
    # #
    # text(x = 4400, y = 4150,
    #      label = paste("Checking side 1 :",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # 
    # # Number of checks
    # text(x = 4400, y = 4400,
    #      label = paste("Number = ", DriedA1_checks," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Total Area of checks
    # text(x = 4400, y = 4550,
    #      label = paste("Area = ", DriedA1_area," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Total Area per Wedge Area
    # text(x = 4400, y = 4700,
    #      label = paste("Rel. area = ", round( 100000*DriedA1_area/GreenA1 , 1)," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Mean check size
    # text(x = 4400, y = 4850,
    #      label = paste("Size = ", ifelse(DriedA1_checks>0,round((DriedA1_area)/DriedA1_checks, 1),"NA") , sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Subjective Score
    # text(x = 4400, y = 5000,
    #      label = paste("Score = ", score_s1," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # 
    # 
    # # ### Side 2
    # #
    # # #
    # # text(x = 4400, y = 5450,
    # #      label = paste("Shrinkage side 2 : ",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # #
    # # # Gross shrinkage
    # # text(x = 4400, y = 5700,
    # #      label = paste("Gross = ", SG_s2," %",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # # # Net shrinkage
    # # text(x = 4400, y = 5850,
    # #      label = paste("Net = ", SN_s2," %",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # # # Radial shrinkage
    # # text(x = 4400, y = 6000,
    # #      label = paste("Radial = ", SR_s2," %",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # # # Tangential shrinkage
    # # text(x = 4400, y = 6150,
    # #      label = paste("Tangential = ", ST_s2," %",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # # #  Anisotropy
    # # text(x = 4400, y = 6300,
    # #      label = paste("Anisotropy = ", AN_s2," ",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # # # COllapse
    # # text(x = 4400, y = 6450,
    # #      label = paste("Collapse = ", CO_s2," %",sep=""),
    # #      adj = c(0,1), col = "white", cex = 0.6)
    # 
    # #
    # text(x = 4400, y = 6750,
    #      label = paste("Checking side 2 :",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # 
    # # Number of checks
    # text(x = 4400, y = 7000,
    #      label = paste("Number = ", DriedA2_checks," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Total Area of checks
    # text(x = 4400, y = 7150,
    #      label = paste("Area = ", DriedA2_area," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Total Area per Wedge Area
    # text(x = 4400, y = 7300,
    #      label = paste("Rel. area = ",round( 100000*DriedA2_area/GreenA1 , 1)," ",sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Mean check size
    # text(x = 4400, y = 7450,
    #      label = paste("Size = ", ifelse(DriedA2_checks>0,round((DriedA2_area)/DriedA2_checks, 1),"NA") , sep=""),
    #      adj = c(0,1), col = "white", cex = 0.6)
    # # Subjective Score
    
    
    
    
    
  }
  
  
  
}


dev.off()




















#### Data Analysis ####
GW_Data <- read.csv("GW_Data.csv", stringsAsFactors = FALSE)
DW_Data1 <- read.csv("DW_Data1.csv", stringsAsFactors = FALSE)
DW_Data2 <- read.csv("DW_Data2.csv", stringsAsFactors = FALSE)
DW_Data3 <- read.csv("DW_Data3.csv", stringsAsFactors = FALSE)
DW_Data4 <- read.csv("DW_Data4.csv", stringsAsFactors = FALSE)
DW_Data5 <- read.csv("DW_Data5.csv", stringsAsFactors = FALSE)
DW_Data6 <- read.csv("DW_Data6.csv", stringsAsFactors = FALSE)
DW_Data7 <- read.csv("DW_Data7.csv", stringsAsFactors = FALSE)
scoring <- read.csv("Scoring.csv", stringsAsFactors = FALSE)
scoring$treeID<- paste(scoring$site2,"_",scoring$Tree,"_",scoring$wedge,"_",scoring$side,sep="")
scoring$treeID2<- paste(scoring$site2,"_",scoring$Tree,"_",scoring$wedge,sep="")

#### 1- Shrinkage 
full_DF <- c()
for (i in 1:nrow(GW_Data)) {
  
  wedge_ID <- (strsplit(GW_Data$treeID[i], "_"))[[1]][1:3]
  
  DW_s1 <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s1.jpg",sep = "")
  DW_s2 <- paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_","s2.jpg",sep = "")
  
  GreenA1_hat <- DW_Data4[DW_Data4$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data4$type=="Green",]$s.area
  DriedA1_hat <- DW_Data4[DW_Data4$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data4$type=="Dried",]$s.area
  DriedA2_hat <- DW_Data4[DW_Data4$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data4$type=="Dried",]$s.area
  
  GreenA1 <- GW_Data[GW_Data$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),]$s.area - GreenA1_hat
  DriedA1 <- DW_Data1[DW_Data1$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),]$s.area - DriedA1_hat
  DriedA2 <- DW_Data1[DW_Data1$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = ""),]$s.area - DriedA2_hat
  
  GreenA1_tri <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green"  & DW_Data6$type2=="Area",]$s.area
  DriedA1_tri <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Area",]$s.area
  DriedA2_tri <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Area",]$s.area
  
  GreenA1_tri_L <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green"  & DW_Data6$type2=="Segment_L",]$s.area
  GreenA1_tri_R <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green" & DW_Data6$type2=="Segment_R",]$s.area
  GreenA1_tri_T <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green" & DW_Data6$type2=="Segment_T",]$s.area
  GreenA1_tri_M <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Green" & DW_Data6$type2=="Segment_M",]$s.area
  
  
  DriedA1_tri_L <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried"  & DW_Data6$type2=="Segment_L",]$s.area
  DriedA1_tri_R <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_R",]$s.area
  DriedA1_tri_T <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_T",]$s.area
  DriedA1_tri_M <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_M",]$s.area
  
  DriedA2_tri_L <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried"  & DW_Data6$type2=="Segment_L",]$s.area
  DriedA2_tri_R <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_R",]$s.area
  DriedA2_tri_T <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_T",]$s.area
  DriedA2_tri_M <- DW_Data6[DW_Data6$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = "") & DW_Data6$type=="Dried" & DW_Data6$type2=="Segment_M",]$s.area
  
  
  DriedA1_checks <- nrow(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),])
  DriedA1_area <- sum(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1.jpg",sep = ""),]$s.area)
  DriedA2_checks <- nrow(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = ""),])
  DriedA2_area <- sum(DW_Data2[DW_Data2$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s2.jpg",sep = ""),]$s.area)
  
  SN_s1 <- round( (GreenA1_tri-DriedA1_tri)/GreenA1_tri*100, 1)
  SG_s1 <- round( (GreenA1-DriedA1)/GreenA1*100, 1)
  SR_s1 <- round( ((GreenA1_tri_R-DriedA1_tri_R)/GreenA1_tri_R + (GreenA1_tri_L-DriedA1_tri_L)/GreenA1_tri_L)/2  *100, 1)
  SR_s1M <- round( (GreenA1_tri_M-DriedA1_tri_M)/GreenA1_tri_M  *100, 1)
  
  ST_s1 <- round( (GreenA1_tri_T-DriedA1_tri_T)/GreenA1_tri_T*100, 1)
  AN_s1 <- round(SR_s1/ST_s1,2)
  CO_s1 <- round(SG_s1-SN_s1,2)
  
  SN_s2 <- round( (GreenA1_tri-DriedA2_tri)/GreenA1_tri*100, 1)
  SG_s2 <- round( (GreenA1-DriedA2)/GreenA1*100, 1)
  SR_s2 <- round( ((GreenA1_tri_R-DriedA2_tri_R)/GreenA1_tri_R + (GreenA1_tri_L-DriedA2_tri_L)/GreenA1_tri_L)/2  *100, 1)
  SR_s2M <- round( (GreenA1_tri_M-DriedA2_tri_M)/GreenA1_tri_M  *100, 1)
  ST_s2 <- round( (GreenA1_tri_T-DriedA2_tri_T)/GreenA1_tri_T*100, 1)
  AN_s2 <- round(SR_s2/ST_s2,2)
  CO_s2 <- round(SG_s2-SN_s2,2)
  
  score_s1 <- scoring[scoring$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1",sep = ""),]$Score_wedgeA
  treatment <- scoring[scoring$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1",sep = ""),]$Treatment
  site <- scoring[scoring$treeID==paste(wedge_ID[1],"_",wedge_ID[2],"_",wedge_ID[3],"_s1",sep = ""),]$Site
  
  
  Shrinkage_ALL <- cbind.data.frame(
    # S1
    Radial_S1=SR_s1,
    Radial_S1M=SR_s1M,
    Tangential_S1=ST_s1,
    Section_Dried_S1=DriedA1,
    Gross_S1= SG_s1,
    Net_S1= SN_s1,
    Collapse_S1= CO_s1,
    Score_S1= score_s1,
    N_check_S1=DriedA1_checks,
    Area_check_S1=DriedA1_area,
    size_check_S1=ifelse(DriedA1_checks>0,DriedA1_area/DriedA1_checks,0),
    R_Area_check_S1= 100000*DriedA1_area/GreenA1,
    # S2
    Radial_S2=SR_s2,
    Radial_S2M=SR_s2M,
    Tangential_S2=ST_s2,
    Section_Dried_S2=DriedA2,
    Gross_S2= SG_s2,
    Net_S2= SN_s2,
    Collapse_S2= CO_s2,
    N_check_S2=DriedA2_checks,
    Area_check_S2=DriedA2_area,
    size_check_S2= ifelse(DriedA2_checks>0,DriedA2_area/DriedA2_checks,0),
    R_Area_check_S2= 100000*DriedA2_area/GreenA1,
    treeID=GW_Data$treeID[i],
    treatment=treatment,
    site=site
  )
  
  full_DF <- rbind.data.frame(full_DF,Shrinkage_ALL)
  
}


### Non biological meaning (deletion)
full_DF[is.na(full_DF)] <- 0  
full_DF[full_DF<0] <- 0
full_DF$Score_S1 <- ifelse(full_DF$Score_S1 %in% c(2,3), 2,
                           ifelse(full_DF$Score_S1 %in% c(4,5,6), 3, 1))
full_DF$Score_S1 <- as.factor(full_DF$Score_S1)

### Correlation matrix Shrinkage  How different is S1 from S2
ggpairs(full_DF[,c(
  "Gross_S1",
  "Gross_S2",
  "Net_S1",
  "Net_S2",
  "Radial_S1",
  "Radial_S2",
  "Radial_S1M",
  "Radial_S2M",
  "Tangential_S1",
  "Tangential_S2",
  "Collapse_S1",
  "Collapse_S2"
)])
### Intertrait correlations S1
ggpairs(full_DF[,c(
  "Gross_S1",
  "Net_S1",
  "Radial_S1",
  "Radial_S1M",
  "Tangential_S1",
  "Collapse_S1",
  "treatment",
  "site"
  
)])

### Correlation matrix Checking How different is S1 from S2
ggpairs(full_DF[,c(
  "N_check_S1",
  "N_check_S2",
  "Area_check_S1",
  "Area_check_S2",
  "R_Area_check_S1",
  "R_Area_check_S2",
  "size_check_S1",
  "size_check_S2"
)])


### Intertrait correlations S1
ggpairs(full_DF[,c(
  "N_check_S1",
  "Area_check_S1",
  # "R_Area_check_S1",
  # "size_check_S1",
  # "Gross_S1",
  # "Tangential_S1",
  "Collapse_S1",
  # "Radial_S1",
  "Score_S1"
  #"treatment"
  #"site"
  
)])



ggpairs(full_DF[,c(8,9,10,11,7)])
ggpairs(full_DF[,c(5,4,1,2,3,6,7)])

ggpairs(full_DF[,c(2:15)])




## 1. Fit extended forest model - Regression

data <- full_DF[full_DF$Score_S1 %in% c(1,2,3,4,5,6),]
data$Score_S1 <- as.numeric(data$Score_S1)

forest_var<-colnames(data[c("N_check_S1", 
                            "Area_check_S1",    
                            "size_check_S1",   
                            "R_Area_check_S1"
)])

form1 <- formula(paste0("Score_S1 ~ ", paste(forest_var, collapse = " + ")))

set.seed(1807)
CHECKS_eRF_1 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 1000, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)

### Model summary
print(CHECKS_eRF_1)

## 2. Plot the variable importance 
extendedForest::varImpPlot(CHECKS_eRF_1, type = 1,
                           main = paste0("Score ~ (R2 = ", round(CHECKS_eRF_1$rsq[CHECKS_eRF_1$ntree], 3) * 100, " %)" )
)







## 3. Plot the partial dependancy plot 
par(mfrow = c(2,2))
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "N_check_S1", "1", main = "Partial Dependency Plot - Score")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "size_check_S1", "1", main = "Partial Dependency Plot - Score")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "1", main = "Partial Dependency Plot - Score")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "R_Area_check_S1", "1", main = "Partial Dependency Plot - Score")





## 1. Fit extended forest model - Classification

data <- full_DF[full_DF$Score_S1 %in% c(1,2,3,4,5,6),] ## Remove score 1 ??
data$Score_S1 <- as.factor(data$Score_S1)

forest_var<-colnames(data[c("N_check_S1", 
                            "Area_check_S1",    
                            "size_check_S1",   
                            "R_Area_check_S1"
)])

form1 <- formula(paste0("Score_S1 ~ ", paste(forest_var, collapse = " + ")))


set.seed(1807)
CHECKS_eRF_1 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             #maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)

### Model summary
print(CHECKS_eRF_1)

## 2. Plot the variable importance 
par(mfrow = c(1,1))
extendedForest::varImpPlot(CHECKS_eRF_1, type = 1,
                           main = paste0("Importance variable Score 1-6")
)


## 3. Plot the partial dependancy plot 
par(mfrow = c(6,1))
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "1", main = "Partial dependence plot - Score=1")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "2", main = "Partial dependence plot - Score=2")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "3", main = "Partial dependence plot - Score=3")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "4", main = "Partial dependence plot - Score=4")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "5", main = "Partial dependence plot - Score=5")
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "Area_check_S1", "6", main = "Partial dependence plot - Score=6")




#################################################################################
## 1. Fit extended forest model - Classification as binary variable per class

data <- full_DF[full_DF$Score_S1 %in% c(1,2,3,4,5,6),] ## Remove score 1 ??
data$Score_S1 <- as.factor(data$Score_S1)
data$Score1 <- as.factor(ifelse(data$Score_S1==1,1,2)) #
data$Score2 <- as.factor(ifelse(data$Score_S1==2,1,2)) #
data$Score3 <- as.factor(ifelse(data$Score_S1==3,1,2)) #
data$Score4 <- as.factor(ifelse(data$Score_S1==4,1,2)) #
data$Score5 <- as.factor(ifelse(data$Score_S1==5,1,2)) #
data$Score6 <- as.factor(ifelse(data$Score_S1==6,1,2)) #

forest_var<-colnames(data[c("N_check_S1", 
                            "Area_check_S1",    
                            "size_check_S1",   
                            "R_Area_check_S1"
)])

set.seed(1807)
form1 <- formula(paste0("Score1 ~ ", paste(forest_var, collapse = " + ")))
CHECKS_eRF_1 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)
form1 <- formula(paste0("Score2 ~ ", paste(forest_var, collapse = " + ")))
CHECKS_eRF_2 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)
form1 <- formula(paste0("Score3 ~ ", paste(forest_var, collapse = " + ")))
CHECKS_eRF_3 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)
form1 <- formula(paste0("Score4 ~ ", paste(forest_var, collapse = " + ")))
CHECKS_eRF_4 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)
form1 <- formula(paste0("Score5 ~ ", paste(forest_var, collapse = " + ")))
CHECKS_eRF_5 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)
form1 <- formula(paste0("Score6 ~ ", paste(forest_var, collapse = " + ")))
CHECKS_eRF_6 <- extendedForest::randomForest(form1, data = data,
                                             na.action=na.omit,
                                             maxLevel = floor(log2(nrow(data) * 0.368/2)),
                                             ntree = 500, corr.threshold = 0.5, importance = T,
                                             keep.inbag = T)


### Model summary
print(CHECKS_eRF_1)
print(CHECKS_eRF_2)
print(CHECKS_eRF_3)
print(CHECKS_eRF_4)
print(CHECKS_eRF_5)
print(CHECKS_eRF_6)

## 2. Plot the variable importance 
par(mfrow = c(1,1))
extendedForest::varImpPlot(CHECKS_eRF_1, type = 1, "1"
                           main = paste0("Importance variable Score 1-6")
)



## 3. Plot the partial dependancy plot 
par(mfrow = c(6,1))
extendedForest::partialPlot(CHECKS_eRF_1, full_DF, x.var = "N_check_S1", "1", main = "Partial dependence plot - Score=1")
extendedForest::partialPlot(CHECKS_eRF_2, full_DF, x.var = "N_check_S1", "1", main = "Partial dependence plot - Score=2")
extendedForest::partialPlot(CHECKS_eRF_3, full_DF, x.var = "N_check_S1", "1", main = "Partial dependence plot - Score=3")
extendedForest::partialPlot(CHECKS_eRF_4, full_DF, x.var = "N_check_S1", "1", main = "Partial dependence plot - Score=4")
extendedForest::partialPlot(CHECKS_eRF_5, full_DF, x.var = "N_check_S1", "1", main = "Partial dependence plot - Score=5")
extendedForest::partialPlot(CHECKS_eRF_6, full_DF, x.var = "N_check_S1", "1", main = "Partial dependence plot - Score=6")





