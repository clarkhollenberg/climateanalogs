#create state tables by biome
###############################################
perc_compare_df<- read.csv("Tables/PA_perc_comparison_master.csv")
biome_table<- read.csv("Tables/Biome_area_base.csv")
biome_2C_flowMatrix<-read.csv("TransitionMat/biome_2C_flow_matrix.csv") %>% as.matrix() %>%transitionMatrix()
biome_4C_flowMatrix<-read.csv("TransitionMat/biome_4C_flow_matrix.csv") %>% as.matrix() %>%transitionMatrix()
PA_bio_2C_flowMatrix<-read.csv("TransitionMat/PA_bio_2C_flowMatrix.csv") %>% as.matrix() %>%transitionMatrix()
PA_bio_4C_flowMatrix<-read.csv("TransitionMat/PA_bio_4C_flowMatrix.csv") %>% as.matrix() %>%transitionMatrix()
tot_area<-tapply(area(biorast_2C_mapped), biorast_2C_mapped[], sum) #total terrestrial land area
tot_area<-tot_area[-c(9, 14, 15, 17)] # remove insuff. data area from total (note this is 16 not 17 since )
tot_area<-sum(tot_area)
PA_area_rast<-mask(biorast_2C_mapped, PA_bin)
tot_area_PA <-tapply(area(PA_area_rast), PA_area_rast[], sum)  #total protected land area
tot_area_PA<-tot_area_PA[-c(9, 14, 15, 17)] # remove insuff. data area from total
tot_area_PA<-sum(tot_area_PA)

state_table_biome<-data.frame("BIOME_ID"=biome_table$BIOME_ID, "BIOME_NAME"=biome_table$BIOME_NAME, 
                              "perc_total_now"=biome_table$Area_now/tot_area*100, "perc_PA_now"=biome_table$PA_now/tot_area_PA*100,
                              "perc_total_2C"=biome_table$Area_2C/tot_area*100, "perc_PA_2C"=biome_table$PA_2C/tot_area_PA*100,
                              "perc_total_4C"=biome_table$Area_4C/tot_area*100, "perc_PA_4C"=biome_table$PA_4C/tot_area_PA*100)
state_table_biome<- state_table_biome[-c(9, 14, 15, 17), ] #remove r&i, water biomes, and insuff. data
write.csv(state_table_biome, "Tables/state_table_biome.csv", row.names=F)

trans_table_biome<-data.frame("BIOME_ID"=biome_table$BIOME_ID[1:15], "BIOME_NAME"=biome_table$BIOME_NAME[1:15], 
                              "perc_trans_2C"=(1-diag(biome_2C_flowMatrix)[1:15])*100, "perc_trans_2C_PA"=(1-diag(PA_bio_2C_flowMatrix)[1:15])*100,
                              "perc_trans_4C"=(1-diag(biome_4C_flowMatrix)[1:15])*100, "perc_trans_4C_PA"=(1-diag(PA_bio_4C_flowMatrix)[1:15])*100)
trans_table_biome<- trans_table_biome[-c(9, 14, 15), ] #remove r&i, water biomes
write.csv(trans_table_biome, "Tables/trans_table_biome.csv", row.names=F)

#by ecoregion
#########################################
state_table_eco<-data.frame("ECO_ID"=perc_compare_df$ECO_ID, "ECO_NAME"=perc_compare_df$econame, "BIOME_ID"=perc_compare_df$BIOME_ID,
                              "perc_total_now"=signif(perc_compare_df$tot_area_current/tot_area*100, digits=2), "perc_PA_now"=signif(perc_compare_df$PA_area_current/tot_area_PA*100, digits=2),
                              "perc_total_2C"=signif(perc_compare_df$tot_area_2C/tot_area*100, digits=2), "perc_PA_2C"=signif(perc_compare_df$PA_area_2C/tot_area_PA*100, digits=2),
                              "perc_total_4C"=signif(perc_compare_df$tot_area_4C/tot_area*100, digits=2), "perc_PA_4C"=signif(perc_compare_df$PA_area_4C/tot_area_PA*100, digits=2))
state_table_eco<-state_table_eco[order(state_table_eco$ECO_ID), ]
write.csv(state_table_eco, "Tables/state_table_eco.csv", row.names=F)

now_2C_flowMatrix<-read.csv("TransitionMat/now-2C_flow_matrix.csv") %>% cleanInput()%>% as.matrix() %>% transitionMatrix()
now_4C_flowMatrix<-read.csv("TransitionMat/now-4C_flow_matrix.csv") %>% cleanInput() %>% as.matrix() %>% transitionMatrix()
PA_eco_2C_flowMatrix<-read.csv("TransitionMat/PA_eco_2C_flowMatrix.csv") %>% cleanInput() %>% as.matrix() %>% transitionMatrix()
PA_eco_4C_flowMatrix<-read.csv("TransitionMat/PA_eco_4C_flowMatrix.csv") %>% cleanInput() %>% as.matrix() %>% transitionMatrix()

#all this merging is the ensure that all ecoregions are preserved with the correct IDs and these are set to NA
#there is a distinction between a 0% transition (indicating that no ecoregion cells shifted) and a NA transition (indicating that the initial area = 0)
trans_table_eco<-data.frame("ECO_ID"=LUT_plus$ECO_ID[c(1:848)], "ECO_NAME"=LUT_plus$econame[c(1:848)], "BIOME_ID"=LUT_plus$BIOME_ID[c(1:848)])
trans_table_1<-data.frame("ECO_NAME"=rownames(now_2C_flowMatrix), 
                          "perc_trans_2C"=(1-diag(now_2C_flowMatrix))*100)
trans_table_2<-data.frame("ECO_NAME"=rownames(PA_eco_2C_flowMatrix),
                          "perc_trans_2C_PA"=(1-diag(PA_eco_2C_flowMatrix))*100)
trans_table_3<-data.frame("ECO_NAME"=rownames(now_4C_flowMatrix),
                              "perc_trans_4C"=(1-diag(now_4C_flowMatrix))*100)
trans_table_4<-data.frame("ECO_NAME"=rownames(PA_eco_4C_flowMatrix),
                          "perc_trans_4C_PA"=(1-diag(PA_eco_4C_flowMatrix))*100)
trans_table_eco<-merge(trans_table_eco, trans_table_1, by="ECO_NAME", all=T) %>% merge(., trans_table_2, by="ECO_NAME", all=T) %>%
             merge(., trans_table_3, by="ECO_NAME", all=T) %>% merge(., trans_table_4, by="ECO_NAME", all=T)
trans_table_eco<-trans_table_eco[order(trans_table_eco$ECO_ID), ]
write.csv(trans_table_eco, "Tables/trans_table_eco.csv", row.names=F)
