library(RMariaDB)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggsoccer)
library(dplyr)
library(stringr)
library(rpart)
library(rpart.plot)
library(scales)
library(randomForest)
library(plotly)
library(factoextra)
library(purrr)
library(rsconnect)

load("pass_data.RData")

players <- readRDS("players.rds")
players <- players %>% distinct(PLAYER_WYID, .keep_all = TRUE)
teams <- readRDS("teams.rds")
matches <- readRDS("matches.rds")
MATCHADVANCEDSTATS <- readRDS("advanced.rds")
#common <- readRDS("common.rds")
passes <- readRDS("passes.rds")
shots <- readRDS("shots.rds")


########################################################################################################################
#SETUP
########################################################################################################################

#common_indeværende <- common %>% filter(SEASON_WYID==191611)
#kampe_indeværende <- common_indeværende %>% distinct(MATCH_WYID, .keep_all = TRUE) #107 kampe i tilgængelig data.
#kampe_indeværende <- kampe_indeværende[, c("MATCH_WYID", "SEASON_WYID","EVENT_WYID", "MATCHPERIOD", "MINUTE","TEAM_WYID","PLAYER_WYID","LOCATIONX","LOCATIONY") ]
#107 kampe i 25/26 sæson so far

#MATCHADVANCEDSTATS <- dbGetQuery(con, "SELECT * FROM superliga2.WYSCOUT_MATCHADVANCEDSTATS_GENERAL;")
MATCHADVANCEDSTATS <- left_join(kampe_indeværende[,c("MATCH_WYID", "SEASON_WYID")], MATCHADVANCEDSTATS[, names(MATCHADVANCEDSTATS) != "SHOTSWIDE"], by="MATCH_WYID")
names(MATCHADVANCEDSTATS)[names(MATCHADVANCEDSTATS) == "AVGDISTANCE"] <- "AVGDISTANCE_TO_GOAL_SHOTS"

#joiner med passes. først via match_id og dernæst på event_id. 
#passes_indeværende <- left_join(kampe_indeværende[,c("MATCH_WYID", "SEASON_WYID")], passes, by="MATCH_WYID")
#passes_indeværende <- left_join(passes_indeværende, common[, c("EVENT_WYID", "MATCHPERIOD", "MINUTE","TEAM_WYID","PLAYER_WYID","LOCATIONX","LOCATIONY")], by = "EVENT_WYID")

#loop til succesfulde afleveringer
filtered_df <- c()
for (j in 1:length(unique(passes_indeværende$MATCH_WYID))){
match1 <- passes_indeværende %>% filter(MATCH_WYID==unique(passes_indeværende$MATCH_WYID)[j])
teamz <- unique(match1$TEAM_WYID)
  for(i in 1:length(teamz)){
    filtered_df2 <- match1 %>% filter(TEAM_WYID==teamz[i])
    filtered_df1 <- filtered_df2 %>% filter(RECIPIENT_WYID %in% unique(filtered_df2$PLAYER_WYID))
    
    filtered_df <- rbind(filtered_df, filtered_df1)
  }}



Kun_passes <- passes_indeværende %>% filter(PRIMARYTYPE=="pass")
kun_passes_succes <- filtered_df %>% filter(PRIMARYTYPE=="pass", PLAYER_WYID != 0, RECIPIENT_WYID != 0)
kun_passes_unsucces <- anti_join(Kun_passes, kun_passes_succes, by="EVENT_WYID")


passes_indeværende <- rbind(
  cbind(kun_passes_succes, succesfuld_aflevering = TRUE), 
  cbind(kun_passes_unsucces, succesfuld_aflevering = FALSE))

afstand_vinkel_passes <- cbind(passes_indeværende, x_meter=passes_indeværende$LOCATIONX*0.01*105, y_meter=passes_indeværende$LOCATIONY*0.01*68) 
passes_indeværende <- subset(afstand_vinkel_passes, select = -c(LOCATIONX, LOCATIONY))
afstand_vinkel_passes <- cbind(passes_indeværende, end_x_meter=passes_indeværende$ENDLOCATIONX*0.01*105, end_y_meter=passes_indeværende$ENDLOCATIONY*0.01*68) 
passes_indeværende <- subset(afstand_vinkel_passes, select = -c(ENDLOCATIONX, ENDLOCATIONY))


#lav nogle gennemsnit på clusters og overordende statistikker under plottet.
kamp_statistik_passes <- passes_indeværende %>%
  group_by(MATCH_WYID, TEAM_WYID) %>%
  summarise(
    gns_X_afleveringer = mean(x_meter, na.rm = TRUE),
    gns_Y_afleveringer = mean(y_meter, na.rm = TRUE),
    antal_afleveringer_total = n(),
    antal_succesfulde_afleveringer = sum(succesfuld_aflevering == TRUE, na.rm = TRUE),
    succes_procent_afleveringer = (antal_succesfulde_afleveringer / antal_afleveringer_total) * 100,
    gns_længde_succes_afleveringer = mean(LENGTH[succesfuld_aflevering == TRUE], na.rm = TRUE)
  )

MATCHADVANCEDSTATS <- left_join(MATCHADVANCEDSTATS, kamp_statistik_passes, by=c("MATCH_WYID", "TEAM_WYID"))

clean_variables <- data.frame(original_variable_names=colnames(MATCHADVANCEDSTATS), 
                              new_clean_names=c("Kamp-ID", "Sæson-ID", "Liga-ID", "Hold-ID", "Antal skud i kampen for holdet", "Antal forseelser i kampen for holdet", 
                                               "Antal hjørnespark i kampen for holdet", "Antal røde kort i kampen for holdet", "Antal gule kort i kampen for holdet", 
                                               "Antal offsides i kampen for holdet","Antal driblinger i kampen for holdet", "Antal mål i kampen for holdet", 
                                               "Gennemsnit af forventet målchance pr. skud i kampen for holdet (Xg per shot)", "Gennemsnitlig skudafstand til målet i kampen for holdet",
                                               "Sum af forventet målchance i kampen for holdet (Xg-sum)", "Antal progressive løb i kampen for holdet", "Antal berøringer i målfeltet i kampen for holdet",
                                               "Antal forseelser begået imod holdet i kampen", "Antal skud på mål i kampen for holdet", "Antal skud som blev blokeret af modstanderen i kampen for holdet",
                                               "Antal skud udenfor målfeltet i kampen for holdet", "Antal skud på mål udenfor målfeltet i kampen for holdet",
                                               "Antal skud på stolpen i kampen for holdet", "Antal skud i målfeltet i kampen for holdet", "Antal skud på mål i målfeltet i kampen for holdet",
                                               "Antal frispark i kampen for holdet", "Antal skud i \"Danger Zone\" i kampen for holdet", "Antal indkast i kampen for holdet",
                                               "Antal indkast i venstre side i kampen for holdet", "Antal indkast i højre side i kampen for holdet",
                                               "Gennemsnitlig afstand i meter hvor afleveringer er slået fra egen baglinje i kampen for holdet",
                                               "Gennemsnitlig afstand i meter hvor afleveringer er slået fra højre side af banen i kampen for holdet", "Antal afleveringer i kampen for holdet",
                                               "Antal succesfulde afleveringer i kampen for holdet", "Procentdel af afleveringer som var succesfulde i kampen for holdet",
                                               "Gennemsnitlig længde på succesfulde afleveringer i kampen for holdet i meter"))

####################
#STATISTIKKER på 3.1
####################
spiller_statistik_passes <- passes_indeværende %>%
  group_by(PLAYER_WYID, TEAM_WYID) %>%
  summarise(
    gns_X_afleveringer = mean(x_meter, na.rm = TRUE),
    gns_Y_afleveringer = mean(y_meter, na.rm = TRUE),
    antal_afleveringer_total = n(),
    antal_succesfulde_afleveringer = sum(succesfuld_aflevering == TRUE, na.rm = TRUE),
    succes_procent_afleveringer = (antal_succesfulde_afleveringer / antal_afleveringer_total) * 100,
    gns_længde_succes_afleveringer = mean(LENGTH[succesfuld_aflevering == TRUE], na.rm = TRUE)
  ) %>% 
  filter(antal_afleveringer_total>50) %>% 
  arrange(desc(succes_procent_afleveringer))

spiller_statistik_passes <- left_join(spiller_statistik_passes, players, by="PLAYER_WYID")
spiller_statistik_passes <- left_join(spiller_statistik_passes, teams, by="TEAM_WYID")


hold_statistik_passes <- passes_indeværende %>%
  group_by(TEAM_WYID) %>%
  summarise(
    gns_X_afleveringer = mean(x_meter, na.rm = TRUE),
    gns_Y_afleveringer = mean(y_meter, na.rm = TRUE),
    antal_afleveringer_total = n(),
    antal_succesfulde_afleveringer = sum(succesfuld_aflevering == TRUE, na.rm = TRUE),
    succes_procent_afleveringer = (antal_succesfulde_afleveringer / antal_afleveringer_total) * 100,
    gns_længde_succes_afleveringer = mean(LENGTH[succesfuld_aflevering == TRUE], na.rm = TRUE)
  ) %>% 
  filter(antal_afleveringer_total>50) %>% 
  arrange(desc(succes_procent_afleveringer))

hold_statistik_passes <- left_join(hold_statistik_passes, teams, by="TEAM_WYID")


########################################################################################################################
#LOOP AF KOMBINATIONER
########################################################################################################################
sort2 <- data.frame(kombi=character(), maks_dist=numeric())
sort4 <- data.frame(kombi=character(), maks_dist=numeric())

for (l in 2:3){

j=31#for (j in 5:(ncol(MATCHADVANCEDSTATS)-1)){

for(i in c(5:(j-1), (j+1):ncol(MATCHADVANCEDSTATS))){#for (i in (j+1):ncol(MATCHADVANCEDSTATS)){
  

df <- MATCHADVANCEDSTATS
if(length(unique(df[,j]))>6){
if(length(unique(df[,i]))>6){

df <- na.omit(df[,c(j,i)])
df_scaled <- scale(df) 
df_scaled <- as.data.frame(df_scaled)


#within sum of squares
#fviz_nbclust(df_scaled, kmeans, method = "wss") +
 # labs(subtitle = "Elbow plot")

n_cluster <- l #aflæs i elbow-plot hvor kurven "knækker"
set.seed(123)
km4 <- tryCatch(
  kmeans(df_scaled, centers = n_cluster, nstart = 25),
  error = function(e) return(NULL)
)

if (is.null(km4)) next
km4df <- as.data.frame(dist(km4$centers))
km4df <- km4df[order(km4df[,1], decreasing = TRUE), ]
km4df <- as.data.frame(km4df)

colname <- paste(colnames(df)[1], colnames(df)[2], l, "clusters", sep = "_")

sort1 <- data.frame(kombi=colname, maks_dist=as.numeric(km4df[1,1]))
sort2 <- rbind(sort2, sort1)
}else{}
}else{}

}
  #}

  sort3 <- sort2[order(sort2$maks_dist, decreasing = TRUE),]
  sort3 <- 

  sort4 <- rbind(sort4, sort3[1:3,]) #DF med distancer på de kombinationer hvor clustre er længst fra hinanden, for 2:7 clustre.
  #sort2 <- data.frame(kombi=character(), maks_dist=numeric()) # hashtag denne ud hvis man gerne vil se på alle kombinationer
  }


#sort4 og sort2 er final output

########################################################################################################################
#3.2 KAMPE OG HOLD
########################################################################################################################

df <- MATCHADVANCEDSTATS
rownames(df) <- paste(df[,"MATCH_WYID"], df[,"TEAM_WYID"], sep = "_")
df <- na.omit(df[,5:ncol(df)])
#df <- na.omit(df[,c("SHOTS", "SHOTSOUTSIDEBOX")])
df_scaled <- scale(df) 
df_scaled <- as.data.frame(df_scaled)

#elbow
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  theme_minimal() + 
  labs(title = "Elbow-plot", 
       subtitle = "Vurdering af optimalt antal klynger",
       x = "Antal klynger (k)",
       y = "Sum af kvadratafvigelser")

n_cluster <- 3
set.seed(123)
km4 <- kmeans(df_scaled, centers = n_cluster, nstart = 25)
print(km4)

#total SS beskriver hvor god modellen er
#viz
km.clusters <- km4$cluster
fviz_cluster(km4, df_scaled) 
table(km.clusters, MATCHADVANCEDSTATS$TEAM_WYID)


km4$centers
dist(km4$centers)

clusters <- as.data.frame(km4$cluster)
df <- cbind(MATCHADVANCEDSTATS[,1:4], df, cluster=clusters[,1])


cluster_means <- data.frame(row.names = 1:32)
for (i in 1:n_cluster){
  cluster_means1 <- data.frame(colMeans(df[df$cluster == i, 5:36]))
  colnames(cluster_means1) <- paste0("Cluster_", i)
  cluster_means <- cbind(cluster_means, cluster_means1)
}

cluster_means <- cbind(variabel=colnames(df[5:36]), cluster_means)


cluster_means$ratio_mellem_min_og_max_clusterafstand <- apply(cluster_means[, 2:ncol(cluster_means)], 1, function(x) {
  max(x) / min(x)})

########################################################################################################################
#3.1 HOLD
########################################################################################################################

df <- as.data.frame(hold_statistik_passes)
rownames(df) <- df[,"TEAM_WYID"]
df <- na.omit(df[,2:7])
#df <- na.omit(df[,c("SHOTS", "SHOTSOUTSIDEBOX")])
df_scaled <- scale(df) 
df_scaled <- as.data.frame(df_scaled)

#elbow
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  theme_minimal() + 
  labs(title = "Elbow-plot", 
       subtitle = "Vurdering af optimalt antal klynger",
       x = "Antal klynger (k)",
       y = "Sum af kvadratafvigelser")

n_cluster <- 5
set.seed(123)
km4 <- kmeans(df_scaled, centers = n_cluster, nstart = 25)
print(km4)

#total SS beskriver hvor god modellen er
#viz
km.clusters <- km4$cluster
fviz_cluster(km4, df_scaled) 
table(km.clusters, hold_statistik_passes$TEAM_WYID)


km4$centers
dist(km4$centers)

clusters <- as.data.frame(km4$cluster)
df <- cbind(hold_statistik_passes[,1], df, cluster=clusters[,1])


cluster_means <- data.frame(row.names = 1:6)
for (i in 1:n_cluster){
  cluster_means1 <- data.frame(colMeans(df[df$cluster == i, 2:7]))
  colnames(cluster_means1) <- paste0("Cluster_", i)
  cluster_means <- cbind(cluster_means, cluster_means1)
}

cluster_means <- cbind(variabel=colnames(df[2:7]), cluster_means)


cluster_means$ratio_mellem_min_og_max_clusterafstand <- apply(cluster_means[, 2:ncol(cluster_means)], 1, function(x) {
  max(x) / min(x)})

########################################################################################################################
#3.1 SPILLERE
########################################################################################################################
df <- as.data.frame(spiller_statistik_passes)
rownames(df) <- paste(df[,"PLAYER_WYID"], df[,"TEAM_WYID"], sep = "_")
df <- na.omit(df[,3:8])
#df <- na.omit(df[,c("SHOTS", "SHOTSOUTSIDEBOX")])
df_scaled <- scale(df) 
df_scaled <- as.data.frame(df_scaled)

#elbow
fviz_nbclust(df_scaled, kmeans, method = "wss") +
  theme_minimal() + 
  labs(title = "Elbow-plot", 
       subtitle = "Vurdering af optimalt antal klynger",
       x = "Antal klynger (k)",
       y = "Sum af kvadratafvigelser")

n_cluster <- 5
set.seed(123)
km4 <- kmeans(df_scaled, centers = n_cluster, nstart = 25)
print(km4)

#total SS beskriver hvor god modellen er
#viz
km.clusters <- km4$cluster
fviz_cluster(km4, df_scaled) 
table(km.clusters, spiller_statistik_passes$TEAMNAME)


km4$centers
dist(km4$centers)

clusters <- as.data.frame(km4$cluster)
df <- cbind(spiller_statistik_passes[,1:2], df, cluster=clusters[,1])


cluster_means <- data.frame(row.names = 1:6)
for (i in 1:n_cluster){
  cluster_means1 <- data.frame(colMeans(df[df$cluster == i, 3:8]))
  colnames(cluster_means1) <- paste0("Cluster_", i)
  cluster_means <- cbind(cluster_means, cluster_means1)
}

cluster_means <- cbind(variabel=colnames(df[3:8]), cluster_means)


cluster_means$ratio_mellem_min_og_max_clusterafstand <- apply(cluster_means[, 2:ncol(cluster_means)], 1, function(x) {
  max(x) / min(x)})

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################




