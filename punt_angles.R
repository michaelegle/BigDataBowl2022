#load in libraries
library(tidyverse)
library(ggthemes)
library(mgcv)

#load in PFF data for hangtimes
pff_dat <- read.csv('PFFScoutingData.csv') %>% 
  filter(!is.na(hangTime) & kickType %in% c('N','R','A')) %>% 
  filter(!is.na(kickType)) %>% 
  select(gameId, playId, hangTime)

#load in extra play data and filter out OOB, Fakes, and Blocked punts
punts <- read.csv('plays.csv') %>% 
  filter(specialTeamsPlayType == 'Punt') %>% 
  select(gameId, playId, specialTeamsPlayType, specialTeamsResult) %>% 
  filter(!specialTeamsResult %in% c('Out of Bounds','Non-Special Teams Result','Blocked Punt'))
  
#This loads in all the angles
punt_angles <- c(2018:2020) %>% 
  list() %>% 
  pmap_dfr(function(x){
    print(x)
    
    #load year
    read.csv(paste0('tracking',x,'.csv')) %>% 
      
      #we only care about what the ball does
      filter(displayName == 'football') %>% 
      
      #join extra data
      left_join(punts) %>%
      
      #this filters out OOB, Fakes, Blocks
      filter(!is.na(specialTeamsResult)) %>%
      
      #We don't care about any of this info
      select(-jerseyNumber:-team, -a, -o, -dir, -nflId, -time, -dis, -displayName) %>% 
      select(-specialTeamsPlayType) %>% 
      
      #Do cleaning for each play
      group_by(gameId, playId) %>% 
      mutate(
        
        #There's a bunch of plays where the snap isn't marked so we can't work from there
        has_snap = ifelse('ball_snap' %in% event, 1, 0),
        
        #using the kinematics assumption, this will be our kick speed
        max_s = max(s),
        
        #this lets us get the yardline the ball was snapped from that we can feed into our model
        yardline = 
          case_when(
            playDirection == 'left' ~ round(x[frameId == 1] - 10), #for communication purposes we round to closest integer
            playDirection == 'right' ~ round(110 - x[frameId == 1]) #the rounding shouldn't make too much of a difference
          )
      ) %>% 
      
      #keep only plays with snaps marked
      filter(has_snap == 1) %>% 
      
      #don't need this info anymore
      select(-has_snap) %>% 
      
      #get the frame of the snap
      mutate(
        snap_frame = frameId[event == 'ball_snap']
      ) %>% 
      
      #look at the frames after the marked snap where the ball isn't still
      #this will make sure we're only looking at when the ball is actually snapped
      filter(frameId >= snap_frame & s != 0) %>%
      
      #since the ball is already moving by now the frame that the punter catches the snap will be 
      #the frame where the ball is basically at a standstill (minimum speed). We'll only look for 
      #this in the second following the snap of the ball
      mutate(
        snap_catch_frame = first(frameId[s == min(s[frameId <= snap_frame + 10])])
      ) %>% 
      
      #Now we only want to look at the frames after the snap was caught
      filter(frameId >= snap_catch_frame) %>% 
      mutate(
        
        #the maximum speed the ball reaches in the next two seconds after this catch we'll say was the moment 
        #the ball was kicked, per our basic kinematics assumptions
        kick_frame = first(frameId[s == max(s[frameId <= snap_catch_frame + 20])]),
        
        #this is for convenience
        event = ifelse(frameId == kick_frame, 'KICK', event)
      ) %>% 
      ungroup() %>% 
      
      #now we don't care about this frames
      select(-snap_frame, -snap_catch_frame, -kick_frame) %>% 
      
      #we only want when the ball is marked as kicked, when it hits the ground, or when it is caught
      filter(event %in% c('KICK','punt_land','punt_received','fair_catch')) %>% 
      
      #sometimes there will be multiple of these in a play, so we'll only look at the first one
      group_by(gameId, playId) %>% 
      mutate(
        row_num = row_number()
      ) %>% 
      ungroup() %>% 
      
      #now we'll only be left with two rows: one when the ball was kicked, and one when the ball landed/was caught
      filter(row_num <= 2) %>% 
      
      #add the PFF data
      left_join(pff_dat) %>% 
      
      #puts everything in one row because this is how my brain works
      group_by(gameId, playId) %>% 
      mutate(
        land_x = lead(x),
        land_y = lead(y)
      ) %>% 
      ungroup() %>% 
      
      #now we get one play on a single row
      filter(row_num == 1) %>% 
      select(-row_num) %>% 
      
      #Just a series of intermediate steps to get the final solution of the launch angle
      mutate(
        dx = (land_x - x)^2, 
        dy = (land_y - y)^2,
        s_2 = max_s^2,
        vz = sqrt(s_2 - (1/hangTime)^2*(dx+dy)),
        phi = (asin(vz/max_s))*(180/pi),
        height = vz*(hangTime/2)*3 #this is just for fun we don't use it for anything
      ) 
  })

#Congrats! You read the cleaning and calculating part of the code! You get a fun fact.
#The fun fact is that if air resistance played no part in a punt's path, 75% of punts
#would hit the big board in Jerry World! Anyways,

#get the modelling factors ready
ml_fit <- punt_angles %>% 
  
  #5% of the remaining punts were weirdly marked/had technology malfunctions so we got rid of them
  filter(!is.na(phi)) %>% 
  
  #this only keeps the variables we want and one hot encodes them (if that's the right term)
  select(max_s, yardline, phi, specialTeamsResult) %>% 
  mutate(
    res = case_when(
      specialTeamsResult == 'Fair Catch' ~ 0,
      specialTeamsResult == 'Downed' ~ 1,
      specialTeamsResult == 'Return' ~ 2,
      specialTeamsResult == 'Touchback' ~ 3
    )
  ) %>% 
  select(-specialTeamsResult) %>% 
  
  #this takes out muffs, of which there were 1% of the remaining sample
  filter(!is.na(res))

#here we run things through the model based on what I read from the tutorial 
#on how to do GAM multinomial logisitic regression from the mgcv documentation
yardline <- ml_fit$yardline
phi <- ml_fit$phi
max_s <- ml_fit$max_s
res <- ml_fit$res

#plz let this work
#small note that this has to be a GAM since the relationships are all nonlinear
plz <- gam(list(res~s(yardline)+s(phi)+s(max_s),
         ~s(yardline)+s(phi)+s(max_s),
         ~s(yardline)+s(phi)+s(max_s)),
    family=multinom(K=3))

#some quick model checks
plot(plz, pages = 1)
gam.check(plz)

#We only care about the relationships between the variables and aren't really trying to predict anything out of sample
#so we don't need to split into training and testing sets

#For the purposes of the viz we'll make some fake data that are well within the distributions of each variable
plot_gr <- expand.grid(yardline = seq(40, 99, length=60), phi=seq(40,60,length=60),max_s=seq(15,25,length=60))
pred_gr <- predict(plz, newdata = plot_gr, type = 'response')

#for the purposes of the viz we'll only look at the most likely outcome
pred_cat <- apply(pred_gr,1,function(x) which(max(x)==x)[1])-1

#this just puts everything into a dataframe because this is how my brain works
plot_data <- data.frame(
  yardline = plot_gr$yardline,
  max_s = plot_gr$max_s,
  phi = plot_gr$phi,
  y = pred_cat
) %>% 
  mutate(
    res = case_when(
      y == 0 ~ 'Fair Catch',
      y == 1 ~ 'Downed',
      y == 2 ~ 'Return',
      y == 3 ~ 'Touchback'
    )
  )

#To make the shiny app as simple as possible we put it into a csv. 
#This will not be included in the GitHub because it is large
write.csv(plot_data, 'plot_data.csv')

#This makes the punt outcomes viz
ggplot(plot_data %>% filter(yardline == 57))+
  geom_tile(mapping = aes(x = phi, y = max_s, fill = res))+
  scale_fill_manual(values = c('Return'='red','Fair Catch'='forestgreen','Touchback'='darkblue','Downed'='violet'))+
  theme_minimal()+
  theme(
    panel.background = element_rect(color = 'transparent'),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    x = 'Phi (deg)',
    y = 'Punt Speed (Yds/sec)',
    title = 'Likeliest Punt Outcomes at the -43 Yardline',
    fill = 'Punt Outcome'
  )

ggsave('outcome.png', height = 7, width = 13)

#this makes the phi vs P(Fair Catch) viz
punt_angles %>% 
  mutate(
    fc = ifelse(specialTeamsResult == 'Fair Catch', 1, 0)
  ) %>% 
  ggplot()+
  geom_smooth(mapping = aes(x = phi, y = fc))+
  theme_minimal()+
  theme(
    panel.background = element_rect(color = 'transparent'),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    x = 'Phi (deg)',
    y = 'Probability of Fair Catch',
    title = 'Probability of Fair Catch v Launch Angle Phi'
  )

ggsave('fc_v_phi.png', height = 7, width = 13)

#This makes the phi vs hang time viz
punt_angles %>% 
  ggplot()+
  geom_smooth(mapping = aes(x = phi, y = hangTime))+
  theme_minimal()+
  theme(
    panel.background = element_rect(color = 'transparent'),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    x = 'Phi (deg)',
    y = 'Hang Time (s)',
    title = 'Hang Time v Launch Angle Phi'
  )

ggsave('ht_v_phi.png', height = 7, width = 13)
