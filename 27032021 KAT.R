# Karl-Anthony Towns AST% over his career
library(nbastatR)
library(tidyverse)
library(future)
library(ggtext)
library(scales)
library(gganimate)

source("C:\\Users\\Philippos\\Documents\\Projects\\Tools\\theme_white.R")

# Get every Regular Season game of KATS's career so far:
logs_kat <- game_logs(seasons=2016:2021,result_types="player") %>% 
  filter(namePlayer=="Karl-Anthony Towns")


# Extract players' box-scores of such games:
plan(multiprocess)
box_scores(game_ids=logs_kat$idGame,box_score_types="Advanced",
           result_types="player")

# Filter Towns' boxscores and select AST% & USG%:
box_kat <- dataBoxScorePlayerNBA %>% 
  filter(namePlayer=="Karl-Anthony Towns") %>% 
  mutate(`AST%`=pctAST,`USG%`=pctUSG) %>% 
  select(idGame,namePlayer,`AST%`,`USG%`)

# Find how many Regular Season games he played each year:
s15 <- length(which(box_kat$idGame<=21600000)) # 2015-16
s16 <- length(which(box_kat$idGame>21600000 & box_kat$idGame<=21700000)) # 2016-17
s17 <- length(which(box_kat$idGame>21700000 & box_kat$idGame<=21800000)) # 2017-18
s18 <- length(which(box_kat$idGame>21800000 & box_kat$idGame<=21900000)) # 2018-19
s19 <- length(which(box_kat$idGame>21900000 & box_kat$idGame<=22000000)) # 2019-20
s20 <- length(which(box_kat$idGame>22000000)) # 2020-21



theme_set(theme_white())
p1 <- ggplot(box_kat,aes(x=1:383,y=`AST%`)) +
  geom_line(color="#236192",size=1.2) + 
  geom_line(aes(y=`USG%`),color="#78BE20",size=1.2) +
  geom_vline(xintercept = s15,linetype = "dashed",color="grey15") +
  geom_vline(xintercept = s15+s16,linetype = "dashed",color="grey15") +
  geom_vline(xintercept = s15+s16+s17,linetype = "dashed",color="grey15") +
  geom_vline(xintercept = s15+s16+s17+s18,linetype = "dashed",color="grey15") +
  geom_vline(xintercept = s15+s16+s17+s18+s19,linetype = "dashed",color="grey15") +
  labs(y="",x="Seasons",
       title="Karl-Anthony Towns: Multi-Year <span style = 'color: #236192;'>AST%</span> & <span style = 'color: #78BE20;'>USG%</span>") +
  theme(plot.title = element_markdown()) +
  scale_x_continuous(expand=c(0,0),breaks = c(82,164,246,323,358),
                   labels=c("2015-16","2016-17","2017-18","2018-19","2019-20")) +
  scale_y_continuous(labels=percent)

ggsave("career.png",p1,width=24,height=12,dpi=700)

theme_set(theme_white())
p2 <- ggplot(box_kat[359:383,],aes(x=359:383,y=`AST%`)) +
  geom_line(color="#236192",size=1.2) + 
  geom_line(aes(y=`USG%`),color="#78BE20",size=1.2) +
  geom_vline(xintercept = 370,linetype = "dashed",color="grey15") +
  labs(y="",x="Game",
       title="Karl-Anthony Towns: <span style = 'color: #236192;'>AST%</span> & <span style = 'color: #78BE20;'>USG%</span>",
       subtitle="2020-21 Regular Season, as of March 27, 2021") +
  theme(plot.title = element_markdown()) +
  scale_x_continuous(expand=c(0,0),breaks=c(5,11,15,20,25)+359,
                     labels=c("5","11(Chris Finch hired)","15","20","25")) +
  scale_y_continuous(labels=percent)

ggsave("this.png",p2,width=18,height=12,dpi=700)
  














