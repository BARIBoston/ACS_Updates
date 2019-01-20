library(sf)
library(tidyverse)
library(tidycensus)

# Create ACS data
acs <- get_acs(geography = "tract",
        survey = "acs5",
        state = "ma",
        year = 2017,
        output = "wide",
        variables = c("B24080_009", # Males federally employed
                      "B24080_018", # Females federally employed
                      "B19057_002", # households with public assistance
                      "B19057_001", # total households
                      "B23025_003"), # Employed age 16 + 
        geometry = TRUE) %>% 
  mutate(fed_emp = B24080_009E + B24080_018E,
         total_emp = B23025_003E,
         pub_assist = B19057_002E / B19057_001E,
         prop_fed_emp = fed_emp / total_emp) %>% 
  select(geoid = GEOID, name = NAME, fed_emp, total_emp, pub_assist, prop_fed_emp)
  

# Filter to just suffolk county

suffolk <- acs %>% 
  separate(name, into = c("tract", "county", "state"), sep = ",") %>% 
  filter(str_detect(county, "Suffolk"))

plot(st_geometry(suffolk))
 
x <- ggplot(suffolk) +
  geom_sf(aes(fill = (prop_fed_emp))) +
  theme_minimal()
  

# choropleth of public assistance
y <- ggplot(suffolk) +
  geom_sf(aes(fill = pub_assist)) +
  theme_minimal()

library(patchwork)

print(x + y)
# compute quantiles (dividing into 3rds) 
fed_employ_q <- quantile(suffolk$prop_fed_emp,
                         c(1/3, 2/3, 1),
                         na.rm = TRUE)

pub_assist_q <- quantile(suffolk$pub_assist,
                         c(1/3, 2/3, 1),
                         na.rm = TRUE)

suffolk_q <- suffolk %>% 
  mutate(
    color_fed = case_when(
      prop_fed_emp < fed_employ_q[1] ~ 1,
      prop_fed_emp < fed_employ_q[2] ~ 3,
      prop_fed_emp <= fed_employ_q[3] ~ 3
    ), 
    color_pub = case_when(
      pub_assist < pub_assist_q[1] ~ 1,
      pub_assist < pub_assist_q[2] ~ 2,
      pub_assist <= pub_assist_q[3] ~ 3
  ),
  color_val =  atan(color_pub/color_fed)) 

View(suffok_q)


ggplot(suffolk_q) +
  geom_sf(aes(fill = color)) +
  scale_fill_viridis()

# plot bivariate relationship
na.omit(suffolk_q) %>% 
ggplot(aes(pub_assist, prop_fed_emp,
           color = color_val,
           alpha = color_pub + color_fed)) +
  geom_point(alpha = 1) +
  geom_hline(yintercept = fed_employ_q, color = "gray20", linetype = 2) +
  geom_vline(xintercept = pub_assist_q, color = "gray20", linetype = 2) + 
  scale_color_viridis(name="Color scale") + 
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10()


d<-expand.grid(x=1:100,y=1:100)



g.legend<-
  ggplot(d, aes(x,y,fill=atan(y/x),alpha=x+y))+
  geom_tile()+
  scale_fill_viridis()+
  theme_void()+
  theme(legend.position="none",
        axis.title=element_text(size=5),
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10))+
  theme(axis.title=element_text(color="black"))+ 
  labs(x="Housing unit growth",
       y="Population growth")



d<-expand.grid(x=1:3,y=1:3)
#dlabel<-data.frame(x=1:3,xlabel=c("X low", "X middle","X High"))
d<-merge(d,data.frame(x=1:3,xlabel=c("X low", "X middle","X high")),by="x")
d<-merge(d,data.frame(y=1:3,ylabel=c("Y low", "Y middle","Y high")),by="y")

g.legend<-
  ggplot(d, aes(x,y,fill=atan(y/x),alpha=x+y,label=paste0(xlabel,"\n",ylabel)))+
  geom_tile()+
  geom_text(alpha=1)+
  scale_fill_viridis()+
  theme_void()+
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10))+
  labs(title="A bivariate color scheme (Viridis)",x="X",y="Y")+
  theme(axis.title=element_text(color="black"))+
  # Draw some arrows:
  geom_segment(aes(x=1, xend = 3 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=1, yend = 3), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) 

suffolk_q %>% 
  ggplot(aes(prop_fed_emp, pub_assist, color = color_val)) +
  geom_point()







acs %>% 
  ggplot(aes(prop_fed_emp, pub_assist, color = prop_fed_emp)) +
  geom_point(alpha = .5) +
  theme_minimal() 


acs %>% 
  ggplot(aes(prop_fed_emp)) +
  geom_histogram()

hist(acs$prop_fed_emp)
hist(acs$pub_assist)

x <- na.omit(acs) 


