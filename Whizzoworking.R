tmp_countries <- rename(tmp_countries,origin= "xls_flavratings$\"Country of Bean Origin\"")
colnames(tmp_countries)
tmp_countries %>%
    group_by(origin) %>%
    summarize(avgrating = )

xls_flavratings %>%
  rename(origin = "Country of Bean Origin", varietal = "Specific Bean Origin or Bar Name", rating = "Rating") %>%
  select(origin,varietal, rating) -> flavratings

flavratings %>%
  group_by(origin) %>%
  filter(origin != "Blend") %>%
  summarize(avgrating = mean(rating)) %>%
  arrange(-avgrating)

library(readxl)

table(xls_cspotratings$SOURCE)

setdiff(flavratings$origin, xls_cspotratings$SOURCE        )
setdiff(xls_cspotratings$SOURCE,flavratings$origin        )

table(xls_cspotratings$TYPE)
colnames(xls_cspotratings)
xls_cspotratings$Parameter<-str_replace_all(xls_cspotratings$Parameter, c(" " = "." , "," = "" ))
names(xls_cspotratings)<-str_replace_all(names(xls_cspotratings), c(" " = "." , "," = "" ))
names(xls_cspotratings)<- make.names(names(xls_cspotratings))

setdiff(flavratings$origin, cspotratings$origin)
setdiff(cspotratings$origin, flavratings$origin )

table(cspotratings$rating)
table(flavratings$rating)

cspotratings %>%
  summarise(cmean = mean(rating, na.rm=TRUE), cmedian = median(rating), csd = sd(rating))

cspotratings[is.na(cspotratings$rating),]
table(allratings$origin)
allratings %>%
  group_by(origin) %>%
  summarize(mean = mean(zrate, na.rm=TRUE), count=n()) %>%
  arrange(-mean) %>%
  filter(count >= 10) %>%
  filter(mean >= 0) %>%
  print(n=30)


allratings %>%
  filter(origin != "Blended")

View(as_tibble(unique(allratings$origin)))

setdiff(allratings$origin, xls_prod$Country)
View(xls_prod$Country)

setdiff(xls_prod$Country,allratings$origin )

tmp_prod <- xls_prod
colnames(tmp_prod)
tmp_prod[tmp_prod$Country == "Belize",]
View(rbind(tmp_prod,tmp_prod[tmp_prod$Country == "Trinidad and Tobago",]))
rbind(tmp_prod,tmp_prod[Country=="Trinidad and Tobago"])

tmp_prod %<>% rbind(tmp_prod[tmp_prod$Country == "Trinidad and Tobago",])
tmp_prod$Country
tmp_prod$Country = sub("Trinidad and Tobago","Trinidad",tmp_prod$Country )

View(contenders)
colnames(xls_prod2)
class(xls_prod2)
xls_prod2$`5‑years CAGR`
xls_prod2$YoY
foo <- contenders
merge(x = contenders, y = xls_prod2[,c("Country","prod_risk")],by.x="origin", by.y="Country" )
merge(x = contenders, y = xls_regions[,c("Country","Region")],by.x="origin", by.y="Country" )
rm(xls_regions)

xls_regions <- read_excel("regions.xlsx")

contenders %>%
  arrange(-mean) %>%
  print(as_tibble(contenders), n = 30)


contenders %>%
  ggplot(aes(x=reorder(origin,-mean), y = mean, fill=factor(ifelse(prod_risk,"Declining","Growing")))) +
  geom_col() + 
  scale_fill_manual(name = "Production", values=c("yellow","chocolate4")) +
  labs(x="Cocoa Origin", y="Average Rating (z-score)") +
  theme(axis.text.x = element_text(angle=60))


viable <- contenders %>%
  filter(prod_risk == FALSE) %>%
  filter(mean >= 0)

allratings %>%
  filter(origin %in% viable$origin) %>%
  group_by(origin) %>%
  summarise(mean = mean(zrate, na.rm=TRUE), count=n())

library(ggridges)
library(viridis)
library(hrbrthemes)
install.packages("ggridges")
install.packages("viridis")
install.packages("hrbrthemes")

allratings %>%
  ggplot(aes(x = `zrate`, y = `origin`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "zrate", option = "C") +
  labs(title = 'Numbers of Samples') +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )
allratings %>%
  filter(origin %in% viable$origin) %>%
  ggplot(aes(x = `zrate`, y = reorder(`origin`,-zrate), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Average Bar Rating", option = "magma") +
  labs(title = 'Numbers of Samples') 

table(allratings$zrate)
allratings %>%
  paste(allratings$origin, " ... ",str_split(allratings$varietal,",",n=1))

allratings %>%

  filter(varietal != "Blend" & is.na(varietal) == FALSE) %>%
  mutate(combokey = paste(origin,":",str_split(varietal,",",n=1), sep="")) %>%
  group_by(combokey) %>%
  summarise(mean_var = round(mean(zrate),2),var_cnt = n()) %>%
  filter(mean_var >= 0.5) %>%
  arrange(-mean_var, -var_cnt) %>%
  View(title="Top Varietals")

library(stringr)
allratings %>%
  filter(origin %in% viable$origin) %>%
  filter(varietal != "Blend" & is.na(varietal) == FALSE) %>%
  mutate(combokey = paste(origin,":",lapply(strsplit(varietal,", "), function(x) x[1]), sep="")) %>%
  group_by(origin, combokey) %>%
  summarise(mean_var = round(mean(zrate),2),var_cnt = n()) %>%
  filter(mean_var >= 0.5) %>%
  arrange(origin, desc(mean_var)) %>%
  rename("country:varietal" = "combokey","avg_zscore"="mean_var", "num_samples" = "var_cnt") %>%
  View(title="Top Varietals") ->top-varietals


