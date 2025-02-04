library(dplyr)
library(ggplot2)

# https://www.gao.gov/products/GAO-14-215
# https://www.opm.gov/data/Index.aspx?tag=FedScope
# https://www.opm.gov/Data/Files/543/b8c3885d-a8e2-43ce-83f4-0398a1dfbc33.zip

nab <- read.csv("D:/geodata/government_units/nrcs_area_boundaries.csv")

area <- soilDB::dbQueryNASIS(
  soilDB::NASIS(), 
  q = "
  SELECT areatypename, areasymbol, areaname, areaacres, obterm, recwlupdated 
  FROM area
  INNER JOIN (SELECT areatypename, areatypeiid FROM areatype) AS a ON a.areatypeiid = areatypeiidref
  
  WHERE areatypename IN ('MLRA Soil Survey Area', 'MLRA Soil Survey Regional Office Area')
  ")
state <- strsplit(area$areaname, ", ")
idx   <- sapply(state, length)
area$state <- mapply(function(x, idx) x[idx], state, idx)
area$state.abb <- state.abb[match(area$state, state.name)]
area$state <- area$state |> tolower()

fy <- seq(as.Date("1997-10-01"), by = "year", length = 28)
area$fy <- substr(area$recwlupdated, 1, 10) |> as.Date()
area$fy <- cut(area$fy, fy, include.lowest = TRUE) |> as.character()
area$fy <- area$fy |> substr(1, 4) |> as.integer() + 1

test <- rvest::read_html("https://www.opm.gov/data/datasets/Index.aspx")
tb <- test |> rvest::html_table()
tb2 <- tb[[2]]
tb3 <- tb2 |> subset(Downloads != "")

href <- test |> rvest::html_elements("a") |> html_attr("href")
zip <- href[grep(".pdf", href) + 1]

dir <- cbind(tb3, zip = paste0("https://www.opm.gov", zip))
dir2 <- dir |> subset(`Data Set` %in% paste0("FedScope Employment Cube (September ", 1998:2024, ")"))
df <- rbind(dir[1, ], dir2)
names(df)[names(df) %in% c("Data Set", "zip")] <- c("filenames", "files")
df[df$filenames == "FedScope Employment Cube (September 2015)", "files"] <- "https://www.opm.gov/data/datasets/Files/413/de1dd3f7-0c39-46ab-a1c6-848be284358b.zip"


# download files
lapply(df$files[10], function(x) {
  x2 = strsplit(x, "\\/")[[1]][8]
  options(timeout = 60 * 3)
  download.file(x, paste0("C:/workspace2/", x2))
  })


# unzipping
split(df, df$files) ->.;
lapply(., function(x) {
  cat("unzipping", x$files, as.character(Sys.time()), "\n")
  x2   = strsplit(x$files, "\\/")[[1]][8]
  zf = unzip(paste0("C:/workspace2/", x2), list = TRUE)
  unzip(paste0("C:/workspace2/", x2), files = zf$Name[grepl("FACTDATA_", zf$Name)])
  })

# reading
# staff <- read.csv("FACTDATA_SEP2017.TXT", stringsAsFactors = FALSE)
names(staff) <- tolower(names(staff))

test <- list.files()
test <- data.frame(files = test[grepl("FACTDATA", test)], stringsAsFactors = FALSE)
test$fy <- as.numeric(substr(test$files, 13, 16))
cn <- c("AGYSUB", "LOC", "AGELVL", "EDLVL", "GSEGRD", "LOSLVL", "OCC", 
"PATCO", "PPGRD", "SALLVL", "STEMOCC", "SUPERVIS", "TOA", 
"WORKSCH", "WORKSTAT", "DATECODE", "EMPLOYMENT", "SALARY", "LOS", 
"fy")

staff <- {
  split(test, test$fy) ->.;
  lapply(., function(x) {
  cat("reading", x$files, as.character(Sys.time()), "\n")
  temp = read.csv(x$files, stringsAsFactors = FALSE)
  temp$fy = x$fy
  # subset soil scientists and USDA employees
  temp = subset(temp, OCC == "0470" | grepl("^AG", AGYSUB))
  return(temp[cn])
  }) ->.;
  do.call("rbind", .)
}

names(staff) <- tolower(names(staff))
# save(staff, file = "C:/workspace2/gov_staff.RData")


# load metadata
files <- list.files()
idx <- grepl("^DT", files)
dd <- gsub("DT|.txt", "", files[idx])

files <- lapply(files[idx], function(x) {
  temp = read.csv(x, stringsAsFactors = FALSE)
  names(temp) <- tolower(names(temp))
  return(temp)
  })
names(files) <- dd

staff2 <- within(staff, {
  agysub = factor(agysub, levels = files$agy$agysub,      labels = files$agy$agysubt)
  # agyt   = factor(agysub, levels = files$agy$agysub,      labels = files$agy$agyt)
  loc    = factor(loc,    levels = files$loc$loc,         labels = files$loc$loct)
  agelvl = factor(agelvl, levels = files$agelvl$agelvl,   labels = files$agelvl$agelvlt)
  edlvl  = factor(edlvl,  levels = files$edlvl$edlvl,     labels = files$edlvl$edlvlt)
  date   = as.Date(as.character(datecode), "%Y%M")
  occt    = factor(occ,    levels = files$occ$occ,         labels = files$occ$occt)
  patco  = factor(patco,  levels = files$patco$patco,     labels = files$patco$patcot)
  sallvl = factor(sallvl, levels = files$sallvl$sallvl,   labels = files$sallvl$sallvlt)
  toa    = factor(toa,    levels = files$toa$toa[-11],         labels = files$toa$toat[-11])
  supervis = factor(supervis, levels = files$super$supervis,  labels = files$super$supervist)
  stemocct = factor(stemocc,  levels = files$stemocc$stemocc, labels = files$stemocc$stemocct)
  # ppttypt  = factor(ppgrd,    levels = files$ppgrd$ppgrd[-2],     labels = files$ppgrd$pptypt[-2])
  workstat = factor(workstat, levels = files$wkstat$workstat, labels = files$wkstat$workstatt)
  worksch  = factor(worksch,  levels = files$wrksch$worksch,  labels = files$wrksch$workscht)
})
# save(staff2, file = "C:/workspace2/gov_staff2.RData")
load(file = "C:/workspace2/gov_staff2.Rdata")

state <- staff2$loc |> as.character() |> tolower() |> strsplit("-")
idx   <- sapply(state, length)
staff2$state <- mapply(function(x, idx) x[idx], state, idx)
staff2$state.abb <- state.abb[match(staff2$state, tolower(state.name))]



# 0470 Series
gg_ss <- filter(staff2, occ == "0470") %>%
  group_by(fy, agysub) %>%
  summarize(n = length(occ)) %>%
  arrange(-fy, - n) %>%
  ggplot(aes(x = fy, y = n, col = agysub)) +
  geom_line(lwd = 2) +
  ylab("# of soil scientists") + xlab("fiscal year") +
  ggtitle("0470 Series in the Federal Government")


# SS as a share of NRCS
gg_nrcs <- staff2 %>%
  filter(grepl("NATURAL RESOURCES CON", agysub)) %>%
  mutate(soil_scientist = occ == "0470") %>%
  group_by(fy, agysub, soil_scientist) %>%
  summarize(n = length(occ)
            ) %>%
  arrange(-fy, - n) %>%
  ungroup()

gg_nrcs <- mutate(gg_nrcs, n_0470_spsd = ifelse(soil_scientist == TRUE, n - nrow(nab) - 50, NA))
names(gg_nrcs)[names(gg_nrcs) == "n"] <- "n_0470"

gg_nrcs %>%
  filter(soil_scientist == TRUE) %>%
  ggplot(aes(x = fy, y = n_0470)) +
  geom_line() +
  geom_line(aes(y = n_0470_spsd))


gg_nrcs %>%
  ggplot(aes(x = fy, y = n, fill = soil_scientist)) +
  geom_area() +
  ylab("# of employees") + xlab("fiscal year") +
  ggtitle("NRCS Employees")


gg_pct <- filter(staff2, grepl("NATURAL RESOURCES CON", agysub)) %>%
  mutate(SS = occ == "0470") %>%
  group_by(fy) %>%
  summarize(pct = sum(SS) / length(SS) * 100) %>%
  ggplot(aes(x = fy, y = pct)) +
  geom_line(lwd = 1) +
  ylab("% of soil scientists") + xlab("fiscal year") +
  ggtitle("NRCS")

gridExtra::grid.arrange(gg_ss, gg_nrcs, gg_pct)


staff2 %>%
  ggplot(aes(y = n, x = ppgrd)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ fy) +
  coord_flip() + 
  ggtitle("Age Brackets of the 0470 Series (NRCS 01 + 02)")

# NRCS in IN
Title <- "Midwest NRCS"
staff2 %>%
  filter(grepl("IOWA|MISSOURI|WISCONSIN|INDIANA|ILLINOIS|MINNESOTA|OHIO|NORTH DAKO|NEBRAS", loc) & grepl("NATURAL RESOURCES CON", agysub)) %>%
  group_by(loc, fy) %>%
  summarize(n = length(loc)) %>%
  ggplot(aes(x = fy, y = n, col = loc)) +
  geom_line(lwd = 1.5) +
  ylab("# of employees") + xlab("fiscal year") +
  ggtitle(Title)


staff2 |> 
  filter(
    agysub == "AG16-NATURAL RESOURCES CONSERVATION SERVICE" 
    & occ == "0470" 
    ) %>% 
  group_by(fy) %>%
  count(state, supervis, ppgrd) %>%
  filter(state == "indiana") |> 
  View()

area |>
  group_by