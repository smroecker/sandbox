library(dplyr)
library(ggplot2)

# https://www.gao.gov/products/GAO-14-215
# https://www.opm.gov/data/Index.aspx?tag=FedScope
# https://www.opm.gov/Data/Files/543/b8c3885d-a8e2-43ce-83f4-0398a1dfbc33.zip

dir <- c(
  FY2018_2Q = "https://www.opm.gov/Data/Files/543/b8c3885d-a8e2-43ce-83f4-0398a1dfbc33.zip",
  FY2018 = "https://www.opm.gov/Data/Files/522/7a0bf199-6c16-4131-92d1-485b18f7878a.zip",
  FY2017 = "https://www.opm.gov/Data/Files/490/ae0351fd-58d1-47d5-b1aa-2ca3bf977d30.zip",
  FY2016 = "https://www.opm.gov/Data/Files/413/de1dd3f7-0c39-46ab-a1c6-848be284358b.zip",
  FY2015 = "https://www.opm.gov/Data/Files/381/f5e03461-5674-4fbe-8538-10a77e7a2739.zip",
  FY2014 = "https://www.opm.gov/Data/Files/331/55974d49-94f0-43e7-acf3-77bf864e802a.zip",
  FY2013 = "https://www.opm.gov/Data/Files/253/5a21ef01-11d7-4a87-bcee-0b8c063e19fc.zip",
  FY2012 = "https://www.opm.gov/Data/Files/235/caac291b-001d-4568-a8be-96215c319fd4.zip",
  FY2011 = "https://www.opm.gov/Data/Files/172/c21d50d7-9b48-432c-a03e-96d5fb93f5fa.zip",
  FY2010 = "https://www.opm.gov/Data/Files/26/f0a8eef6-a0b5-4015-a2f4-6597f1ca3ae7.zip",
  FY2009 = "https://www.opm.gov/Data/Files/38/3653d805-eb0a-4e70-b96d-39b7c58347f0.zip",
  FY2008 = "https://www.opm.gov/Data/Files/50/7b7655fd-b4d0-4e15-9e97-33956b8aca09.zip",
  FY2007 = "https://www.opm.gov/Data/Files/53/b9945224-54e1-45e3-9780-b6b36b845b4b.zip",
  FY2006 = "https://www.opm.gov/Data/Files/56/f00a1a8f-d865-4214-8051-049f66d322be.zip",
  FY2005 = "https://www.opm.gov/Data/Files/59/b5c2a4f7-5ac5-44f7-bd3c-0f2cfb000e21.zip",
  FY2004 = "https://www.opm.gov/Data/Files/62/ecdd5c00-aeda-45ff-bd74-975714ee10a6.zip",
  FY2003 = "https://www.opm.gov/Data/Files/65/a5e33b42-51aa-41f0-8d1c-0dcde6c6ff2a.zip",
  FY2002 = "https://www.opm.gov/Data/Files/68/22fb81c4-51bb-4309-a9e5-de7fc891d299.zip",
  FY2001 = "https://www.opm.gov/Data/Files/71/f74f444b-b84b-4a75-b7d5-83726b80a320.zip",
  FY2000 = "https://www.opm.gov/Data/Files/74/70967261-28e6-4598-ad8a-69f0f6b04532.zip",
  FY1999 = "https://www.opm.gov/Data/Files/77/25455df4-0c25-49f3-a460-147b3aa596c8.zip"
  )

df <- data.frame(files = dir[-1], filenames = names(dir)[-1], stringsAsFactors = FALSE)


# download files
lapply(dir, function(x) {
  x2 = strsplit(x, "\\/")[[1]][7]
  download.file(x, paste0("C:/workspace2/", x2))
  })


# unzipping
split(df, df$filenames) ->.;
lapply(., function(x) {
  cat("unzipping", x$files, as.character(Sys.time()), "\n")
  x2   = strsplit(x$files, "\\/")[[1]][7]
  year = as.numeric(substr(x$filenames, 3, 6)) - 1
  nam = paste0("FACTDATA_SEP", year, ".TXT")
  unzip(paste0("C:/workspace2/", x2), files = nam)
  })

# reading
# staff <- read.csv("FACTDATA_SEP2017.TXT", stringsAsFactors = FALSE)
# names(staff) <- tolower(names(staff))

test <- list.files()
test <- data.frame(files = test[grepl("FACTDATA", test)], stringsAsFactors = FALSE)
test$fy <- as.numeric(substr(test$files, 13, 16))
staff <- {
  split(test, test$fy) ->.;
  lapply(., function(x) {
  cat("reading", x$files, as.character(Sys.time()), "\n")
  temp = read.csv(x$files, stringsAsFactors = FALSE)
  temp$fy = x$fy + 1
  # subset soil scientists and USDA employees
  temp = subset(temp, OCC == "0470" | grepl("^AG", AGYSUB))
  return(temp)
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

# 0470 Series
gg_ss <- filter(staff2, occ == "0470") %>%
  group_by(fy, agysub) %>%
  summarize(n = length(occ)) %>%
  arrange(- n) %>%
  ggplot(aes(x = fy, y = n, col = agysub)) +
  geom_line(lwd = 2) +
  ylab("# of soil scientists") + xlab("fiscal year") +
  ggtitle("0470 Series in the Federal Government")


# SS as a share of NRCS
gg_nrcs <- filter(staff2, grepl("NATURAL RESOURCES CON", agysub)) %>%
  mutate(soil_scientist = occ == "0470") %>%
  group_by(fy, agysub, soil_scientist) %>%
  summarize(n = length(occ)
            ) %>%
  arrange(- n) %>%
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


test %>%
  ggplot(aes(y = n, x = ppgrd)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ fy) +
  coord_flip() + 
  ggtitle("Age Brackets of the 0470 Series (NRCS 01 + 02)")

# NRCS in IN
Title <- "Midwest NRCS"
staff2 %>%
  filter(grepl("IOWA|MISSOURI|WISCONSIN|INDIANA|ILLINOIS|MINNESOTA|OHIO|NORTH DAKO", loc) & grepl("NATURAL RESOURCES CON", agysub)) %>%
  group_by(loc, fy) %>%
  summarize(n = length(loc)) %>%
  ggplot(aes(x = fy, y = n, col = loc)) +
  geom_line(lwd = 1.5) +
  ylab("# of employees") + xlab("fiscal year") +
  ggtitle(Title)

