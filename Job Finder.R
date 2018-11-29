library(httr)
library(RCurl)
library(rvest)
library(XML)
library(xml2)
library(curl)

#curs <- read.csv("/home/james/Documents/OccupationScraper/Indeed.csv")
#burl + a search term + locvw + radius argument

 dtcl <- read.csv("/home/james/Documents/OccupationScraper/DTClist.csv", stringsAsFactors = FALSE)
 dtc <- dtcl$x
#dtc <- as.character()

burl <- "https://www.indeed.com/jobs?q="
search_terms <- c("r+programming",
                  "data+analyst+R",
                  "data+analyst",
                  "Research+Analyst",
                  "Quantitative+analyst+R",
                  "R+Shiny",
                  "Data+Engineer",
                  "Urban+Informatics")
locvw <- "&l=Boston%2C+MA"
rad <- "&radius=5"
ft <- "&jt=fulltime"
pss <- "&start="
#pagination is start = 0, then 10, then 20, etc.
plks <- data.frame(Term = as.character(),
                   Link = as.character()
                   )
for (z in 1:length(search_terms)){
  print(paste("Looking for", paste('"', search_terms[z], '"', sep = ""), "jobs..."))
for (t in seq(from = 0, to = 100, by = 10)){
durl <- paste(burl, search_terms[z], locvw, rad, ft, pss, t, sep = "")

#Get posting links

Sys.sleep(sample(1:5, 1))
fp <- read_html(durl)

lks <- fp %>% 
  html_nodes("#resultsBody") %>% 
  html_nodes("#pageContent") %>% 
  html_nodes("#resultsCol") %>%
  html_children() %>% 
  .[grep("jobsearch-SerpJobCard",.)] %>%
  html_children() %>%
  html_children() %>%
  html_attr("href") %>% 
  .[grep("/rc/clk",.)] %>% 
  paste("https://www.indeed.com", ., sep = "")
print(paste("Finished with page", which(seq(from = 0, to = 100, by = 10) == t), "of", length(seq(from = 0, to = 100, by = 10))))
if (length(lks[!(lks %in% plks$Link)]) == 0){
  "Nothing new on this page..."
  next
} else{
  lks <- lks[!(lks %in% plks$Link)]
  glk <- data.frame(Term = search_terms[z],
                    Link = lks)
  plks <- rbind(plks, glk)
}


} #END pagination loop for link gathering
} #END seach term loop
#plks <- plks[!(plks$Link %in% curs$Link),]

if (nrow(plks) == 0){
  print("There are no new job listings")
} else {
plks <- unique(plks)

jbsx <- data.frame("Position"= as.character(),
                  "Company" = as.character(),
                  "Posting_Text" = as.character(),
                  "Link" = as.character(),
                  "Term" = as.character(),
                  "FIT" = as.numeric())


for (j in 1:nrow(plks)){
  Sys.sleep(sample(1:2,1))
  lk <- read_html(as.character(plks$Link[j]))

  Job.Title <-   lk %>% 
    html_children() %>% 
    html_children() %>% 
    html_nodes("div") %>% 
    html_nodes(".icl-u-xs-mb--xs.icl-u-xs-mt--none.jobsearch-JobInfoHeader-title") %>% 
    html_text()
  if (length(grep("intern", Job.Title, ignore.case = TRUE)) > 0){
    print(paste("Listing", j, "is an internship"))
    next
  }
  if (length(grep("co-op", Job.Title, ignore.case = TRUE)) > 0){
    print(paste("Listing", j, "is a co-op"))
    next
  }
  if (length(grep("biostat", Job.Title, ignore.case = TRUE)) > 0){
    print(paste("Listing", j, "is a biostats job"))
    next
  }
  if (length(grep(" fellow", Job.Title, ignore.case = TRUE)) > 0){
    print(paste("Listing", j, "is a fellowship"))
    next
  }
  
  Job.Company <-   lk %>% 
    html_children() %>% 
    html_children() %>% 
    html_nodes("div") %>% 
    html_nodes(".icl-u-lg-mr--sm.icl-u-xs-mr--xs") %>% 
    html_text() %>% 
    .[1]
  
  if(as.character(Job.Company) %in% dtc){
    print(paste("Listing", j, "is a dtc"))
    next
  }
  
  JDSearch <- lk %>% 
    html_children() %>%
    html_children() %>%
    html_nodes("div") %>%
    html_nodes("p")
  
  # grep("experience", JDSearch, ignore.case = TRUE)
  # JDSearch[9]
  # reqnum <- min(grep("requirement", JDSearch, ignore.case = TRUE))
  # 
  # JDSearch[reqnum:length(JDSearch)]
  # grep("require", JDSearch, ignore.case = TRUE)
  
  Job.Desc <-  lk %>% 
    html_children() %>%
    html_children() %>%
    html_nodes("div") %>%
    html_nodes("p") %>%
    html_text(trim = TRUE) %>% 
    unlist() %>% 
    paste(collapse = "\n") %>% 
    as.character()
  #With the way this is captured, it includes "\n" characters at 
  #the paragraph breaks. If you cat() the text it looks fine and
  #if you use strsplit() later on, you can get them seperated again
  
  #Check to see if that method actually grabbed anything.
  #Sometimes the description is just a set of divs without the p node
  #need to get Job.Desc and then check if nchar() == 0
  #If so, then apply the other, div oriented approach to get job descriptions
  
  #lk <- read_html("https://www.indeed.com/viewjob?jk=f38df3b3346dc90f&from=serp&vjs=3")
  if (nchar(Job.Desc) == 0){
    Job.Desc <-  lk %>%
      html_nodes(".jobsearch-ViewJobLayout.jobsearch-ViewJobLayout-changeTextSize.jobsearch-ViewJobLayout-changeTextColor") %>% 
      html_nodes(".icl-Container--fluid.icl-u-xs-p--sm") %>% 
      html_nodes(".icl-Grid.jobsearch-ViewJobLayout-content.icl-u-lg-mt--md") %>% 
      html_nodes(".jobsearch-ViewJobLayout-innerContent.icl-Grid-col.icl-u-xs-span12.icl-u-lg-offset2.icl-u-lg-span10") %>% 
      html_nodes(".jobsearch-ViewJobLayout-jobDisplay.icl-Grid-col.icl-u-xs-span12.icl-u-lg-span7") %>% 
      html_nodes(".jobsearch-JobComponent-description.icl-u-xs-mt--md") %>% 
      html_text(trim = TRUE) %>% 
      unlist() %>% 
      paste(collapse = "\n") %>% 
      as.character()
    }
  
if(length(grep("drug test", Job.Desc)) != 0){
  dtc <- c(dtc, as.character(Job.Company))
  print(paste("Listing", j, "is a new dtc"))
  next
}  
  if(length(grep("drug screening", Job.Desc)) != 0){
    dtc <- c(dtc, as.character(Job.Company))
    print(paste("Listing", j, "is a new dtc"))
    next
  } 
  
  
 jbs <- data.frame("Position"= Job.Title,
                   "Company" = Job.Company,
                   "Posting_Text" = Job.Desc,
                   "Link" = plks$Link[j],
                   "Term" = plks$Term[j],
                   "FIT" = NA)
 jbsx<- rbind(jbsx, jbs)
 print(paste("Finished with listing", j, "of", nrow(plks)))
}

wris <- jbsx
#wris <- rbind(curs, jbsx)
#write.csv(wris, "/home/james/Documents/OccupationScraper/Indeed.csv", row.names = FALSE)

if (nrow(dtcl) < length(dtc)){
write.csv(data.frame(x=dtc), "/home/james/Documents/OccupationScraper/DTClist.csv", row.names = FALSE)
}
print(paste("Finished scraping", nrow(jbsx), "jobs"))
} #END Else statement that stops the script from running if there are no new links









#wris$FIT <- NA
for (t in 1:nrow(wris[which(is.na(wris$FIT) == TRUE),])){
browseURL(as.character(wris$Link[which(is.na(wris$FIT) == TRUE),][t]))
  cv <- menu(choices = c("Y", "N"), title = paste("Is", wris$Position[which(is.na(wris$FIT) == TRUE),][t], "at", wris$Company[which(is.na(wris$FIT) == TRUE),][t], "a good fit?"))
  wris$FIT[which(is.na(wris$FIT) == TRUE),][t] <- cv
}

length(wris$Link)
length(unique(wris$Link))
nrow(unique(wris))

nchar(as.character(wris$Posting_Text[1]))
wris[which(nchar(as.character(wris$Posting_Text)) == 0),]

#N-Gram Tests
library(ngram)
library(tidyr)
library(tidytext)
library(dplyr)

#Training Data

Trd <- data.frame("Text" = as.character(wris$Posting_Text[1:nrow(wris)/2])) 

tks <- unnest_tokens(tbl = Trd, output = bigram, input = "Text", token = "ngrams", n = 2) %>% 
  count(bigram, sort =  TRUE)

tks3 <- unnest_tokens(tbl = Trd, output = trigram, input = "Text", token = "ngrams", n = 4) %>% 
  count(trigram, sort =  TRUE)

wks3 <- unnest_tokens(tbl = Trd, output = trigram, input = "Text", token = "sentences") %>% 
  count(trigram, sort =  TRUE)

wks3[grep("language", wks3$trigram, ignore.case = TRUE),]

wks3$trigram[grep("master", wks3$trigram, ignore.case = TRUE)] %>% 
  grep("public policy", ., ignore.case = TRUE)

wks3[grep("required experience", wks3$trigram, ignore.case = TRUE),]

tl <- wris[,c(1:3, 5)]
tl$Posting_Text <- as.character(tl$Posting_Text)
book_words <- unnest_tokens(tbl = tl, output = wrds, input = Posting_Text, token = "words") %>%
  count(Company, wrds, sort = TRUE) %>%
  ungroup()

cd <- unnest_tokens(tbl = tl, output = wrds, input = Posting_Text, token = "ngrams", n = 2) %>% 
  separate(wrds, c("word1", "word2"), sep = " ")
  
scd <- cd %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) #%>% 
  # count(word1, word2, sort = TRUE)

scd %>% 
  filter(word1 == "vacation") %>% 
  count(Position, word2, sort = TRUE)



wks3$trigram[grep("@", wks3$trigram, ignore.case = TRUE)]


wks3 <- unnest_tokens(tbl = Trd, output = trigram, input = "Text", token = "sentences") %>% 
  separate(trigram, c("Responsibilities", "Qualifications"), sep = "") %>% 
  na.omit()




tks3[grep("language", tks3$trigram, ignore.case = TRUE),]


#Test Data (for later)

Tsd <- data.frame("Text" = as.character(wris$Posting_Text[(nrow(wris)/2+1):nrow(wris)]))

#Ngram test of training data

Trd[2,] %>% 
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)

nrow(wris)/2





###Set up some sort of email alerts system that will at least
###get me the statistics of this job, ie number of jobs,
###average salary, and how many are in which zip or town, etc.