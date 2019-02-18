#Install required packages
install.packages('rvest')

#Import rvest library
library('rvest')

#Scrape Top 100 from Billboard
   #Access top 100 website
	hot100url <- "https://www.billboard.com/charts/hot-100"
	hot100 <- read_html(hot100url)
   #Scrape html list and input into an array/list
	titles_html <- html_nodes(hot100, '.chart-row__title')
	titles_text <- html_text(titles_html)
   #Clean titles array
	for (i in 1:length(titles_text)) {
		titles_text[i] <- substr(titles_text[i], 2, nchar(titles_text[i])-2)
		titles_text[i] <- gsub("\n\n", " // ", titles_text[i])
		titles_text[i] <- gsub("\n \n", " / ", titles_text[i])
	}

#Scrape lyrics
   #Create an array of url endings for Genius
	titles_urls <- vector(mode = "character", length = length(titles_text))
	for (j in 1:length(titles_text)) {
		name <- substr(titles_text[j], 0, regexpr("/", titles_text[j])-2)
		artist <- paste(substr(titles_text[j], regexpr("/", titles_text[j])+3, nchar(titles_text[j])), " ", sep="")
		artist <- substr(artist, 0, regexpr(" ", artist)-1)
		searchphrase <- paste(name, artist, "lyrics genius")
		searchphrase <- gsub("'", "", searchphrase)
		searchphrase <- gsub("&", "and", searchphrase)
		searchphrase <- gsub(" ", "+", searchphrase)
		titles_urls[j] <- paste("https://www.google.com/search?q=", searchphrase, sep="")
	}
	
   #Loop through array THIS DOES WORK, BUT NEEDS TO BE CLEANED
	song_url <- "https://genius.com/Ed-sheeran-and-beyonce-perfect-duet-lyrics"
	song <- read_html(song_url)
	song_html <- html_nodes(song, '.lyrics')
	song_text <- html_text(song_html)
	song_text <- gsub('([[:upper:]])', ' \\1', song_text)
	song_text <- gsub( " *\\[.*?\\] *", "", song_text)
