#' Title: Get Text : XML example
#' Purpose: Grab some text from an XML file for instruction
#' Author: Ted Kwartler
#' email: ehk116@gmail.com
#' License: GPL>=3
#' Date: 2018-4-24
#' 

# Libs
library(xml2)
library(stringr)

# In Chrome, press f12 to open the developer tab. 
# Reload the page with closed caption turned on.
# In the dev tab search box type "timed" to get the caption info
# Right-click and open in a new tab to view XML captions.
# https://www.youtube.com/watch?v=34Na4j8AVgA
url<-'https://www.youtube.com/api/timedtext?sparams=caps%2Cv%2Cexpire&caps&key=yttt1&signature=059D3B7FA3B20598FFE1844F1402A2750A792E66.7959B2B2AF044DBB87E613037AD122AD851E8BF3&v=34Na4j8AVgA&expire=1529181671&hl=en_US&lang=en&fmt=srv3'

# Read in the closed caption info
x<-read_xml(url)

# Extract text, remove carriage returns, remove special characters
text<-xml_text(x)
text<-str_replace_all(text, "[\r\n]" , "")
text<-iconv(text, "latin1", "ASCII", sub="")

# Save
writeLines(text,'~/Weeknd.txt')

# End
