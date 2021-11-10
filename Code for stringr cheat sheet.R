library(stringr)

# MATCH CHARACTERS

see <- function(rx) str_view_all("abc ABC 123\t.!?\\(){}\n", rx)
see <- function(rx) str_view("abc ABC 123\t.!?\\(){}\n", rx)

see("a")
see("\\.")
see("\\!")
see("\\?")
see("\\\\") 
see("\\(")
see("\\)")
see("\\{")
see( "\\}")
see("\\n")
see("\\t")
see("\\s")
see("\\d")
see("\\w")
see("\\b")
see("[:digit:]")
see("[:alpha:]")
see("[:lower:]")
see("[:upper:]")
see("[:alnum:]")
see("[:punct:]")
see("[:graph:]")
see("[:space:]")
see("[:blank:]")
see(".")


# ALTERNATES

alt <- function(rx) str_view_all("abcde", rx)

alt("ab|d") #or
alt("[abe]") #one of
alt("[^abe]") #anything but
alt("[a-c]") #range


# QUANTIFIERS

quant <- function(rx) str_view_all(".a.aa.aaa", rx)

quant("a?") #zero or one
quant("a*") #zero or more
quant("a+") #one or more
quant("a{2}") #exactly n
quant("a{2,}") #n or more
quant("a{2,4}") #between n and m q


# ANCHORS

anchor <- function(rx) str_view_all("aaa", rx)

anchor("^a") #start of string
anchor("a$") #end of string


# LOOK AROUNDS

look <- function(rx) str_view_all("bacad", rx)

look("a(?=c)") #followed by
look("a(?!c)") #not followed by
look("(?<=b)a") #preceded by 
look("(?<!b)a") #not preceded by 


# GROUPS

ref <- function(rx) str_view_all("abbaab", rx)

alt("(ab|d)e")
ref("(a)(b)\\2\\1")
