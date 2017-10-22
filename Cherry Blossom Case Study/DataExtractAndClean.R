#### R Source File for Prepping Data for Analysis ####

# Set working directory (only works when sourcing this R file)
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Review example race logs
els = readLines("MenTxt/2012.txt")
els[1:10] #review first 10 rows

els2011 = readLines("MenTxt/2011.txt")
els2011[1:10]

# Identify line index for header-data break
eqIndex = grep("^===", els)
eqIndex

# Discard rows above header name line
spacerRow = els[eqIndex]
headerRow = els[eqIndex - 1]
body = els[ -(1:eqIndex) ]

# Extract runners' age
headerRow = tolower(headerRow) #Make all headers lower case since different years use different cases

#ageStart = regexpr("ag", headerRow) #Identify age column location
#ageStart

#age = substr(body, start = ageStart, stop = ageStart + 1) #Extract age values
#head(age)

#summary(as.numeric(age))

# Find column locations
findColLocs = function(spacerRow) {
    #Find locations of all blanks in line of '=' characters
    spaceLocs = gregexpr(" ", spacerRow)[[1]]
    rowLength = nchar(spacerRow)
    
    #Extract all the columns
    if (substring(spacerRow, rowLength, rowLength) != " ")
        return( c(0, spaceLocs, rowLength + 1))
    else return(c(0, spaceLocs))
}

# Extract columns
selectCols = function(colNames, headerRow, searchLocs) {
    sapply(colNames, 
           function(name, headerRow, searchLocs)
           {
               startPos = regexpr(name, headerRow)[[1]]
               if (startPos == -1) 
                   return( c(NA, NA) )
               
               index = sum(startPos >= searchLocs)
               c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
}

# Test findColLocs and selectCols functions
searchLocs = findColLocs(spacerRow)
ageLoc = selectCols("ag", headerRow, searchLocs) 
ages = mapply(substr, list(body), 
              start = ageLoc[1,], stop = ageLoc[2, ])

summary(as.numeric(ages))

# Create shortened column identifiers and account for when some tables missing columns
#shortColNames = c("name", "home", "ag", "gun", "net", "time")
#
#locCols = selectCols(shortColNames, headerRow, searchLocs)
#
#Values = mapply(substr, list(body), start = locCols[1, ], 
#                stop = locCols[2, ])
#
#class(Values)
#
#colnames(Values) = shortColNames
#head(Values)
#
#tail(Values)[ , 1:3]

# Build wrapper function for column extraction
extractVariables = 
    function(file, varNames =c("name", "home", "ag", "gun",
                               "net", "time"))
    {
        # Find the index of the row with =s
        eqIndex = grep("^===", file)
        # Extract the two key rows and the data
        spacerRow = file[eqIndex] 
        headerRow = tolower(file[ eqIndex - 1 ])
        body = file[ -(1 : eqIndex) ]
        
        # Obtain the starting and ending positions of variables
        searchLocs = findColLocs(spacerRow)
        locCols = selectCols(varNames, headerRow, searchLocs)
        
        Values = mapply(substr, list(body), start = locCols[1, ], 
                        stop = locCols[2, ])
        colnames(Values) = varNames
        
        invisible(Values)
    }

# Read table lines into R
mfilenames = paste("MenTxt/", 1999:2012, ".txt", sep = "")
menFiles = lapply(mfilenames, readLines)
names(menFiles) = 1999:2012

# Create list of character matrices containing the column contents for each of the 14 years of data
menResMat = lapply(menFiles, extractVariables)
length(menResMat)

sapply(menResMat, nrow) #Review row counts for each matrix


# Create numeric age variable
age = as.numeric(menResMat[['2012']][ , 'ag'])

tail(age)

age = sapply(menResMat,
             function(x) as.numeric(x[ , 'ag']))

boxplot(age, ylab = "Age", xlab = "Year")

head(menFiles[['2003']])

menFiles[['2006']][2200:2205]

# Update selecCols to account for offset age values in age column
selectCols = function(shortColNames, headerRow, searchLocs) {
    sapply(shortColNames, function(shortName, headerRow, searchLocs){
        startPos = regexpr(shortName, headerRow)[[1]]
        if (startPos == -1) return( c(NA, NA) )
        index = sum(startPos >= searchLocs)
        c(searchLocs[index] + 1, searchLocs[index + 1])
    }, headerRow = headerRow, searchLocs = searchLocs )
}

menResMat = lapply(menFiles, extractVariables) #Re-run data extraction

age = sapply(menResMat, function(x) as.numeric(x[ , 'ag']))
boxplot(age, ylab = "Age", xlab = "Year") #Re-plot age data

# Observe NA values present in age data
sapply(age,  function(x) sum(is.na(x)))

age2001 = age[["2001"]]

grep("^===", menFiles[['2001']])

badAgeIndex = which(is.na(age2001)) + 5
menFiles[['2001']][ badAgeIndex ]

badAgeIndex

# Update extractVariables to account for missing age data
extractVariables = function(file, varNames =c("name", "home", "ag", "gun", "net", "time"))
    {
        # Find the index of the row with =s
        eqIndex = grep("^===", file)
        # Extract the two key rows and the data 
        spacerRow = file[eqIndex] 
        headerRow = tolower(file[ eqIndex - 1 ])
        body = file[ -(1 : eqIndex) ]
        # Remove footnotes and blank rows
        footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
        if ( length(footnotes) > 0 ) body = body[ -footnotes ]
        blanks = grep("^[[:blank:]]*$", body)
        if (length(blanks) > 0 ) body = body[ -blanks ]
        
        
        # Obtain the starting and ending positions of variables   
        searchLocs = findColLocs(spacerRow)
        locCols = selectCols(varNames, headerRow, searchLocs)
        
        Values = mapply(substr, list(body), start = locCols[1, ], 
                        stop = locCols[2, ])
        colnames(Values) = varNames
        
        return(Values)
    }

menResMat = lapply(menFiles, extractVariables) #Re-run data extraction

# Look for additional age errors
which(age2001 < 5)

menFiles[['2001']][ which(age2001 < 5) + 5 ]

# Split and process times
convertTime = function(time) {
    timePieces = strsplit(time, ":")
    timePieces = sapply(timePieces, as.numeric)
    sapply(timePieces, function(x) {
        if (length(x) == 2) x[1] + x[2]/60
        else 60*x[1] + x[2] + x[3]/60
    })
}

# Apply character matrices in menResMat and return a dataframe with variables
createDF = function(Res, year, sex) 
{
    # Determine which time to use
    if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
    else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
    else useTime = Res[ , 'time']
    
    # Remove # and * and blanks from time
    useTime = gsub("[#\\*[:blank:]]", "", useTime)
    runTime = convertTime(useTime[ useTime != "" ])
    
    # Drop rows with no time
    Res = Res[ useTime != "", ]
    
    Results = data.frame(year = rep(year, nrow(Res)),
                         sex = rep(sex, nrow(Res)),
                         name = Res[ , 'name'], home = Res[ , 'home'],
                         age = as.numeric(Res[, 'ag']), 
                         runTime = runTime,
                         stringsAsFactors = FALSE)
    invisible(Results)
}

#menDF = mapply(createDF, menResMat, year = 1999:2012, sex = rep("M", 14), SIMPLIFY = FALSE)

#sapply(menDF, function(x) sum(is.na(x$runTime)))

# Fix missing runTime data issues for 2006
separatorIdx = grep("^===", menFiles[["2006"]])
separatorRow = menFiles[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
menFiles[['2006']][separatorIdx] = separatorRowX

menResMat = sapply(menFiles, extractVariables)
menDF = mapply(createDF, menResMat, year = 1999:2012, sex = rep("M", 14), SIMPLIFY = FALSE)

boxplot(sapply(menDF, function(x) x$runTime), xlab = "Year", ylab = "Run Time (min)") #View run times after 2006 correction

# Combine men's race results across all years
cbMen = do.call(rbind, menDF)
dim(cbMen)
