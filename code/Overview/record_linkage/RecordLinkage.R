setwd('~/Desktop/OReillyHealthcareData/code/Overview/record_linkage/')

# DATA PREP

require(RecordLinkage)
require(data.table)

online.data <- fread('online_data.csv')
neighborhood.data <- fread('neighborhood_data.csv')
data <- fread('data.csv')

# convert to caps
neighborhood.data <- as.data.frame(sapply(neighborhood.data, toupper))
online.data <- as.data.frame(sapply(online.data, toupper))
data <- as.data.frame(sapply(data, toupper))

# remove punctuation
data[, c("firstname", "surname")] <- as.data.frame(sapply(data[, c("firstname", "surname")], function(x) gsub("[[:punct:]]", "", x)))
neighborhood.data[, c("firstname", "surname")] <- as.data.frame(sapply(neighborhood.data[, c("firstname", "surname")], function(x) gsub("[[:punct:]]", "", x)))
online.data[, c("firstname", "surname")] <- as.data.frame(sapply(online.data[, c("firstname", "surname")], function(x) gsub("[[:punct:]]", "", x)))

# remove whitespace
data[, c("firstname", "surname")] <- as.data.frame(sapply(data[, c("firstname", "surname")], function(x) gsub(" ", "", x)))
neighborhood.data[, c("firstname", "surname")] <- as.data.frame(sapply(neighborhood.data[, c("firstname", "surname")], function(x) gsub(" ", "", x)))
online.data[, c("firstname", "surname")] <- as.data.frame(sapply(online.data[, c("firstname", "surname")], function(x) gsub(" ", "", x)))

# empirically, typos occur more frequently later in words than earlier, so we 'block' on first letter of first and last names
data$initials <- paste(substring(data$firstname,1,1), substring(data$surname, 1, 1), sep = '')
neighborhood.data$initials <- paste(substring(neighborhood.data$firstname,1,1), substring(neighborhood.data$surname, 1, 1), sep = '')
online.data$initials <- paste(substring(online.data$firstname,1,1), substring(online.data$surname, 1, 1), sep = '')


#DETERMINISTIC HEURISTIC RECORD LINKAGE

# if first names and birthdate matche
combined.data.using.firstname <- merge(data, online.data, by = c("firstname", "birthdate"))
combined.data.using.firstname$surname <- combined.data.using.firstname$surname.x

# if last name and birthdate matches
combined.data.using.surname <- merge(data, online.data, by = c("surname", "birthdate"))
combined.data.using.surname$firstname <- combined.data.using.surname$firstname.x

# combine both options (since it's either or)
combined.data <- rbind(combined.data.using.firstname[, c("firstname", "surname", "birthdate", "zip_codes")], combined.data.using.surname[, c("firstname", "surname", "birthdate", "zip_codes")])
dim(combined.data)
combined.data <- unique(combined.data)
dim(combined.data)

# if initials match plus birthday is within 1 week
online.data <- online.data[1:1000, ] # to keep it small
all_combos <- merge(data, online.data, by = "initials", allow.cartesian = TRUE)
all_combos <- as.data.table(all_combos)
all_combos$birthdate.x <- as.Date(all_combos$birthdate.x, format = "%m/%d/%y")
all_combos$birthdate.y <- as.Date(all_combos$birthdate.y, format = "%m/%d/%y")

# if you look, some of the birthdays are in the future!
# not a problem for our application, but otherwise you need to 

all_combos$birthday.delta <- all_combos$birthdate.x - all_combos$birthdate.y
all_combos$birthday.delta <- as.numeric(all_combos$birthday.delta)
dim(all_combos)

all_combos <- all_combos[all_combos$birthday.delta < 7 & all_combos$birthday.delta > -7, ]
dim(all_combos)

all_combos <- all_combos[with(all_combos, order(surname.x, firstname.x)), ]
dim(all_combos)
dim(unique(all_combos[, c('firstname.x', 'surname.x'), with = FALSE]))
all_combos[all_combos$surname.x == 'BROWN', ]

dim(all_combos)
dim(unique(all_combos[, .(firstname.x, surname.x)]))


#PROBABILISTIC RECORD LINKAGE
# for a thorough primer, see https://www.ncbi.nlm.nih.gov/books/NBK253312/

# create comparison vectors for pairs for data and neighborhood data 
comp <- compare.linkage(data[, -3], neighborhood.data, blockfld = c("zip_codes", "initials"), strcmp <- TRUE)
print(head(comp$pairs))

# calculate the agreement weights based on comparison vectors
# getting an idea of the distribution of the weights (should be bimodal) already gives an idea of a 'good' weight to use
em.result <- emWeights(comp, cutoff = 0.8)
summary(em.result)

# look at pairings to get an idea of good threashhold
neighborhood.pairs <- getPairs(em.result)
head(neighborhood.pairs)
neighborhood.pairs[1:10,]


# finalize pairings
final.pairs <- getPairs(em.result, max.weight = 5.5, min.weight = 4)
head(final.pairs)
tail(final.pairs)


