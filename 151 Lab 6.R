PErisk <- read.csv('https://raw.githubusercontent.com/umbertomig/qtm151/main/datasets/PErisk.csv') 
band <- tribble( ~name,  ~band,
  "Mick",  "Stones",
  "John", "Beatles",
  "Paul", "Beatles")

instrument <- tribble( ~name,   ~plays,
   "John", "guitar",
   "Paul",   "bass",
  "Keith", "guitar")

instrument2 <- tribble(~artist,   ~plays,
   "John", "guitar",
   "Paul",   "bass",
  "Keith", "guitar")

?tribble
band <- tribble(~name, ~band, "Mick", "Stones", "John", "Beatles", "Paul", "Beatles")
instrument <- tribble(~name, ~plays, "John", "guitar", "Paul", "bass", "Keith", "guitar")

left_join(band, instrument) #matches to elements of band
right_join(band,instrument) #matches to elements of instrument (right element)
inner_join(band, instrument) #only what is in both datasets
full_join(band, instrument) #preserves all data in both datasets, filling NAs accordingly

semi_join(x,y, by='') #keeps values in x that have a match in y
semi_join(band, instrument)
filter(band, name %in% instrument)

anti_join(x,y, by='') #gives values in x that are not in y
anti_join(band, instrument)

?dplyr
?select
