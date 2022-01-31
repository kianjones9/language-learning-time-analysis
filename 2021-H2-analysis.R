library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(tidyverse)

# ETL
df <- read.csv(file = "2021-H2.csv")
df$Description = tolower(df$Description)

df[df$Description == "talking with friends", "Description"] = "friends"
df[df$Description == "textbook work", "Description"] = "textbook"


# Create lookup table for activity skills colours

anki <- c("vocabulary")
italki <- c("speaking", "listening")
lingq <- c("vocabulary", "reading")
mango <- c("listening", "vocabulary", "grammar")
netflix <- c("listening")
podcast <- c("listening")
friends <- c("listening", "speaking")
textbook <- c("grammar")

act_skill_map = structure(list(), anki = anki, italki = italki, lingq = lingq, mango = mango, netflix = netflix, podcast = podcast, friends = friends, textbook = textbook)
print(attributes(act_skill_map))


activities <- data.frame(name = c("anki", "italki", "lingq", "mango", "netflix", "podcast", "friends", "textbook"),
                         listening = 0, speaking = 0, grammar = 0, reading = 0, vocabulary = 0)

# Graph time spent doing activities which develop different skills
skills = c("listening", "reading", "speaking", "grammar", "vocabulary")

for(activity in activities$name) {
  for(skill in attr(x = act_skill_map, which = activity)) {
    activities[activities$name == activity, skill] = 1
  }
}

# Graph time spent performing each activity
by_desc <- df %>% group_by(Description)

by_desc <- summarise(by_desc,
  Duration = sum(period_to_seconds(hms(Duration)))
)


yLabels1 <- function(x)
{
  round(x/3600)
}


colors = c("#0059b1","#4fce5d", "#ff4338", "#338fff", "#f6883b", "#e50914", "#1db954", "#008575")

ggplot(by_desc, aes(x=Description, y=Duration)) + geom_bar(position=position_dodge(), stat="identity", fill=colors) + scale_y_continuous(labels = yLabels1)

time_per_skill <- data.frame(skill = skills, time = 0)

for (activity in activities$name) {
  for(skill in attr(x = act_skill_map, which = activity)) {
    current_time <- time_per_skill[time_per_skill$skill == skill, "time"]
    time_to_add <- by_desc[by_desc$Description == activity, "Duration"]

    
    time_per_skill[time_per_skill$skill == skill, "time"] = current_time + time_to_add
  }
}

lbls <- time_per_skill$skill
pct <- round(time_per_skill$time/sum(time_per_skill$time)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels

pie(time_per_skill$time, labels=lbls)


# ETL
df <- read.csv(file = "2021-H2-skip-first.csv")
df$Description = tolower(df$Description)

df[df$Description == "talking with friends", "Description"] = "friends"
df[df$Description == "textbook work", "Description"] = "textbook"


# Groupby logically groups, but you have to give
# summarize directive explain how to group other cells

df$Start.date = as_datetime(df$Start.date)

sum_period_as_seconds <- function (x) {
  sum(period_to_seconds(hms(x)))
}

time_per_day = aggregate(df$Duration, list(df$Start.date), sum_period_as_seconds)

time_per_day$Total.time = cumsum(time_per_day$x)

colnames(time_per_day) <- c("Date", "Hours Per Day", "Hours Studied")

yLabels2 <- function(x)
{
  round(x/3600) + 22
}

plot <- ggplot(time_per_day, aes(Date, `Hours Studied`)) +
  geom_line(position=position_dodge(), stat="identity") +
  scale_y_continuous(labels=yLabels2) +
  geom_segment(aes(x=as_datetime("2021-08-23"), y=period_to_seconds(hours(0)), xend=as_datetime("2021-12-31"), yend=period_to_seconds(hours(128))))

plot

time_per_day <- time_per_day %>%
  complete(Date = seq(Date[1], Date[length(Date)], by = "1 day"),
           fill = list(`Hours Per Day` = 0, `Hours Studied` = 0))

time_per_day

time_per_day$avgTime = round(3600 * (128/131))

time_per_day$cumAvg = cumsum(time_per_day$avgTime)



time_per_day$ahead = ifelse(time_per_day$`Hours Studied` > time_per_day$cumAvg, TRUE, FALSE)

time_per_day$seqLen = 0
seqLens = c()
days = c()

for (i in 2:length(time_per_day$ahead)-1){
  if (time_per_day$ahead[i + 1] == time_per_day$ahead[i]) {
    time_per_day$seqLen[i + 1] = time_per_day$seqLen[i] + 1
  } else {
    time_per_day$seqLen[i + 1] = 0
    seqLens = append(seqLens, time_per_day$seqLen[i])
    days = append(days, time_per_day$Date[i])
  }
}

seqLens = append(seqLens, tail(time_per_day$seqLen, n=1))
seqLens
# 
days
critPoints = c()
for (day in days) {
  # print(time_per_day[time_per_day$Date == day, "cumAvg"])
  critPoints <- append(critPoints, time_per_day[time_per_day$Date == day, "cumAvg"]$cumAvg)
}
critPoints

for (i in 1:length(days)) {
  plot <- plot + annotate("point", x=days[i],y=critPoints[i], col="red")
}
plot
