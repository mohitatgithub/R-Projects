#Amazon Review Case Study
#Note: We have divided whole analysis in Steps mentioned in comments, all questions are answered at end using analysis results from these steps
#Data Source: Data taken from http://jmcauley.ucsd.edu/data/amazon/ for product categories Movies and TV, CDs and Vinyl, Kindle Store

#Load SparkR
library(SparkR)
library(ggplot2)

#Initiating the spark session
sc = sparkR.session(master='local')

#Creating Spark DataFrames by reading json files from S3 bucket
cd_vinyl_reviews <- SparkR::read.df("s3://mohitcasestudy/Cds_and_Vinyl/reviews_CDs_and_Vinyl_5.json", "json")
kindle_reviews <- SparkR::read.df("s3://mohitcasestudy/Kindle_Store/reviews_Kindle_Store_5.json",  "json")
movie_reviews <- SparkR::read.df("s3://mohitcasestudy/Movies_and_TV/reviews_Movies_and_TV_5.json",  "json")

#Creating Spark DataFrames
cd_vinyl_reviews
kindle_reviews
movie_reviews

#Checking dataset and its structure
head(cd_vinyl_reviews)
head(kindle_reviews)
head(movie_reviews)
str(movie_reviews) #helpful is array, overall & unixReviewTime are numeric & remaining are character type

#------------------------------------------------------------------------------------------------------------------------------------------------------
##Step 1: Finding distinct number of reviewers & their percentage in each product category
distinct_kindle_reviewers <- count(groupBy(kindle_reviews, kindle_reviews$reviewerID))
head(distinct_kindle_reviewers)
nrow(kindle_reviews) #982619
nrow(distinct_kindle_reviewers) #68223 distinct reviewers out of total 982619(6.82%)

distinct_cd_vinyl_reviewers <- count(groupBy(cd_vinyl_reviews, cd_vinyl_reviews$reviewerID))
head(distinct_cd_vinyl_reviewers)
nrow(cd_vinyl_reviews) #1097592
nrow(distinct_cd_vinyl_reviewers) #75258 distinct reviewers out of total 1097592(6.85%)

distinct_movies_tv_reviewers <- count(groupBy(movie_reviews, movie_reviews$reviewerID))
head(distinct_movies_tv_reviewers)
nrow(movie_reviews) #1697533
nrow(distinct_movies_tv_reviewers) #123960 distinct reviewers out of total 1697533(7.3%) 

#Observations: Based on above Movies category have largest number of distinct reviewers compared to cd-vinyl & kindle products

#Lets investigate this further by checking number of distinct reviewers ratio accross product categories:
#a) Kindle vs Movies
nrow(distinct_kindle_reviewers)/nrow(distinct_movies_tv_reviewers) #68223/123960=.05503, So Movies have almost twice reviewers compared to kindle
#b) Kindle vs Cd-Vinyl
nrow(distinct_kindle_reviewers)/nrow(distinct_cd_vinyl_reviewers) #68223/75258=.9065, So there is not much difference between kindle & cd-vinly reviewer count
#c) Movies vs Cd-Vinyl
nrow(distinct_movies_tv_reviewers)/nrow(distinct_cd_vinyl_reviewers)#123960/75258=1.6471, So movies category & 1.5 times more reviewers then cd-vinyl

#So overall Movies/TV product category wins in terms of number of distinct reviewers, but overall number of reviews 
#for Movies/TV are also 1.72 & 1.54 times kindle & cd-vinyl reviews respectively, will investigate it further below

#-------------------------------------------------------------------------------------------------------------------------------------------------------
##Step 2: Finding average number of reviews per reviewer for each product category
showDF(agg(distinct_kindle_reviewers, mean = mean(distinct_kindle_reviewers$count))) #14.40
showDF(agg(distinct_cd_vinyl_reviewers, mean = mean(distinct_cd_vinyl_reviewers$count))) #14.58
showDF(agg(distinct_movies_tv_reviewers, mean = mean(distinct_movies_tv_reviewers$count))) #13.69

#Observations: Average reviews based on reviewers does not show much variation among 3 product categories 

#-------------------------------------------------------------------------------------------------------------------------------------------------------
##Step 3: Finding reviews count by date
#a) Yearly Analysis
createOrReplaceTempView(movie_reviews, "movie_reviews_table")
movie_reviews_tabledf <- sql("SELECT year(from_unixtime(unixreviewtime)) as yr, count(*) as review_count FROM movie_reviews_table group by year(from_unixtime(unixreviewtime)) order by yr")
showDF(movie_reviews_tabledf)

createOrReplaceTempView(kindle_reviews, "kindle_reviews_table")
kindle_reviews_tabledf <- sql("SELECT year(from_unixtime(unixreviewtime)) as yr, count(*) as review_count FROM kindle_reviews_table group by year(from_unixtime(unixreviewtime)) order by yr")
showDF(kindle_reviews_tabledf)

createOrReplaceTempView(cd_vinyl_reviews, "cdvinyl_reviews_table")
cdvinyl_reviews_tabledf <- sql("SELECT year(from_unixtime(unixreviewtime)) as yr, count(*) as review_count FROM cdvinyl_reviews_table group by year(from_unixtime(unixreviewtime)) order by yr")
showDF(cdvinyl_reviews_tabledf)

#Visualising product category reviews trend by years
normal_movie_dataframe <- collect(movie_reviews_tabledf) # collecting dataframe on local as ggplot doesn't work with spark dataframe 
str(normal_movie_dataframe) #'yr' is int type, changing type to factor for plotting
normal_movie_dataframe$yr<-as.factor(normal_movie_dataframe$yr)
#plot(normal_movie_dataframe)
ggplot(normal_movie_dataframe, aes(x=yr, y=review_count)) +
  geom_bar(stat="identity", fill="#e67e22", alpha=0.9) +
  labs(title="Reviews per year for Movies category",x="Year" ,y="Total Number of Reviews")
#Observation: movie reviews rose rose gradually till 2005, remained constant till 2011 & then suddenly rose exponentialy in 2013 & 2014

normal_kindle_dataframe <- collect(kindle_reviews_tabledf)
#plot(normal_kindle_dataframe)
ggplot(normal_kindle_dataframe, aes(x=yr, y=review_count)) +
  geom_bar(stat="identity", fill="#e67e22", alpha=0.9) +
  labs(title="Reviews per year for Kindle category",x="Year" ,y="Total Number of Reviews")
#Observation: As kindle is a recent product reviews started in 2008 & have grown exponentially since then

normal_cdvinyl_dataframe<-collect(cdvinyl_reviews_tabledf)
#plot(normal_cdvinyl_dataframe)
ggplot(normal_cdvinyl_dataframe, aes(x=yr, y=review_count)) +
  geom_bar(stat="identity", fill="#e67e22", alpha=0.9) +
  labs(title="Reviews per year for CD/Vinyl",x="Year" ,y="Total Number of Reviews")
#cd-vinyl reviews fluctuates alot, were at peak in 2005 & 2013

#Observations: From plots its clear that movie & kindle products demand rised rapidly but cd-vinyl shows a fluctuating trend
#              Lets explore this further by filtering on monthly basis for recent year data in each product category

#b) Monthly Analysis
#Lets investigate further & check monthly review pattern for year 2013 as for year 2014 only 7 month data is available
movie_reviews_2013<-sql("select * from movie_reviews_table where year(from_unixtime(unixreviewtime))==2013")
createOrReplaceTempView(movie_reviews_2013, "movie_reviews_2013")
movie_reviews_monthly_tabledf <- sql("SELECT month(from_unixtime(unixreviewtime)) as mnth, count(*) as review_count FROM movie_reviews_2013 group by month(from_unixtime(unixreviewtime)) order by mnth")
showDF(movie_reviews_monthly_tabledf)
#plot(collect(movie_reviews_monthly_tabledf))
ggplot(collect(movie_reviews_monthly_tabledf), aes(x=mnth, y=review_count)) +
  geom_bar(stat="identity", fill="#e67e22", alpha=0.9) +
  labs(title="Reviews per month for Movies category",x="Month" ,y="Total Number of Reviews")

kindle_reviews_2013<-sql("select * from kindle_reviews_table where year(from_unixtime(unixreviewtime))==2013")
createOrReplaceTempView(kindle_reviews_2013, "kindle_reviews_2013")
kindle_reviews_monthly_tabledf <- sql("SELECT month(from_unixtime(unixreviewtime)) as mnth, count(*) as review_count FROM kindle_reviews_2013 group by month(from_unixtime(unixreviewtime)) order by mnth")
showDF(kindle_reviews_monthly_tabledf)
#plot(collect(kindle_reviews_monthly_tabledf))
ggplot(collect(kindle_reviews_monthly_tabledf), aes(x=mnth, y=review_count)) +
  geom_bar(stat="identity", fill="#e67e22", alpha=0.9) +
  labs(title="Reviews per month for Kindle category",x="Month" ,y="Total Number of Reviews")

cdvinyl_reviews_2013<-sql("select * from cdvinyl_reviews_table where year(from_unixtime(unixreviewtime))==2013")
createOrReplaceTempView(cdvinyl_reviews_2013, "cdvinyl_reviews_2013")
cdvinyl_reviews_monthly_tabledf <- sql("SELECT month(from_unixtime(unixreviewtime)) as mnth, count(*) as review_count FROM cdvinyl_reviews_2013 group by month(from_unixtime(unixreviewtime)) order by mnth")
showDF(cdvinyl_reviews_monthly_tabledf)
#plot(collect(cdvinyl_reviews_monthly_tabledf))
ggplot(collect(cdvinyl_reviews_monthly_tabledf), aes(x=mnth, y=review_count)) +
  geom_bar(stat="identity", fill="#e67e22", alpha=0.9) +
  labs(title="Reviews per month for Cd-Vinyl category",x="Month" ,y="Total Number of Reviews")

#Observations: From ercent monthly reviews patterns its clear that kindle is most popular category by increase in number of reviews recently 

#-------------------------------------------------------------------------------------------------------------------------------------------------------
#Step 4: Finding correlation between reviewtext length and rating
createOrReplaceTempView(movie_reviews, "movie_reviewrating_table")
movie_reviewrating_tabledf <- sql("SELECT overall as rating, avg(size(split(reviewtext,' '))) as words from movie_reviewrating_table group by overall order by words desc")
showDF(movie_reviewrating_tabledf)
#Observations: Rating 2 have largest average review length followed by 3,4,1 & 5 for movie product category.

createOrReplaceTempView(kindle_reviews, "kindle_reviewrating_table")
kindle_reviewrating_tabledf <- sql("SELECT overall as rating, avg(size(split(reviewtext,' '))) as words from kindle_reviewrating_table group by overall order by words desc")
showDF(kindle_reviewrating_tabledf)
#Observations: Rating 4 have largest average review length followed by 3,2,5 & 1 for kindle product category.

createOrReplaceTempView(cd_vinyl_reviews, "cdvinyl_reviewrating_table")
cdvinyl_reviewrating_tabledf <- sql("SELECT overall as rating, avg(size(split(reviewtext,' '))) as words from cdvinyl_reviewrating_table group by overall order by words desc")
showDF(cdvinyl_reviewrating_tabledf)
#Observations: Rating 4 have largest average review length followed by 3,2,5 & 1 for cd-vinyl product category.

#So there is no obviuos correlation between reviewtext length and rating as we see case of movies low rating of 2 had longest reviews 
#while in case of cd-vinyl & kindle high rating of 4 had longer reviews, rating of 5 was always inbetween, Lets investigate this further using helpfulness feature

#--------------------------------------------------------------------------------------------------------------------------------------------------------
#Step 5: Finding Popular categories based on how helpful their reviews were, only reviews with atleast 10 people vote are used here
#a)
#Kindle reviews helpfulness percentage
createOrReplaceTempView(kindle_reviews, "helpful_kindle_reviews_ratio")
helpful_kindle_reviews_ratio<-sql("select helpful[0] found_helpful,helpful[1] total_people,(helpful[0]/helpful[1])*100 as percent_helpfulness from helpful_kindle_reviews_ratio where helpful[1]!=0 and helpful[1]>=10")
head(helpful_kindle_reviews_ratio) 

createOrReplaceTempView(helpful_kindle_reviews_ratio, "helpful_kindle_reviews_ratio")
helpful_kindle_reviews_average<-sql("select avg(percent_helpfulness) from helpful_kindle_reviews_ratio")
head(helpful_kindle_reviews_average) #83.2422%

#Cd-Vinyl reviews helpfulness percentage
createOrReplaceTempView(cd_vinyl_reviews, "helpful_cd_vinyl_reviews_ratio")
helpful_cd_vinyl_reviews_ratio<-sql("select helpful[0] found_helpful,helpful[1] total_people,(helpful[0]/helpful[1])*100 as percent_helpfulness from helpful_cd_vinyl_reviews_ratio where helpful[1]!=0 and helpful[1]>=10")
head(helpful_cd_vinyl_reviews_ratio) 

createOrReplaceTempView(helpful_cd_vinyl_reviews_ratio, "helpful_cd_vinyl_reviews_ratio")
helpful_cd_vinyl_reviews_average<-sql("select avg(percent_helpfulness) from helpful_cd_vinyl_reviews_ratio")
head(helpful_cd_vinyl_reviews_average) #69.63146%

#Movie reviews helpfulness percentage
createOrReplaceTempView(movie_reviews, "helpful_movie_reviews_ratio")
helpful_movie_reviews_ratio<-sql("select helpful[0] found_helpful,helpful[1] total_people,(helpful[0]/helpful[1])*100 as percent_helpfulness from helpful_movie_reviews_ratio where helpful[1]!=0 and helpful[1]>=10")
head(helpful_movie_reviews_ratio)

createOrReplaceTempView(helpful_movie_reviews_ratio, "helpful_movie_reviews_ratio")
helpful_movie_reviews_average<-sql("select avg(percent_helpfulness) from helpful_movie_reviews_ratio")
head(helpful_movie_reviews_average) #64.97571%

#Observations: People found kindle reviews most helpful on average followed by cd-vinyl & movies

#b) Lets investigate further & check coorelation between review length & helpfulness for each product category
createOrReplaceTempView(kindle_reviews, "helpful_kindle_reviews_length")
helpful_kindle_reviews_length<-sql("select (helpful[0]/helpful[1])*100 percent_helpfulness,size(split(reviewtext,' ')) words from helpful_kindle_reviews_length where helpful[1]!=0 and helpful[1]>=10 order by words")
helpful_kindle_reviews_length_local<-collect(helpful_kindle_reviews_length)
cor(helpful_kindle_reviews_length_local$percent_helpfulness,helpful_kindle_reviews_length_local$words)
ggplot(helpful_kindle_reviews_length_local, aes(words,percent_helpfulness)) + geom_point()
#Above plot is very cluttered, lets narrow down to small range of word length(5-500) 

helpful_kindle_reviews_length_subset<-subset(helpful_kindle_reviews_length_local,helpful_kindle_reviews_length_local$words<1000 & helpful_kindle_reviews_length_local$words>5)
nrow(helpful_kindle_reviews_length_subset) #21249 rows
ggplot(helpful_kindle_reviews_length_subset, aes(words,percent_helpfulness)) + geom_point()
cor(helpful_kindle_reviews_length_subset$percent_helpfulness,helpful_kindle_reviews_length_subset$words)
#Still very cluttered, lets try binning data to refine further

#c) Dividing data into equal size bins for word length
library(data.table)
library(lattice)
nBins <- 50 #So each bin will have words between length 1 to 50,50 to 100,...n to n+50

#i) Kindle
helpful_kindle_reviews_binned<-data.table(helpful_kindle_reviews_length_local) #converting dataframe type for binning
helpful_kindle_reviews_binned[, c('words_bin'):=list(floor(words/nBins))] #binning words based on word length in review

helpful_kindle_reviews_binned<-helpful_kindle_reviews_binned[,-c(2)]
helpful_kindle_reviews_binned <- helpful_kindle_reviews_binned[,lapply(.SD,mean),by=words_bin]
head(helpful_kindle_reviews_binned)
ggplot(helpful_kindle_reviews_binned, aes(words_bin,percent_helpfulness)) + geom_point()

#ii) Movies
createOrReplaceTempView(movie_reviews, "helpful_movie_reviews_length")
helpful_movie_reviews_length<-sql("select (helpful[0]/helpful[1])*100 percent_helpfulness,size(split(reviewtext,' ')) words from helpful_movie_reviews_length where helpful[1]!=0 and helpful[1]>=10 order by words")
helpful_movie_reviews_length_local<-collect(helpful_movie_reviews_length)

helpful_movie_reviews_binned<-data.table(helpful_movie_reviews_length_local)
helpful_movie_reviews_binned[, c('words_bin'):=list(floor(words/nBins))]

helpful_movie_reviews_binned<-helpful_movie_reviews_binned[,-c(2)]
helpful_movie_reviews_binned <- helpful_movie_reviews_binned[,lapply(.SD,mean),by=words_bin]
head(helpful_movie_reviews_binned)
ggplot(helpful_movie_reviews_binned, aes(words_bin,percent_helpfulness)) + geom_point()

#iii) Cd-Vinyl
createOrReplaceTempView(cd_vinyl_reviews, "helpful_cdvinyl_reviews_length")
helpful_cdvinyl_reviews_length<-sql("select (helpful[0]/helpful[1])*100 percent_helpfulness,size(split(reviewtext,' ')) words from helpful_cdvinyl_reviews_length where helpful[1]!=0 and helpful[1]>=10 order by words")
helpful_cdvinyl_reviews_length_local<-collect(helpful_cdvinyl_reviews_length)

helpful_cdvinyl_reviews_binned<-data.table(helpful_cdvinyl_reviews_length_local)
helpful_cdvinyl_reviews_binned[, c('words_bin'):=list(floor(words/nBins))]

helpful_cdvinyl_reviews_binned<-helpful_cdvinyl_reviews_binned[,-c(2)]
helpful_cdvinyl_reviews_binned <- helpful_cdvinyl_reviews_binned[,lapply(.SD,mean),by=words_bin]
head(helpful_cdvinyl_reviews_binned)
ggplot(helpful_cdvinyl_reviews_binned, aes(words_bin,percent_helpfulness)) + geom_point()

#Observations: Above plots show that:
#              i) Review helpfulness sharply increases between bin 0-8 or word length 0-400 
#             ii) Review helpfulness almost become constant between bin 9-30 or word length 450-1500
#             iii)Reviews helpfulness above bin 30 or word length>1500 is almost erratic & unpredictable      
#But all 3 product categories show similar observations 

#---------------------------------------------------------------------------------------------------------------------------------------------------
##Step 6: Finding popular products in each category
#a) Finding top products by number of reviews they received
movies_productwise_reviews<-summarize(groupBy(movie_reviews, movie_reviews$asin), total_movie_reviews = count(movie_reviews$reviewerID))
head(arrange(movies_productwise_reviews, desc(movies_productwise_reviews$total_movie_reviews)))
#Top 3 movie products by total reviews: B003EYVXV4:2213, B001KVZ6HK:2110, B009934S5M:1974

kindle_productwise_reviews<-summarize(groupBy(kindle_reviews,kindle_reviews$asin), total_kindle_reviews = count(kindle_reviews$reviewerID))
head(arrange(kindle_productwise_reviews, desc(kindle_productwise_reviews$total_kindle_reviews)))
#Top 3 kindle products by total reviews: B006GWO5WK:1113, B00BTIDW4S:781, B00BT0J8ZS:516

cdvinyl_productwise_reviews<-summarize(groupBy(cd_vinyl_reviews,cd_vinyl_reviews$asin), total_cdvinyl_reviews = count(cd_vinyl_reviews$reviewerID))
head(arrange(cdvinyl_productwise_reviews, desc(cdvinyl_productwise_reviews$total_cdvinyl_reviews)))
#Top 3 cd-vinyl products by total reviews: B00008OWZG:1026, B000000IRB:855, B000089RVX:702

##b) Investing further to find top products by rating they received
best_rated_movies<-subset(movie_reviews, movie_reviews$overall %in% c(5,4))
best_movie_reviews<-summarize(groupBy(best_rated_movies,best_rated_movies$asin), good_movie_reviews=count(best_rated_movies$overall))
head(arrange(best_movie_reviews, desc(best_movie_reviews$good_movie_reviews)))
#Top 3 movies with max ratings of 4 & 5: B001KVZ6HK:1869, B003EYVXV4:1705, B009934S5M:1621

best_rated_kindle<-subset(kindle_reviews, kindle_reviews$overall %in% c(5,4))
best_kindle_reviews<-summarize(groupBy(best_rated_kindle,best_rated_kindle$asin), good_kindle_reviews=count(best_rated_kindle$overall))
head(arrange(best_kindle_reviews, desc(best_kindle_reviews$good_kindle_reviews)))
#Top 3 movies with max ratings of 4 & 5: B006GWO5WK:987, B00BTIDW4S:685, B00BT0J8ZS:474

best_rated_cdvinyl<-subset(cd_vinyl_reviews, cd_vinyl_reviews$overall %in% c(5,4))
best_cdvinyl_reviews<-summarize(groupBy(best_rated_cdvinyl,best_rated_cdvinyl$asin), good_cdvinyl_reviews=count(best_rated_cdvinyl$overall))
head(arrange(best_cdvinyl_reviews, desc(best_cdvinyl_reviews$good_cdvinyl_reviews)))
#Top 3 movies with max ratings of 4 & 5: B000000IRB:773, B000002UB2:586, B000002UJQ:569

#Observations: Based on reviews usually top products have both large number of reviewes & higher ratings

#---------------------------------------------------------------------------------------------------------------------------------------------------
#Step 7: Finding satisfaction ratio using ratings, assumming those rated 4,5 are best & those rated 1,2 are worst 
#Using best rated products count in each category from previous step
nrow(best_rated_movies) #1289602

worst_rated_movies<-subset(movie_reviews, movie_reviews$overall %in% c(1,2))
worst_movie_reviews<-summarize(groupBy(worst_rated_movies,worst_rated_movies$asin), worst_movie_reviews=count(worst_rated_movies$overall))
head(arrange(worst_movie_reviews, desc(worst_movie_reviews$worst_movie_reviews)))
nrow(worst_movie_reviews) #33487
movie_satisfaction_ratio <- 1289602/33487 #Calculated as best_rated_movies/worst_rated_movies
movie_satisfaction_ratio #38.51

nrow(best_kindle_reviews) #61842

worst_rated_kindle<-subset(kindle_reviews, kindle_reviews$overall %in% c(1,2))
worst_kindle_reviews<-summarize(groupBy(worst_rated_kindle,worst_rated_kindle$asin), worst_kindle_reviews=count(worst_rated_kindle$overall))
head(arrange(worst_kindle_reviews, desc(worst_kindle_reviews$worst_kindle_reviews)))
nrow(worst_kindle_reviews) #24398
kindle_satisfaction_ratio <- 61842/24398
kindle_satisfaction_ratio #2.53

nrow(best_cdvinyl_reviews) #64356

worst_rated_cdvinyl<-subset(cd_vinyl_reviews, cd_vinyl_reviews$overall %in% c(1,2))
worst_cdvinyl_reviews<-summarize(groupBy(worst_rated_cdvinyl,worst_rated_cdvinyl$asin), worst_cdvinyl_reviews=count(worst_rated_cdvinyl$overall))
head(arrange(worst_cdvinyl_reviews, desc(worst_cdvinyl_reviews$worst_cdvinyl_reviews)))
nrow(worst_cdvinyl_reviews) #28608
cdvinyl_satisfaction_ratio <- 64356/28608
cdvinyl_satisfaction_ratio #2.249

#Observations: Movies product category clearly have much higher ratio of positive ratings(4 or 5 rating) compared to other 2 categories

#------------------------------------------------------------------------------------------------------------------------------------------------
#Conclusions:
#Problem1 : Which product category has a larger market size?
#Answer 1 : As "Movies and TV" category gave maximum distinct reviewers overall as well as by ratio(Step 1), its most popular popular product category. 
#Problem2 : Which product category is likely to be purchased heavily?
#Answer 2 : Based on reviews helpfulness percentage(Step 5), ratings(Step 4) & increase in number of reviews(Step 3) "Kindle" seems likely to be purchased heavily.
#Problem3 : Which product category is likely to make the customers happy after the purchase?
#Answer 3 : Based on satisfaction ratio using ratings(Step 7), "Movies ansd TV" category seems like best category in terms of customer satisfaction.
#
#Final Choice: So there is a tie between Kindle & Movie category here, but as Movie category is best in 2 out 3 questions above,
#We will recommned our client to invest in "Movies and TV" category as its better in terms of market size & customer satisfaction. 
#
#Also based on analysis in Step 6 we can suggest top 3 products under "Movies and TV" category as: B003EYVXV4, B001KVZ6HK, B009934S5M


