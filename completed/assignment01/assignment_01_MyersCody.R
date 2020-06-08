#1) 
#Observational units : The grades of the students

#2)
#Variables: 
#Categorical : Class selection, count
#quantitative : total points earned


scores_df <- read.csv("data/scores.csv")
summary(scores_df)
myList <- split(scores_df, scores_df$Section)

#3)
sports_scores <- myList$Sports
regular_scores <- myList$Regular

#4)
plot(sports_scores$Score, sports_scores$Count, xlab = "Scores", ylab = "Count of Students", main = "Sports based course student scores")
plot(regular_scores$Score, regular_scores$Count, xlab = "Scores", ylab = "Count of Students", main = "Regular course student scores")

#4a)
#After reviewing the two plots, it was shown that students in the regular course tended to receive higher scores than that of 
#their counterparts in the sports based section. 

#4b)
#No, one section of students did not completely outperform the other section even thought one set of students performed better than the other. 
#This can be explained by what a statistical tendency means. A statistical tendency, in this case, is the average of the scores of the two sets of
#students. So to tell which set of students did better, we can look at the charts and see that on average, the regular course
#had students that performed better than those in the sports course.

#4c)
#One variable that could be influencing the point distribution is the GPA of the students prior to taking the course.
#The questions that come to my mind regarding this are "Who are these students taking these two classes?",
#"Are they high performing students or low performing ones?", "Is there a correlation between the performance level of a student
#and the version of the course they choose to take?".