# Load project
library("ProjectTemplate")
load.project()

#analysis the gender
enr_gender = filter(enrolments, gender == "male" | gender == "female" )
g_enr_gender = ggplot(data = enr_gender)
g_enr_gender_bar = g_enr_gender + geom_bar(aes(x = gender, fill = gender ),  width = 0.4) + theme(panel.grid.major =element_blank(),  # 去除边框
                          panel.grid.minor = element_blank(),
                          panel.background = element_blank(),
                          axis.title =  element_text(size=12,face = "bold"),
                          axis.text.x =   element_text(hjust = 1, # Adjusting the position of text in horizontal coordinates
                                                       size=15),  # Adjusting the font size of text in horizontal coordinates
                          plot.margin=unit(rep(3,4),'lines'))     # Set image margins 
g_enr_gender_bar





#analysis the age
#enrolments
enr_age = filter(enrolments, age_range != "Unknown")
#enr_age
#Find out what the age ranges are
index = duplicated(enr_age$age_range)
enr_age_list = enr_age[!index,9]
enr_age = select(enr_age, learner_id, age_range)
age_num = c()
for(i in 1:7){
  #generating single country data frame
  single_age = filter(enr_age,age_range == as.character(enr_age_list[i,1]))
  #compute the number of people that age
  nums = dim(single_age)[1]
  #Add this num to the list
  age_num = c(age_num ,nums)
}
age_num = as.data.frame(age_num)
enr_age_num = cbind(enr_age_list,age_num)
#enr_age_num
ggplot(data=enr_age_num, aes(x=age_range, y=age_num)) + geom_point(color="red") + xlab("Age_range")

 

#analysis the country
enr_country = filter(enrolments, country != "Unknown" )
#enr_country
#Find out what the countries are
index = duplicated(enr_country$country)
enr_country_list = enr_country[!index,8]    
#enr_country_list
#educed data volume and accelerated computing
enr_country = select(enr_country,country)
country_num = c()
for(i in 1:153){
  #generating single country data frame
  single_country = filter(enr_country,country == as.character(enr_country_list[i,1]))
  #compute the number of people that country
  nums = dim(single_country)[1]
  #Add the uncomplete rate for this step to the list
  country_num = c(country_num ,nums)
}
country_num 
#Transformation into a data framework
country_num = as.data.frame(country_num)
#combine
enr_country_num = cbind(enr_country_list,country_num)
enr_country_num = filter(enr_country_num, country_num>10)
enr_country_num = arrange(enr_country_num,desc(country_num))

myLabel = as.vector(enr_country_num$country)   ## Conversion to vectors, otherwise the labels of the legend may not match the actual order
myLabel = paste(myLabel, "(", round(enr_country_num$country_num / sum(enr_country_num$country_num) * 100, 2), "%) ", sep = "")   ## Use round() to retain two decimal places for the result

ggplot(enr_country_num, aes(x = "", y=country_num, fill = country)) + geom_bar(stat = "identity") +
          coord_polar(theta = "y") + 
          labs(x = "", y = "", title = "") + theme(axis.ticks = element_blank()) + 
          theme(legend.title = element_blank(), legend.position = "top") + 
          scale_fill_discrete(breaks = enr_country_num$country, labels = myLabel)   ## Replace the original legend label with the current myLabel








#analysis the leaving
ggplot(data=leaving_survey_responses,
       aes(x=last_completed_step_number, y=last_completed_step)) +geom_point(stat = "sum", color="darkred")+
       theme(
          axis.title =  element_text(size=12,face = "bold"),
          axis.text.x =   element_text(hjust = 1, # Adjusting the position of text in horizontal coordinates
                                       size=15),  # Adjusting the font size of text in horizontal coordinates
          plot.margin=unit(rep(3,4),'lines'))     # Set image margins  

#view(leaving_survey_responses)







#analysis the false rate
#Find out what the problems are
index = duplicated(question_response$quiz_question)
question_response_quiz = question_response[!index,2]

#compute the false rate for each question
false_rate = data.frame()
for(i in 1:29){
  #generating single question's data frame
  question_response_false = filter(question_response,quiz_question == as.character(question_response_quiz[i,1]))
  #compute the num of true answers
  true_answers = select(question_response_false,correct)
  e = unlist(true_answers)
  true_times = sum(e == "false")
  #compute the rate
  question_response_false = mutate(question_response_false,quiz_false_rates = true_times/dim(question_response_false)[1])
  #add the rate to the data frame
  false_rate = rbind(false_rate,question_response_false) 
}
#false_rate
#view(false_rate)
#analysis the false rate of each quiz
#quiz data de-duplication
index = duplicated(false_rate$quiz_question)
quiz_rate = false_rate[!index,]
quiz_rate_plot = select(quiz_rate,quiz_question, quiz_false_rates)
#quiz_rate_plot
ggplot(data = quiz_rate_plot, aes(x = quiz_question, y = quiz_false_rates, fill= quiz_question)) + geom_bar(stat='identity') + 
              theme(panel.grid.major =element_blank(),  
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.title =  element_text(size=12,face = "bold"),
                    axis.text.x = element_text(size = 0),  
                    plot.margin=unit(rep(3,4),'lines'))    






#analysis the activity step
#Find out what the activities are
index = duplicated(step_activity$step)
step_activity_step = step_activity[!index,2]    
#step_activity_step
step_activity2 = select(step_activity, step, last_completed_at)
#compute the complete rate
uncomplete_rate = c()
for(i in 1:58){
  #generating single step's data frame
  step_activity_step_single = filter(step_activity2,step == as.character(step_activity_step[i,1]))
  #compute the times that did not finish
  times = select(step_activity_step_single,last_completed_at)
  e = unlist(times)
  uncomplete_times = sum(e == "")
  #Add the uncompleted rate for this step to the list
  uncomplete_rate = c(uncomplete_rate,uncomplete_times/dim(step_activity_step_single)[1])
}
#Transformation into a data framework
uncomplete_rate = as.data.frame(uncomplete_rate)
#combine
step_activity_step = cbind(step_activity_step,uncomplete_rate)
#step_activity_step
#Removal of lower values
step_activity_step = filter(step_activity_step, uncomplete_rate > 0.15)
ggplot(data = step_activity_step, aes(x = as.factor(step), y = uncomplete_rate, fill= step)) + geom_bar(stat='identity') + 
  theme(panel.grid.major =element_blank(),  
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title =  element_text(size=12,face = "bold"),
        axis.text.x = element_text(size = 12),  
        plot.margin=unit(rep(3,4),'lines'))     





#分析学习者性别/国籍/年龄和正确率的关系
#数据清洗
enrolments3 = filter(enrolments,role == 'learner'& gender != 'Unknown' &country != 'Unknown' &age_range!='Unknown')
enrolments3 = select(enrolments3, learner_id, gender, country, age_range)
enrolments3 = arrange(enrolments3,desc(learner_id))
enrolments3
#减少复杂度
question_response3 = select(question_response, learner_id,correct)
question_response3 = arrange(question_response3, desc(learner_id))
question_response3
#Find out what the ids are
index = duplicated(question_response3$learner_id)
learner_list = question_response3[!index,1]
learner_list
learner_true_rate = data.frame()
for(i in 1:9176){
  #generating single id's data frame
  single_id = filter(question_response3, learner_id == as.character(learner_list[i,1]))
  learner_true = dim(filter(single_id, correct == 'true'))[1]
  true_rate = learner_true/dim(single_id)[1]
  single_id = mutate(single_id, true_rate = learner_true/dim(single_id)[1])
  learner_true_rate = rbind(learner_true_rate,select(single_id,learner_id,true_rate))
}
#去重
index = duplicated(learner_true_rate)
learner_true_rate2 = learner_true_rate[!index,]
learner_true_rate2
#合并正确率和性别
enrolments3 = left_join(enrolments3,learner_true_rate2,by='learner_id')
#去除空值
enrolments3 = na.omit(enrolments3)
enrolments3

