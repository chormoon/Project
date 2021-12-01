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
                          axis.text.x =   element_text(hjust = 1, # 调整横坐标文字位置
                                                       size=15),  # 调整横坐标文字字号
                          plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  

g_enr_gender_bar


#analysis the leaving
ggplot(data=leaving_survey_responses,
       aes(x=last_completed_step_number, y=last_completed_step)) +geom_point(stat = "sum", color="darkred")+
       theme(
          axis.title =  element_text(size=12,face = "bold"),
          axis.text.x =   element_text(hjust = 1, # 调整横坐标文字位置
                                       size=15),  # 调整横坐标文字字号
          plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  




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
  question_response_false = mutate(question_response_false,rates = true_times/dim(question_response_false)[1])
  #add the rate to the data frame
  false_rate = rbind(false_rate,question_response_false) 
}
false_rate
#view(false_rate)


#analysis the false rate of each quiz
#quiz data de-duplication
index = duplicated(false_rate$quiz_question)
quiz_rate = false_rate[!index,]
quiz_rate_plot = select(quiz_rate,quiz_question, rates)
quiz_rate_plot
ggplot(data = quiz_rate_plot, aes(x = quiz_question, y = rates, fill= quiz_question)) + geom_bar(stat='identity') + 
              theme(panel.grid.major =element_blank(),  # 去除边框
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.title =  element_text(size=12,face = "bold"),
                    axis.text.x = element_text(size = 0),  # 调整横坐标文字字号
                    plot.margin=unit(rep(3,4),'lines'))     # 设置图片边距  
                   