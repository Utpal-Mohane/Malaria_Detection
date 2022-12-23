# Malaria Detection 

# importing the data set
df = read.csv("C:\\Users\\utpal\\Desktop\\R PROJECT\\maleria dataset.csv")

# shifting the target column towards end of the dataset
df = df[,c(2,3,4,5,6,1)]


# Performing Logistic Regression on 'Label' column
df$Label = as.factor(df$Label)
colnames(df)[6] = 'Label'

# splitting the data into train and test
samp = sample(c(1: nrow(df)), 22000)
train = df[samp, ]
test = df[-samp, ]

# LR
model = glm(Label~., data = train, family = 'binomial')
summary(model)


# making predictions
pred = predict(model, test, response = 'predict')
pred
# checking for accuracy 
p = ifelse(pred<0.5, 'Parasitized', 'Uninfected')
t = table(p, test$Label)

acc = mean(p == test$Label)
acc * 100

acc = sum(diag(t))/ sum(t) * 100
acc # 90% accuracy



# making pie chart

# for Parasitized person -----------------------------------------> 

# taking average of each area
area0 = mean(c(df[1: 13779, 1]))
area1 = mean(c(df[1: 13779, 2]))
area2 = mean(c(df[1: 13779, 3]))
area3 = mean(c(df[1: 13779, 4]))
area4 = mean(c(df[1: 13779, 5]))
a = c(area0, area1, area2, area3, area4)

# creating a pie plot regarding average area in case of Parasitized
pie(a, main = 'average proportion of each area for a patient to be, 
    declared Parasitized', labels = c('area 0', 'area 1', 
                                      'area 2', 'area 3', 
                                      'area 4'))
barplot(a, names.arg = c('area 0', 'area 1', 'area 2', 'area 3', 
                         'area 4'), ylab = 'Area', main = 'proportion of
        different areas')

# for Uninfected person --------------------------------------------> 

# taking average of each area
area0 = mean(c(df[13780: nrow(df), 1]))
area1 = mean(c(df[13780: nrow(df), 2]))
area2 = mean(c(df[13780: nrow(df), 3]))
area3 = mean(c(df[13780: nrow(df), 4]))
area4 = mean(c(df[13780: nrow(df), 5]))

a = c(area0, area1, area2, area3, area4)

# creating a pie plot regarding average area in case of Uninfected
pie(a, main = 'average proportion of each area for a patient to be, 
    declared Uninfected', labels = c('area 0', 'area 1', 
                                     'area 2', 'area 3', 
                                     'area 4'))

barplot(a, names.arg = c('area 0', 'area 1', 'area 2', 'area 3', 
                         'area 4'), ylab = 'Area', main = 'proportion of
        different areas')