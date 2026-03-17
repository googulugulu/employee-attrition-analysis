#  Data Import
employee_data = read.csv("C://APU//YEAR 2 SEM 1//PROGRAMMING FOR DATA ANALYSIS//Assignment//employee_attrition.csv", header = TRUE)
employee_data

str(employee_data)

#  Packages
install.packages("ggplot2")
install.packages("forcats")
install.packages("tidyr")
install.packages("dplyr")
install.packages("scales")

#  Library
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
library(scales)


names(employee_data) = c("Employee_ID", "Record_Date", "Birth_Date", "Hire_Date", "Termination_Date", "Age", "Length_Of_Service", "City_Name", "Department_Name", "Job_Title", "Store_Name", "Gender", "Gender_Full", "Termination_Reason" , "Termination_Type", "Status_Year", "Status", "Business_Unit")
employee_data

data <- employee_data %>%
  mutate(
    Termination_Date = ifelse(Termination_Date == "1/1/1900", NA, Termination_Date),
    Termination_Date = as.Date(Termination_Date, format = "%m/%d/%Y"),
    City_Name = as.factor(City_Name),
    Department_Name = as.factor(Department_Name),
    Job_Title = as.factor(Job_Title), 
    Store_Name = as.factor(Store_Name),
    Gender = as.factor(Gender),
    Termination_Reason = ifelse(Termination_Reason == "Resignaton", "Resignation", Termination_Reason),
    Termination_Reason = as.factor(Termination_Reason),
    Termination_Type = as.factor(Termination_Type),
    Status_Year = as.factor(Status_Year),
    Status_Year = ordered(Status_Year, c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015)),
    Status = as.factor(Status),
    Business_Unit = as.factor(Business_Unit)
  ) %>%
  select(everything(), -c(Gender_Full, Record_Date, Birth_Date, Hire_Date))
data



str(data)
summary(data)

#leave the latest information
per_employee <- data %>% group_by(Employee_ID) %>%
  filter(Status_Year == max(Status_Year))

#create new column to identify the duplicate employee id
per_employee <- per_employee %>% group_by(Employee_ID) %>%
  mutate(Duplicate = ifelse(n() > 1, "Duplicate", "Not Duplicate"))

#create new column to identify the status of each duplicate id
per_employee <- per_employee %>% group_by(Employee_ID, Duplicate) %>%
  mutate(Duplicate_Status = ifelse(Status == "TERMINATED", "Terminated", "Active"))

#filter data to include only required rows
per_employee <- per_employee %>%
  filter(Duplicate == "Not Duplicate" | Duplicate_Status == "Terminated")

#ungroup so the duplicate data can be deleted
per_employee <- per_employee %>% ungroup()

#remove the extra column created for identification
per_employee <- per_employee %>%
  select(everything(), -Duplicate, -Duplicate_Status)
per_employee %>% group_by(Gender) %>% summarise(Count = n())

per_employee <- data %>% group_by(Employee_ID) %>% filter(Status_Year == max(Status_Year))


#does female more loyal than male?  
#analysis 1: find total male and female
total_male <- sum(per_employee$Gender == "M")
total_female <- sum(per_employee$Gender == "F")
gender_counts <- data.frame(Gender = c("M", "F"),
                            Count = c(total_male, total_female))
gender_counts

ggplot(data = gender_counts, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width=0.7) +
  labs(title = "        Gender Distribution", x = "Gender", y = "Count") +
  scale_fill_manual(values = c("M" = "skyblue", "F" = "pink"))

#bar chart
#geom_bar(stat = "identity) is to create an actual bar
#fill name must same with the x-axis
#analysis 2: find gender in each department
gender_department <- data %>% group_by(Department_Name, Gender) %>%
  summarise(Count = n())

#graph(extra function)   #fill means will show a instruction what is what beside
ggplot(data = gender_department, aes(x = Gender, y = Count, fill = Gender)) + 
  geom_bar(stat = "identity") +
  labs(title = "Counts of Males and Females by Department", x = "Gender", y = "Count") +
  facet_grid(. ~ Department_Name, scales = "free_y") +
  scale_y_continuous(breaks = seq(0, max(gender_counts$Count), by = 500)) +
  scale_fill_manual(values = c("M" = "skyblue", "F" = "pink"))

#analysis 3: find length of service of employee for each department
length_department <- per_employee %>%
  group_by(Department_Name, Length_Of_Service) %>%
  select(Employee_ID, Length_Of_Service, Department_Name, Status) %>%
  filter(Status == "ACTIVE") 

#chart
ggplot(data, aes(x = Department_Name, y = Length_Of_Service, group = Department_Name, color = Department_Name)) +
  geom_line() +
  geom_point() +
  labs(title = "Length of Service by Department", x = "Department", y = "Length of Service") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#analysis 4: which termination reason have more for male 
termination_reason_counts <- per_employee %>%
  filter(Gender == "M") %>%
  group_by(Termination_Reason) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))


ggplot(termination_reason_counts, aes(x = "", y = Count, fill = Termination_Reason)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Termination Reasons for Males", fill = "Termination Reason") +
  scale_fill_manual(values = c("lightgreen", "orange", "lightblue", "yellow")) +
  theme_void()

#analysis 5: find the number of active employees that will be going to retire soon  

retire_soon <- per_employee %>%
  filter(
    Status_Year == 2015,
    Status == "ACTIVE"
  ) %>%
  group_by(Job_Title) %>%
  mutate(
    might_retired_soon = ifelse((Age >= 55 & Age <= 65), TRUE, FALSE)
  )

ggplot(retire_soon, mapping = aes(x = fct_rev(fct_infreq(Job_Title)), fill = might_retired_soon)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage of employees that will retire soon based on business unit (55 to 65 years old in 2005)", 
       fill = "Soon to be retired") +
  xlab("Job Title") +
  ylab("Percentage(%)") +
  coord_flip() +
  facet_wrap(~ Business_Unit, scales = "free")

ggplot(retire_soon, mapping = aes(x = fct_rev(fct_infreq(Job_Title)), fill = might_retired_soon)) +
  geom_bar() + 
  scale_y_continuous(breaks = pretty_breaks()) +
  labs(title = "Number of employee that soon be retired based on business unit (55 to 65 years old in 2015)", 
       fill = "Soon to be retired") +
  xlab("Job Title") + 
  ylab("Percentage(%)") +
  coord_flip() +
  facet_wrap(~Business_Unit, scales = "free")

#analysis 6: find relationship between average age in each job
age_job <- per_employee %>% 
  group_by(Job_Title) %>%
  summarise(Average_Age = mean(Age))

#graph
ggplot(age_job, aes(x = Job_Title, y = Average_Age)) +
  geom_point() +
  labs(title = "Average age in each job", x = "Job Title", y = "Average Age") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#analysis 7: which store and which company have more female
female_store <- per_employee %>% 
  filter(Gender == "F") %>%
  group_by(Store_Name, City_Name) %>%
  summarise(Female_Store = n())

#graph
ggplot(female_store, aes(x = Store_Name, y = Female_Store)) +
  geom_violin() +
  geom_text(aes(label = Female_Store), vjust = -0.5) +
  labs(title = "Count of Females in Each Store and Company", x = "Store", y = "Female Count") +
  theme_minimal()

#analysis 8: which gender terminated more in year of 2008
terminated_year <- per_employee %>%
  filter(Status_Year == 2008) %>%
  group_by(Gender) %>%
  summarise(Termination_Year = sum(Status == "TERMINATED"))
terminated_year

#graph
ggplot(terminated_year, aes(x = Gender, y = Termination_Year)) +
  geom_bar(stat = "identity", fill = "gray") +
  labs(title = "Termination Count by Gender in Year Of 2008", x = "Gender", y = "Termination Count") +
  theme_minimal()

#analysis 9: how many people working in each store
total_people_active <- per_employee %>%
  group_by(Store_Name) %>%
  summarise(Total_People_Active = sum(Status == "ACTIVE"))

#graph
ggplot(total_people_active, aes(x = Store_Name, y = Total_People_Active)) +
  geom_bar(stat = "identity", fill = "#A155B9") +
  labs(title = "Total people working in each store", x = "Store Name", y = "Total People Active") +
  theme_minimal()

#10. Age distribution for each department
# Calculate age statistics by department
age_distribution <- per_employee %>%
  group_by(Department_Name) %>%
  summarise(Min_Age = min(Age),
            Max_Age = max(Age),
            Avg_Age = mean(Age),
            Median_Age = median(Age))

# Plot age distribution graph using facet_wrap
ggplot(per_employee, aes(x = Age, fill = Department_Name)) +
  geom_histogram(binwidth = 5, position = "identity") +
  facet_wrap(~ Department_Name) +
  labs(x = "Age", y = "Count", title = "Age Distribution by Department") +
  geom_vline(data = age_distribution, aes(xintercept = Min_Age),
             linetype = "dashed", color = "green", size = 1) +
  geom_vline(data = age_distribution, aes(xintercept = Max_Age),
             linetype = "dashed", color = "red", size = 1) +
  geom_vline(data = age_distribution, aes(xintercept = Avg_Age),
             linetype = "dashed", color = "blue", size = 1) +
  guides(fill = FALSE) + # Remove the legend for the fill color
  theme_minimal()

#question 2: Why satff leave the compamy and how to reduce resignation
#analysis 1: find the highest termination reason
highest_reason <- factor(per_employee$Termination_Reason)
TermReason = names(sort(summary(highest_reason), decreasing = T)[2:4])  #[2:4] means select 2 to 4 highest
TermReason

#graph
barplot(summary(highest_reason)[TermReason], 
        main = "Highest Termination Reasons",
        xlab = "Termination Reason",
        ylab = "Count",
        col = "skyblue")


#analysis 2: which city is the most people between 19 - 25 and what is their job
age_city <- per_employee %>%
  filter(Age >= 19 & Age <= 25) %>%
  group_by(City_Name, Job_Title) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  group_by(City_Name) %>%
  slice(1)

#graph
ggplot(age_city, aes(x = City_Name, y = Count, fill = Job_Title)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Most Common Job Titles for People Aged 19-25 in Each City",
       x = "City",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#analysis 3: find people between age(25-55), gender(M) resignation
age_gender_reason <- per_employee %>% 
  filter(Age >= 25 & Age <= 55, Gender == "M", Termination_Reason == "Resignation") %>%
  group_by(Age, Gender) %>%
  summarise(Count = n())

#graph
ggplot(age_gender_reason, aes(x = Age, y = Count)) +
  geom_line() +
  labs(title = "Resignation number of male Between Age 25 - 55",
       x = "Age",
       y = "Count",
       color = "Resignation Reason") +
  theme_minimal()

#analysis 4: find relationship between department and resignation reason
department_termination <- per_employee %>%
  filter(Termination_Reason %in% c("Resignation", "Layoff", "Retirement")) %>%
  group_by(Department_Name, Termination_Reason) %>%
  summarise(Count = n())

#graph
ggplot(department_termination, aes(x = Department_Name, y = Count, fill = Termination_Reason)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Resignations, Layoffs, and Retirements by Department",
       x = "Department",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#analysis 5: retirement rate in stores
retirement_rate <- per_employee %>%
  group_by(Store_Name) %>%
  summarise(Retirement_Rate = sum(Termination_Reason == "Retirement") / n() * 100)

#graph
ggplot(retirement_rate, aes(x = Store_Name, y = Retirement_Rate)) +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  labs(title = "Retirement Rate by Store", x = "Store", y = "Retirement Rate (%)") +
  theme_minimal()

#analysis 6: find total people leaving in each job
job_leave <- per_employee %>%
  group_by(Job_Title) %>%
  filter(Status == "TERMINATED") %>%
  summarise(Count = n())

#graph
ggplot(job_leave, aes(x = Job_Title, y = Count, group = 1)) +
  geom_line(color = "blue") +
  labs(title = "People Leaving in Each Job",
       x = "Job",
       y = "Count") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#analysis 7: which year is people layoff more than resignation
library(tidyr)
termination_counts <- per_employee %>%
  group_by(Status_Year, Termination_Reason) %>%
  filter(Termination_Reason == "Layoff" | Termination_Reason == "Resignation") %>%
  summarise(Count = n()) %>%
  pivot_wider(names_from = Termination_Reason, values_from = Count, values_fill = 0)

#graph
ggplot(termination_counts, aes(x = Status_Year, y = Layoff)) +
  geom_bar(stat = "identity", fill = "cyan", color = "black") +
  labs(title = "Years with More Layoffs than Resignations",
       x = "Year",
       y = "Count") +
  theme_minimal()

#analysis 8: how many people leave the company in each year
leave_year <- per_employee %>%
  group_by(Status_Year) %>%
  filter(Status == "TERMINATED") %>%
  summarise(Count = n())

#graph
ggplot(leave_year, aes(x = Status_Year, y = Count)) +
  geom_violin() +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "people leave the company in each year", x = "Year", y = "Count") +
  theme_minimal()


#analysis 9: find length of service for age between 23 - 34
length_age <- per_employee %>%
  group_by(Length_Of_Service) %>%
  filter(Age >= 23 & Age <= 34) %>%
  summarise(Count = n())

#graph
ggplot(length_age, aes(x = Length_Of_Service, y = Count)) +
  geom_point(size = 3) +
  labs(title = "Distribution of Length of Service for Ages 23-34",
       x = "Length of Service",
       y = "Count") +
  theme_minimal()

#analysis 10: average age for current active employee
age_active <- per_employee %>%
  group_by(Status) %>%
  filter(Status == "ACTIVE") %>%
  summarise(Average_Age = mean(Age))
age_active

#graph
ggplot(age_active, aes(x = "", y = Average_Age)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Current Active Employees",
       x = "",
       y = "Age") +
  theme_minimal()

#question 3: Each city have how many store 
#analysis 1: which city have more cashier
city_cashier <- per_employee %>%
  group_by(City_Name) %>%
  filter(Job_Title == "Cashier") %>%
  summarise(Count = n())

#graph
ggplot(city_cashier, aes(x = City_Name, y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Number of Cashiers in Each City",
       x = "City",
       y = "Number of Cashiers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#2. which city have more stores
city_store_counts <- per_employee %>%
  group_by(City_Name) %>%
  count() %>%
  arrange(desc(n))

most_stores <- city_store_counts$City_Name[1]
most_stores_count <- city_store_counts$n[1]

# Print the city with the most stores
cat("The city with the most stores is", most_stores, "with", most_stores_count, "stores.")

#graph
ggplot(city_store_counts, aes(x = City_Name, y = n)) +
  geom_bar(stat = "identity", fill = "midnightblue", color = "black") +
  labs(title = "Number of Stores in Each City",
       x = "City",
       y = "Number of Stores") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3. find total of gender in each city
city_employee_gender <- per_employee %>%
  group_by(City_Name, Gender) %>%
  summarise(Count = n())

#graph
ggplot(city_employee_gender, aes(x = City_Name, y = Count, color = Gender)) +
  geom_jitter(width = .2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4. find relationship between city and termination reason --- termination more, so store less
selected_reasons <- c("Layoff", "Resignation", "Retirement")

city_termination <- per_employee %>%
  filter(Termination_Reason %in% selected_reasons) %>%
  group_by(City_Name, Termination_Reason) %>%
  summarise(Count = n())

#graph
ggplot(city_termination, aes(x = City_Name, y = Count, fill = Termination_Reason)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Termination Reasons by City",
       x = "City",
       y = "Count",
       fill = "Termination Reason") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#5. which department is more in Chilliwack
department_chilliwack <- per_employee %>%
  filter(City_Name == "Chilliwack") %>%
  group_by(Department_Name) %>%
  summarise(Count = n())

#graph
ggplot(department_chilliwack, aes(x = Department_Name, y = Count, fill = Department_Name)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = " which department is more in Chilliwack",
       x = "Department Name",
       y = "Count",
       fill = "Department Name") +
  theme_minimal()

#6. find average length of service in each city
length_city <- per_employee %>%
  group_by(City_Name) %>%
  summarise(Average_Length = mean(Length_Of_Service))

#graph
ggplot(length_city, aes(x = City_Name, y = Average_Length, group = 1)) +
  geom_line() +
  labs(title = "Average Length of Service by City",
       x = "City",
       y = "Average Length of Service") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#7. which store is the most in 2013
store_2013 <- per_employee %>%
  group_by(Store_Name) %>%
  filter(Status_Year == 2013) %>%
  summarise(Count = n())

#graph

ggplot(store_2013, aes(x = Store_Name, y = Count)) +
  geom_violin() +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Count of store in 2013", x = "Store", y = "Female Count") +
  theme_minimal()

#8. how many terminated in year of 2014 in New Westminister
terminated_2014_Westminister <- per_employee %>%
  group_by(Status) %>%
  filter(Status == "TERMINATED", Status_Year == 2014, City_Name == "New Westminister") %>%
  summarise(Count = n())
terminated_2014_Westminister

#9. how many male and female in WestVancouver
gender_WestVancouver <- per_employee %>%
  group_by(Gender) %>%
  filter(City_Name == "West Vancouver") %>%
  summarise(Count = n())

#chart
ggplot(gender_WestVancouver, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Male and Female in West Vancouver",
       x = "",
       y = "Count") +
  theme_bw() +
  facet_grid(. ~ Gender) +
  theme(legend.position = "right")

#10. which city have more baker
city_baker <- per_employee %>%
  group_by(City_Name) %>%
  filter(Job_Title == "Baker") %>%
  summarise(Count = n())

#graph
ggplot(city_baker, aes(x = City_Name, y = Count, fill = City_Name)) +
  geom_bar(stat = "identity", width = 0.8, color = "black") +
  labs(title = "Number of Bakers in Each City",
       x = "City",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

#question 4: why customer service is needed - add salary
#analysis:
#1. find the salary for customer service department for each person who is working now or before
base_salary <- 1800  # Base salary for customer service department
factor <- 100   # Add 100 when increase 1 year

customer_service_salary <- per_employee %>%
  filter(Department_Name == "Customer Service") %>%
  mutate(Salary = 1800 + Length_Of_Service * factor) %>%
  select(Employee_ID, Length_Of_Service, Status, Salary)

#graph
ggplot(customer_service_salary, aes(x = Length_Of_Service, y = Salary)) +
  geom_point() +
  labs(title = "Salary vs Length of Service in Customer Service Department",
       x = "Length of Service",
       y = "Salary") +
  theme_minimal()

#2. Age Distribution by Gender in Customer Service Department
age_distribution <- per_employee %>%
  filter(Department_Name == "Customer Service") %>%
  ggplot(aes(x = Age, fill = Gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Age Distribution by Gender in Customer Service Department",
       x = "Age",
       y = "Density") +
  theme_bw()

age_distribution

#3. find relationship between city, gender and status (which city have more active people)
city_active <- per_employee %>%
  filter(Status == "ACTIVE") %>%
  group_by(City_Name, Gender) %>%
  summarise(Count = n())

#graph
ggplot(city_active, aes(x = City_Name, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(x = "City", y = "Active Count", title = "Active Employees by City and Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#4. find relationship between employee id and length of service in customer service department (which employee have more length of service)
employee_customer_service <- per_employee %>%
  filter(Department_Name == "Customer Service") %>%
  select(Employee_ID, Length_Of_Service) %>%
  arrange(desc(Length_Of_Service))

#chart
ggplot(employee_customer_service, aes(x = Employee_ID, y = Length_Of_Service)) +
  geom_line() +
  labs(x = "Employee ID", y = "Length of Service", title = "Length of Service in Customer Service by Employee ID")

#5. distribution customer service department in each city
customer_service_city <- per_employee %>%
  filter(Department_Name == "Customer Service") %>%
  group_by(City_Name) %>%
  summarise(Count = n())

#graph
ggplot(customer_service_city, aes(x = City_Name, y = Count)) +
  geom_point() +
  labs(title = "Distribution of Customer Service Department in Each City",
       x = "City",
       y = "Employee Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#6. how many department Fort Nelson
department_nelson <- per_employee %>%
  filter(City_Name == "Fort Nelson") %>%
  group_by(Department_Name) %>%
  summarise(Count = n())

#graph
ggplot(department_nelson, aes(x = Department_Name, y = Count)) +
  geom_point(size = 3, color = "skyblue") +
  labs(title = "Department Distribution in Fort Nelson",
       x = "Department",
       y = "Count") +
  theme_minimal()




#7. which department have more people resignation 
department_resignation <- per_employee %>%
  filter(Termination_Reason == "Resignation") %>%
  group_by(Department_Name) %>%
  summarise(Count = n())

#graph
ggplot(department_resignation, aes(x = Department_Name, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(x = "Department", y = "Count", title = "Resignation Count by Department") +
  theme_minimal()

#8. why does the customer service department have more older employees than young employees
older_younger <- per_employee %>%
  filter(Department_Name == "Customer Service") %>%
  mutate(Salary = base_salary + Length_Of_Service * factor) %>%
  select(Employee_ID, Age, Salary)

# Calculate average salary for older employees (age >= 40) and younger employees (age < 40)
avg_salary <- older_younger %>%
  mutate(Age_Group = ifelse(Age >= 40, "Older", "Younger")) %>%
  group_by(Age_Group) %>%
  summarise(Avg_Salary = mean(Salary))

# Visualize the average salary by age group
ggplot(avg_salary, aes(x = Age_Group, y = Avg_Salary, fill = Age_Group)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Age Group", y = "Average Salary", title = "Average Salary by Age Group in Customer Service Department") +
  theme_minimal() +
  scale_fill_manual(values = c("Older" = "skyblue", "Younger" = "orange"))

#9. in customer service department, manager or cashier is more in active status?
manager_cashier <- per_employee %>%
  filter(Department_Name == "Customer Service", Status == "ACTIVE") %>%
  group_by(Job_Title) %>%
  summarise(Count = n())
manager_cashier
#graph
ggplot(manager_cashier, aes(x = Job_Title, y = Count, fill = Job_Title)) +
  geom_bar(stat = "identity") +
  labs(x = "Job Title", y = "Count", title = "Distribution Manager and Cashier In Customer Service Department") +
  theme_minimal()

#10. is active status more than terminated in 2015 in customer service department
status_year <- per_employee %>%
  filter(Status_Year == 2015, Department_Name == "Customer Service") %>%
  group_by(Status) %>%
  summarise(Count = n())

#graph
ggplot(status_year, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Status In 2015 In Customer Service Department",
       x = "Status",
       y = "Count",
       fill = "Status") +
  theme_minimal()

#question 5: which department and which store have more older people who is in active status
#analysis:
#1. employee count by job title, department, gender and store
employee_counts <- per_employee %>%
  group_by(Job_Title, Department_Name, Gender, Store_Name) %>%
  summarise(Employee_Count = n())

ggplot(employee_counts, aes(x = Gender, y = Employee_Count, color = Department_Name)) +
  geom_point() +
  labs(title = "Employee Counts by Gender and Department",
       x = "Gender",
       y = "Employee Count") 

#2. how many resignation in each business unit
resignation_count <- per_employee %>%
  group_by(Business_Unit) %>%
  summarise(Resignation_Count = sum(Termination_Reason == "Resignation"))
resignation_count

#graph
ggplot(resignation_count, aes(x = Business_Unit, y = Resignation_Count, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Business Unit", y = "Resignation Count", title = "Resignation Count by Business Unit") +
  theme_minimal()

#3. for baker, which status is more?
baker_status <- per_employee %>%
  filter(Job_Title == "Baker") %>%
  group_by(Status) %>%
  summarise(Count =n())
baker_status

#graph 
ggplot(baker_status, aes(x = Status, y = Count)) +
  geom_bar(stat = "identity", fill = "violet") +
  labs(title = "Number of Bakers by Status",
       x = "Status",
       y = "Count")

#4. which job and department have more female retirement
job_department_female <- per_employee %>%
  filter(Termination_Reason == "Retirement" & Gender == "F") %>%
  group_by(Job_Title, Department_Name) %>%
  summarise(count = n())

#graph
ggplot(job_department_female, aes(x = Job_Title, y = count, color = Department_Name)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  labs(title = "Female Retirements by Job Title and Department",
       x = "Job Title",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#5. for dairy department, resignation and retirement which is more
dairy_reason <- per_employee %>%
  filter(Department_Name == "Dairy" & 
           (Termination_Reason == "Resignation" | Termination_Reason == "Retirement")) %>%
  group_by(Termination_Reason) %>%
  summarise(Count =n())

#graph 
ggplot(dairy_reason, aes(x = Termination_Reason, y = Count, fill = Termination_Reason)) +
  geom_bar(stat = "identity") +
  labs(title = "Resignations vs. Retirements in Dairy Department",
       x = "Termination Reason",
       y = "Count") +
  theme(legend.position = "none")

#6. distribution meats department in each city and which job is more in the city
meats_city <- per_employee %>%
  filter(Department_Name == "Meats") %>%
  group_by(City_Name, Job_Title) %>%
  summarise(Count = n())

#graph 
ggplot(meats_city, aes(x = City_Name, y = Count, color = Job_Title)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7) +
  labs(title = "Meats Department Distribution by City",
       x = "City",
       y = "Department",
       color = "Job Title") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#7. which department is the most active in the year of 2012
department_2012 <- per_employee %>%
  filter(Status_Year == 2012) %>%
  group_by(Department_Name) %>%
  summarise(Count = n())
department_2012

#graph
ggplot(department_2012, aes(x = Department_Name, y = Count, fill = Department_Name)) +
  geom_bar(stat = "identity") +
  labs(title = "which department is the most active in the year of 2012",
       x = "Department Name",
       y = "Count") +
  theme(legend.position = "none")

#8. which department and job have more resignation in age between 22 - 48
resignations_age_range <- per_employee %>%
  filter(Age >= 22 & Age <= 48 & Termination_Reason == "Resignation") %>%
  group_by(Department_Name, Job_Title) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) 
resignations_age_range

#graph
ggplot(resignations_age_range, aes(x = reorder(Department_Name, Count), y = Count, 
                                   fill = Job_Title)) +
  geom_bar(stat = "identity") +
  labs(title = "Resignations by Department and Job",
       x = "Department",
       y = "Count",
       fill = "Job Title") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.title = element_blank())

#9. distribution age for retirement for male and female
age_gender_retirement <- per_employee %>%
  filter(Termination_Reason == "Retirement") %>%
  group_by(Gender) %>%
  summarise(Count = n())
age_gender_retirement

#graph
ggplot(age_gender_retirement, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Retirement Counts by Gender",
       fill = "Gender",
       x = NULL,
       y = NULL) +
  geom_text(aes(label = Count), vjust = -0.5) +
  theme_void() +
  theme(legend.title = element_blank())

#10. which store and which department will be affected by retirement
# Calculate retirement counts by store and department
retirement_counts <- per_employee %>%
  filter(Termination_Reason == "Retirement") %>%
  group_by(Store_Name, Department_Name) %>%
  summarise(Count = n())
# Identify the store and department with the highest retirement count
top_retirement <- retirement_counts %>%
  arrange(desc(Count)) %>%
  slice(1)

#graph
ggplot(retirement_counts, aes(x = Store_Name, y = Count, fill = Department_Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Retirement Counts by Store and Department",
       x = "Store",
       y = "Count",
       fill = "Department") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

