import pandas as pd
from os import path

## read school designation file ##
## create reference dataframe that will retain only priority schools ##
ref_df = pd.read_csv("/mnt/c/Users/CA20640/Desktop/DSI_project/data_request/school_designations_file.csv")
desig = ['Comprehensive Support', 'Priority & Comprehensive Support', 'Priority Exit & Comprehensive Support']
desig_df = ref_df[ref_df['designation'].isin(desig)]
district_school_num_list = (desig_df[['system', 'school']]).values.tolist()
print "Number of Priority Schools : {}".format(len(district_school_num_list))

## convert excel file to csv file ##
input_file = "/mnt/c/Users/CA20640/Desktop/DSI_project/data_request/SAS-TDOE-Teacher-VA-by-Subject_2019.csv"
if path.exists(input_file) == False:
	init_df = pd.read_excel("/mnt/c/Users/CA20640/Desktop/DSI_project/data_request/SAS-TDOE-Teacher-VA-by-Subject_2019.xlsx")
	init_df.to_csv("/mnt/c/Users/CA20640/Desktop/DSI_project/data_request/SAS-TDOE-Teacher-VA-by-Subject_2019.csv", sep = ",", index = False)
else:
	pass

## for each year... ##
ela_subjects = ['English Language Arts', 'English I', 'English II']
math_subjects = ['Math', 'Algebra I', 'Algebra II', 'Geometry', 'Integrated Math I', 'Integrated Math II', 'Integrated Math III']
years_of_interest = ['2017', '2018', '2019']
tvaas_list = ['Level 4', 'Level 5']
for year in years_of_interest:
	input_df = pd.read_csv("/mnt/c/Users/CA20640/Desktop/DSI_project/data_request/SAS-TDOE-Teacher-VA-by-Subject_2019.csv", index_col = None)
	input_df.rename(columns = {'District Number' : 'district_no', 'School Number' : 'school_no'}, inplace = True)
	agg_teacher_cnt = []
	ela_teacher_cnt = []
	math_teacher_cnt = []
	for district_school_pair in district_school_num_list:
		csi_df = input_df.query('district_no == @district_school_pair[0] and school_no == @district_school_pair[1] and Year == @year')
		ela_df= csi_df[csi_df['Subject'].isin(ela_subjects)]
		math_df = csi_df[csi_df['Subject'].isin(math_subjects)]
		ela_teacher_list = list(ela_df[ela_df['Level'].isin(tvaas_list)]['TLN'].unique())
		math_teacher_list = list(math_df[math_df['Level'].isin(tvaas_list)]['TLN'].unique())
		agg_teacher_list = list(csi_df[csi_df['Level'].isin(tvaas_list)]['TLN'].unique()) ## some teachers teach multiple subjects, grades... only report unique teachers ##
		agg_teacher_cnt.extend(agg_teacher_list)
		ela_teacher_cnt.extend(ela_teacher_list)
		math_teacher_cnt.extend(math_teacher_list)
	print "{} : Total Number of Teachers with Level 4 or Level 5 : {}".format(year, len(set(agg_teacher_cnt)))
	print "{} : Number of ELA Teachers with Level 4 or Level 5 : {}".format(year, len(set(ela_teacher_cnt)))
	print "{} : Number of Math Teachers with Level 4 or Level 5 : {}".format(year, len(set(math_teacher_cnt)))
