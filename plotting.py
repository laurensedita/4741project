import numpy as np
import matplotlib.pyplot as plt
import pandas as pd


def plot_unemp_bar(df,saveloc):
	plt.figure(num=None, figsize=(8, 6), dpi=80, facecolor='w', edgecolor='k')
	unemp = df.loc[:,'UNEMP'] == 1
	unemp = len(unemp[unemp].index)
	emp = df.loc[:,'UNEMP'] == 0
	emp = len(emp[emp].index)
	num_people = len(df.index)
	labels = ['Unemployed','Always Employed']
	sizes = [unemp,emp]
	colors = ['lightskyblue', 'lightcoral']
	plt.pie(sizes, labels=labels, colors=colors,autopct='%1.1f%%', shadow=False, startangle=90)
	plt.axis('equal')
	plt.title('Percent of People Whom Have Experienced Unemployment')
	plt.savefig(saveloc+'unemp_pie')
	plt.clf()

def autolabel(rects,labels, ax):
    # Get y-axis height to calculate label position from.
    (y_bottom, y_top) = ax.get_ylim()
    y_height = y_top - y_bottom

    for rect,label in zip(rects,labels):
        height = rect.get_height()
        label_position = height + (y_height * 0.01)

        ax.text(rect.get_x() + rect.get_width()/2., label_position,
                '%1.1f%%' % label,
                ha='center', va='bottom')


def plot_unemp_bycategory(df,saveloc,category,category_list,column_name,savename):

	plt.figure(num=None, figsize=(12, 6), dpi=80, facecolor='w', edgecolor='k')
	industries = category_list
	sizes = np.zeros((3,len(industries)))
	for i in np.arange(len(industries)):
		total = df.loc[:,column_name]==industries[i]
		unemp = df.loc[total[total].index,'UNEMP'] ==1
		emp = df.loc[total[total].index,'UNEMP'] ==0
		sizes[0,i] = len(total[total].index)
		sizes[1,i] = len(unemp[unemp].index)
		sizes[2,i] = len(emp[emp].index)

	sizes2 = sizes[0,:]
	plt.pie(sizes2, labels=industries,autopct='%1.1f%%', shadow=False, startangle=90)
	plt.axis('equal')
	plt.title(category+' Makeup')
	plt.savefig(saveloc+savename+'_pie')
	plt.clf()

	n_groups = len(industries)
	means_emp = sizes[2,:]
	means_unemp = sizes[1,:]
	fig, ax = plt.subplots()
	index = np.arange(n_groups)

	bar_width = .35
	
	opacity = 0.8

	rects1 = plt.bar(index, means_emp, bar_width,
	                 alpha=opacity,
	                 color='b',
	                 label='Employed')
	 
	rects2 = plt.bar(index + bar_width, means_unemp, bar_width,
	                 alpha=opacity,
	                 color='g',
	                 label='Unemployed')
	label1 = [100*sizes[2,i]/float(sizes[0,i]) for i in np.arange(len(industries))]
	label2 = [100*sizes[1,i]/float(sizes[0,i]) for i in np.arange(len(industries))]
	autolabel(rects1,label1, ax)
	autolabel(rects2,label2, ax)
	plt.xlabel(category)
	plt.ylabel('People')
	plt.title('Unemployment by '+category)
	plt.xticks(index + bar_width/2.0, industries)
	plt.legend()

	plt.tight_layout()
	plt.savefig(saveloc+savename+'_unemp_bar')
	plt.clf()

saveloc = '/home/kristine/Documents/17-18/4741project/figures/'
filename = '/home/kristine/Documents/17-18/4741project/final_var/var_string_hgr_null.csv'
file_name = '/home/kristine/Documents/17-18/4741project/final_var/var_string_hgr_null.pkl'
df = pd.read_pickle(file_name)
f = open(filename,'r')
line = f.readlines(1)
qlist = line[0].strip().split(';')
df.columns = qlist

#REGION
#category = 'Region'
#category_list = ['Northeast','North central','South','West']
#column_name = 'REGION'
#savename = 'region'

#GENDER
#category= 'Gender'
#category_list = ['Male','Female']
#column_name = 'SAMPLE_SEX'
#savename = 'gender'

#RACE
#category = 'Race'
#category_list = ['Non-Black, Non-Hispanic','Black','Hispanic']
#column_name = 'SAMPLE_RACE'
#savename = 'race'

#EDUCATION
#category = 'Education'
#category_list = ['Less than high school','High school','College']
#column_name = 'HIGHESTGRADE'
#savename = 'education'

#INDUSTRY
category = 'Industry'
category_list = ['Agriculture, Forestry, Fishing, and Hunting','Mining','Construction','Manufacturing','Transportation, Communication, Public Utilities','Wholesale and Retail Trade','Finance, Insurance, and Real Estate','Business and Repair Services','Personal Services','Entertainment and Recreation Services','Professional and Related Services','Public Administration']
column_name = 'INDUSTRY'
savename = 'industry'

#MARRIAGE
#category = 'Marriage Status'
#category_list = ['Never married','Married, together','Other']
#column_name = 'MARSTAT_COL'
#savename = 'marriage'

#URBAN
#category = 'Urban vs Rural'
#category_list = ['Urban','Rural']
#column_name = 'URBAN_RURAL'
#savename = 'urban'

#HEALTH
#category = 'Health Limit'
#category_list = ['No','Yes']
#column_name = 'HEALTHLIMIT'
#savename = 'health'

plot_unemp_bycategory(df,saveloc,category,category_list,column_name,savename)


df_male = df.loc[:,'SAMPLE_SEX'] == 'Male'

#df_male_rural = df.loc[df_male[df_male].index,'URBAN_RURAL']=='Rural'
#df_male_urban = df.loc[df_male[df_male].index,'URBAN_RURAL']=='Urban'
#print(len(df_male_rural[df_male_rural].index))
#print(len(df_male_rural[df_male_urban].index))

df_male = df.loc[df_male[df_male].index]
df_male_null = df_male.loc[:,'HIGHESTGRADE'] != 'null'
df_male_null = df_male.loc[df_male_null[df_male_null].index]

plot_unemp_bycategory(df_male,saveloc,category+' for Males Only',category_list,column_name,savename+'_men')

plot_unemp_bar(df_male,saveloc+'male')


df_female = df.loc[:,'HIGHESTGRADE'] != 'null'
df_female_null = df.loc[df_female[df_female].index]
#df_female_null.to_csv('/home/kristine/Documents/17-18/4741project/final_var/all.csv', index=False, header=True,columns = qlist,sep=';')


#df_male_null.to_csv('/home/kristine/Documents/17-18/4741project/final_var/male_only_null.csv', index=False, header=True,columns = qlist,sep=';')



