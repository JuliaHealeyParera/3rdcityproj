import requests
from bs4 import BeautifulSoup
import pandas as pd
import certifi 
import os 

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

#URL for the Arkansas executions table 
url = "https://doc.arkansas.gov/correction/inmates/executions/"

response = requests.get(url, headers=headers, verify = 'C:/Users/amydu/AppData/Roaming/Python/Python312/site-packages/certifi/cacert.pem')
soup = BeautifulSoup(response.text, 'html.parser')

#Parsing the data 
arkansas_executions = soup.find('table')
individual_death = []

for row in arkansas_executions.find_all('tr'): 
    td_tags = row.find_all('td')
    
    information = []
    if len(td_tags) > 0: 
        for i, characteristic in enumerate(td_tags): 
            if (i == 1): 
                race_sex = characteristic.text.strip() 
                race, sex = race_sex.split("/")                     
                information.append(race) 
                information.append(sex) 
                continue 
            information.append(characteristic.text.strip())
    individual_death.append(information)

arkansas_ex_df = pd.DataFrame(individual_death[1:], columns = ["ind_name", "ind_race", "ind_gender", "ind_age", "ind_county", "ind_offense", "ind_dod"])
data_path = os.path.join('data', 'webscraping_data', 'arkansas_executions.csv')
arkansas_ex_df.to_csv(data_path, index = False) 