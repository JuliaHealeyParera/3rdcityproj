import requests
from bs4 import BeautifulSoup
import pandas as pd
import certifi 
import os 

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

#URL for the Tennessee executions table
url = "https://www.tn.gov/correction/statistics/executions/tennessee-executions.html"

response = requests.get(url, headers = headers, verify = 'C:/Users/amydu/AppData/Roaming/Python/Python312/site-packages/certifi/cacert.pem')
soup = BeautifulSoup(response.text, 'html.parser') 

#Parsing the data
tennessee_executions = soup.find('table') 
individual_death = [] 
addition = True

for row in tennessee_executions.find_all('tr'): 
    td_tags = row.find_all('td')
    
    information = []
    if len(td_tags) > 0: 
        for characteristic in td_tags: 
            if characteristic.text.strip() == '': 
                addition = False
            information.append(characteristic.text.strip())
    if addition is False: 
        addition = True
        continue
    individual_death.append(information) 

tennessee_ex_df = pd.DataFrame(individual_death[2:], columns = ["Name", "Race", "Offense", "County", "Date Executed"])
data_path = os.path.join('data', 'webscraping_data', 'tennessee_executions.csv')
tennessee_ex_df.to_csv(data_path, index = False) 
    

