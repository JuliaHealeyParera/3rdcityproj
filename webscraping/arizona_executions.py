import requests
from bs4 import BeautifulSoup
import pandas as pd
import certifi 
import os 

headers = {
    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
}

#URL for the Arizona executions table 
url = "https://corrections.az.gov/death-row/executions-prior-1992-execution-methods"

response = requests.get(url, headers=headers, verify = 'C:/Users/amydu/AppData/Roaming/Python/Python312/site-packages/certifi/cacert.pem')
soup = BeautifulSoup(response.text, 'html.parser')

#Parsing the data 
arizona_executions = soup.find_all('table') 
individual_death = []

# print(arizona_executions) 

information = []
for row in arizona_executions: 
    if row.startswith("<td>"): 
        print(row) 
    if row != None and not row.startswith("<td>"): 
        information = []
        
    information.append(row.text.strip())
    individual_death.append(information) 
print(individual_death)

# arizona_ex_df = pd.DataFrame(individual_death, columns = ["ADC (Arizona Department of Corrections ID)", "Name", "Ethnicity", "Time", "Date"])
# data_path = os.path.join('data', 'webscraping_data', 'arizona_executions.csv')
# arizona_ex_df.to_csv(data_path, index = False)