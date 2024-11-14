import time 

import selenium
  
from selenium import webdriver 
  
driver = webdriver.Chrome() 
  
driver.get('https://icjia.illinois.gov/researchhub/datasets/death-in-custody-reports/') 
  
time.sleep(5) 

print(driver.title) 
  
driver.close()