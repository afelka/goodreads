# -*- coding: utf-8 -*-
"""
Created on Fri Feb  9 10:27:25 2024

@author: Erdem Emin Akcay
"""

from selenium import webdriver
from selenium.webdriver.common.by import By
import re
import pandas as pd
import requests
import os
import time
import geopandas as gpd
import matplotlib.pyplot as plt

#url to goodreads account
my_url = "https://www.goodreads.com/review/list/102002329?"

#start chrome driver
driver = webdriver.Chrome()

#sign in to goodreads
driver.get("https://www.goodreads.com/user/sign_in")

sign_in_with_email = driver.find_element("css selector", ".authPortalSignInButton")
sign_in_with_email.click()

# Find elements by CSS selector
user_email = driver.find_element("css selector", "#ap_email")
user_password = driver.find_element("css selector", "#ap_password")

# Use the send_keys method to input data
email_input = input("Enter your email: ")
password_input = input("Enter your password: ")

user_email.send_keys(email_input)
user_password.send_keys(password_input)

sign_in_submit = driver.find_element("css selector", "#signInSubmit")
sign_in_submit.click()

#after signin go to goodreads book shelf
driver.get(my_url)

#find number of pages
web_elements = driver.find_elements(By.XPATH, '//div[@class="inter"]//a')
    
# Extract href attributes and retrieve the maximum page number
max_no_of_pages = max(
    map(lambda x: int(re.search(r'(?<=page=)\s*-?[0-9.]+', x.get_attribute("href")).group()), web_elements)
)

#create empty table
goodreads_list = pd.DataFrame(columns=['book_names', 'author_pages', 'no_of_pages', 'avg_rating', 'date_added', 'image_sources'])

#create function to get numeric from some of the texts
def extract_numeric(text):
    return re.sub(r'[^0-9.]', '', text) if text else None

#go to each page and get different information about the books 
for i in range(1, max_no_of_pages + 1):
    # Create the new page dynamically
    new_url = f"{my_url}page={i}"

    # Navigate to the new page
    driver.get(new_url)
    
    web_elements0 = driver.find_element("css selector","#shelfSettingsLink")
    web_elements0.click()
    
    num_pages_field = driver.find_element("css selector","#num_pages_field")
    num_pages_field.click()

    # Find elements using XPath
    web_elements1 = driver.find_elements(By.XPATH, '//td[@class="field title"]//a')
    book_names = [elem.get_attribute("title") for elem in web_elements1]

    web_elements2 = driver.find_elements(By.XPATH, '//td[@class="field author"]//a')
    author_pages = [elem.get_attribute("href") for elem in web_elements2]

    web_elements3 = driver.find_elements("css selector",".num_pages")
    no_of_pages = [extract_numeric(elem.text) for elem in web_elements3][1:]
    
    web_elements4 = driver.find_elements(By.XPATH, '//td[@class="field avg_rating"]//div[@class="value"]')
    avg_rating = [extract_numeric(elem.text) for elem in web_elements4]

    web_elements5 = driver.find_elements(By.XPATH, '//td[@class="field date_added"]//span')
    date_added = [pd.to_datetime(elem.get_attribute("title"), format="%B %d, %Y") for elem in web_elements5]
            
    web_elements6 = driver.find_elements(By.XPATH, '//td[@class="field cover"]//img')
    image_sources = [elem.get_attribute("src") for elem in web_elements6]

    # Create a temporary DataFrame
    temp_list = pd.DataFrame({
        'book_names': book_names,
        'author_pages': author_pages,
        'no_of_pages': no_of_pages,
        'avg_rating': avg_rating,
        'date_added': date_added,
        'image_sources': image_sources
    })

    # Append the temporary DataFrame to the main DataFrame
    goodreads_list = pd.concat([goodreads_list, temp_list], ignore_index=True)
    print(i)

#download book covers 
for i in range(len(goodreads_list)):
    image_url = goodreads_list.iloc[i, 5]  

    image_name = f"cover_{i + 1}.png"
    goodreads_list.loc[i, 'image_name'] = image_name

    response = requests.get(image_url)
    
    with open(image_name, 'wb') as file:
        file.write(response.content)
        
# Select the 'author_pages' column and get unique values as a DataFrame
authors = pd.DataFrame({'author_pages': goodreads_list['author_pages'].unique()})

# Add an empty 'name' column
authors['name'] = ""


# Loop through the rows of the 'authors' DataFrame
for i in range(len(authors)):
    author_url = authors.iloc[i, 0]  

    # Navigate to the author's URL
    driver.get(author_url)

    time.sleep(3)
    
    # Find the author name element using XPath
    web_elem = driver.find_element(By.XPATH, '//h1[@class="authorName"]//span[@itemprop="name"]')

    # Update the 'name' column in the 'authors' DataFrame
    authors.loc[i, 'name'] = web_elem.text

# Close WebDriver
driver.quit()

#write goodreads_list to csv
goodreads_list.to_csv("goodreads_list.csv",index=False )

#write authors to csv file and find countries for authors manually and read csv back
authors.to_csv('authors.csv', index=False)
