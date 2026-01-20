from selenium import webdriver
from selenium.webdriver.common.by import By
import re
import pandas as pd
import requests
import time
import os

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

url = "https://www.goodreads.com/review/list/43581926-erdem-emin-ak-ay?shelf=read"

# Create an empty DataFrame
goodreads_list = pd.DataFrame(columns=[
    'book_names',
    'author_pages',
    'no_of_pages',
    'no_of_ratings',
    'avg_rating',
    'my_rating',
    'date_added',
    'date_read',
    'image_sources'
])

# Navigate to the read shelf
driver.get(url)
time.sleep(3)  # wait for the page to load

# Find all page number links in the pagination
page_links = driver.find_elements(By.XPATH, '//div[@class="inter"]//a')

# Extract the page numbers from the href attributes
page_numbers = []
for link in page_links:
    href = link.get_attribute("href")
    if href:
        match = re.search(r'(?<=page=)\d+', href)
        if match:
            page_numbers.append(int(match.group()))

# Get the maximum page number
no_of_pages = max(page_numbers) if page_numbers else 1

for i in range(1, no_of_pages + 1):
    new_url = re.sub(r"shelf=read", f"page={i}&shelf=read", url)
    driver.get(new_url)
    time.sleep(2)

    webElems1 = driver.find_elements(By.XPATH, '//td[@class="field title"]//a')
    book_names = [elem.get_attribute("title") for elem in webElems1]

    webElems2 = driver.find_elements(By.XPATH, '//td[@class="field author"]//a')
    author_pages = [elem.get_attribute("href") for elem in webElems2]

    webElems3 = driver.find_elements(By.XPATH, '//td[@class="field num_pages"]//div[@class="value"]')
    no_of_pages_list = [
        int(re.sub(r"[^\d.]", "", elem.text)) if re.sub(r"[^\d.]", "", elem.text) else None
        for elem in webElems3
    ]

    webElems4 = driver.find_elements(By.XPATH, '//td[@class="field avg_rating"]//div[@class="value"]')
    avg_rating = [
        float(re.sub(r"[^\d.]", "", elem.text)) if re.sub(r"[^\d.]", "", elem.text) else None
        for elem in webElems4
    ]

    webElems5 = driver.find_elements(By.XPATH, '//td[@class="field date_added"]//span')
    date_added = [elem.get_attribute("title") for elem in webElems5]

    webElems6 = driver.find_elements(By.XPATH, '//td[@class="field rating"]//div[@class="stars"]')
    my_rating = [elem.get_attribute("data-rating") for elem in webElems6]

    webElems7 = driver.find_elements(By.XPATH, '//td[@class="field cover"]//img')
    image_sources = [elem.get_attribute("src") for elem in webElems7]

    webElems8 = driver.find_elements(By.XPATH, '//tr[contains(@id, "review_")]')
    date_read = []
    for row in webElems8:
        try:
            span = row.find_element(By.XPATH, './/td[@class="field date_read"]//span')
            date_read.append(span.text)
        except:
            date_read.append("")

    webElems9 = driver.find_elements(By.XPATH, '//td[@class="field num_ratings"]//div[@class="value"]')
    no_of_ratings_list = [
        int(re.sub(r"[^\d.]", "", elem.text)) if re.sub(r"[^\d.]", "", elem.text) else None
        for elem in webElems9
    ]

    temp_list = pd.DataFrame({
        'book_names': book_names,
        'author_pages': author_pages,
        'no_of_pages': no_of_pages_list,
        'no_of_ratings': no_of_ratings_list,
        'avg_rating': avg_rating,
        'my_rating': my_rating,
        'date_added': date_added,
        'date_read': date_read,
        'image_sources': image_sources
    })

    goodreads_list = pd.concat([goodreads_list, temp_list], ignore_index=True)

# Download book www (downloads www to your computer, used later in plots)

if not os.path.exists("www"):
    os.makedirs("www")

goodreads_list['image_name'] = ""

for idx, row in goodreads_list.iterrows():
    image_url = row['image_sources']
    image_name = f"www/cover_{idx+1}.png"
    goodreads_list.at[idx, 'image_name'] = image_name
    if image_url:
        try:
            response = requests.get(image_url, stream=True)
            if response.status_code == 200:
                with open(image_name, 'wb') as f:
                    for chunk in response.iter_content(1024):
                        f.write(chunk)
        except Exception as e:
            print(f"Failed to download {image_url}: {e}")
    time.sleep(1)

# Unique authors
authors = goodreads_list[['author_pages']].drop_duplicates().reset_index(drop=True)
authors['name'] = ""

# Get names for each author, might take long depending on how many unique authors one has
for idx, row in authors.iterrows():
    author_url = row['author_pages']
    if author_url:
        driver.get(author_url)
        time.sleep(3)
        try:
            webElem = driver.find_element(By.XPATH, '//h1[@class="authorName"]//span[@itemprop="name"]')
            authors.at[idx, 'name'] = webElem.text
        except Exception as e:
            print(f"Failed to get author name from {author_url}: {e}")
    time.sleep(1)

# Close Selenium
driver.quit()

# Add author names to the goodreads_list by merging on 'author_pages'
goodreads_list = goodreads_list.merge(authors, on="author_pages", how="left")

# Convert my_rating to numeric
goodreads_list['my_rating'] = pd.to_numeric(goodreads_list['my_rating'], errors='coerce')

# Assign my_rating as rounded average if it is 0
mean_rating = round(goodreads_list['my_rating'].mean(), 0)
goodreads_list['my_rating'] = goodreads_list['my_rating'].apply(
    lambda x: mean_rating if x == 0 or pd.isna(x) else x
)

goodreads_list.to_csv("goodreads_list_erdem.csv", index=False)