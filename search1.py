import string

def cleanToken(token):
    is_alphabet = any(c.isalpha() for c in token)
    cleaned_token = "".join(c for c in token if c.isalpha())
    return cleaned_token.lower() if is_alphabet else ""

def buildInvertedIndex(content):
    inverted_index = {}  
    pages = content.split("<endPageBody>")
    sum_count = 0
    page_count = 0
    for page in pages:
        body = page.replace("<pageBody>", "")
        tokens = body.split()
        if tokens:
            url, unique_tokens = tokens[0], set()
            for token in tokens[1:]:
                cleaned_token = cleanToken(token)
                if cleaned_token:
                    unique_tokens.add(cleaned_token)
                    
            if unique_tokens:
                sum_count += len(unique_tokens)
                inverted_index[url] = unique_tokens  
            page_count += 1
    print(f"Stand by while building index... \n\nIndexed {page_count} pages containing {sum_count} unique terms.")
    return inverted_index  

def findQueryMatches(index, query):
    query = query.lower()
    query_terms = query.split()
    matching_urls = set(index.keys()) if index is not None else set()
    result = set()
    for term in query_terms:
        operator = ""
        search_word = term.strip()
        for char in search_word:
            if char in string.punctuation:
                operator += char
                search_word = search_word.lstrip(char)

        term_matches = set()
        for url, tokens in index.items():
            if search_word in tokens:
                term_matches.add(url)

        if operator == "+":
            result.intersection_update(term_matches)
        elif operator == "-":
            result.difference_update(term_matches)
        else:
            result.update(term_matches)

    return result

def readDocs(dbfile):
    with open(dbfile, 'r', encoding="utf8") as file:
        content = file.read() 
    return content  

def mySearchEngine(dbfile):
    content = readDocs(dbfile)
    if content is None:
        print("Failed to read the document.")
        return
    
    inverted_index = buildInvertedIndex(content)
    while True:
        query = input("\nEnter query sentence (RETURN/ENTER to quit): ")
        if not query:
            break
        matches = findQueryMatches(inverted_index, query)
        print(f"Found {len(matches)} matching pages:\n")
        for match in matches:
            print(match)

if __name__ == "__main__":
    mySearchEngine("\\Users\\lomad\\Downloads\\sampleWebsiteData.txt")
