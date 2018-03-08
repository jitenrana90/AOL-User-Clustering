# Exploratory Data Analysis of AOL User Search Queries and Clustering

Data Set : 
http://www.cim.mcgill.ca/~dudek/206/Logs/AOL-user-ct-collection/user-ct-test-collection-02.txt.gz
Above is the link to download dataset

# Exploratory Data Analysis:
The code for this part is in the file EDA_Code.R
It extracts the following high level insights at two levels (query level and user level)
# At User Level:
  a. Extract average click rate
  
  b. Average no of queries per user
  
  c. Average session duration of a user
  
  d. Average No of page view by a User

# At Query Level:
  a. Average length of a query
  
  b. Average length of a query with high click rate and low click rate

# User Clustering based on Search Queries:
The code for this part is in the file Clustering_Code.R
The aim is to identify following things:
  a. User Interest Vector
  
  b. Cluster the users with similar interests

# Approach for clustering :
a. Employed Latent Dirichlet Allocation (LDA) to discover latent topics prevalent in search queries \n
b. Model Topics as distribution over search keywords
c. Identify Topics Users are intested in
d. Cluster users with similar interests with Jensen-Shannon Divergence (JSD) as a distance metrics between users
