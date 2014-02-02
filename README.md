sotus-topicmodels-and-timeseries
================================

topicmodel sand timeseries of SOTUSes
PURPOSE: investigate topic models in context of a collection of State of the Union speeches (SOTUS) by way of tracking the evolution of the United States from an agrarian colonial backwater through various stages of Empire to global corporate/state hegemon.


  * UCSB_scraper.R scrapes all presidential material from UCSB website on US presidents: http://www.presidency.ucsb.edu/
  * KNN_SOTUS.R 	                investigate which texts most closely resemble other texts using K-Nearest Neighbor
  * SOTUS_Hclust.R                performs hierarchical clustering of SOTUS addresses (similarity metric)
  * SOTUS_topicmodels_all.R 	    creates topic models of all speeches
	
TO DO:
  * create SOTUS_topicmodels_RvsD.R      create topic models of speeches by party
  * create SOTUS_topicmodels_decade.R    create topic models of speeches by decade
  * create SOTUS_topicmodels_era.R       create topic models of speeches by era: early Republic, Jacksonian,     reconstruction, guilded, progressive, roaring teens and twenties, depression, post-war prosperity, (current) neoliberal eras
  * improve scraper to be able to filter and capture more metadata (author/date/party affiliation, etc.)
  * create annotated d3.js graphs for display purposes

	                               
	                               
	                             
	                               
	                               
