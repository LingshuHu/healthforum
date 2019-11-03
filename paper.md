---
title: 'healthforum: A R package for scraping health forum discussion threads'
tags:
  - R
  - health information
  - scrapper
authors:
  - name: Lingshu Hu
    orcid: 0000-0003-0304-882X
    affiliation: 1
  - name: Michael W. Kearney
    orcid: 0000-0002-0730-4694
    affiliation: 1
affiliations:
 - name: Missouri School of Journalism, University of Missouri
   index: 1
date: 20 October 2019
bibliography: paper.bib
---

# Summary

Online health forums have increasingly become an important resource for individuals to obtain health and medical information in recent years [@Holland:2019; @Lovatt:2017]. For example, in the United States, one-third of adults reported that they go to online health forums to figure out their health conditions before, during, or after they visit professional doctors  [@Pew:2013]. Because of their importance, studying information on health forums is necessary for health communication scholars. A wealth of studies have probed this area [@Sudau:2014; @Lovatt:2017; @Pan:2017]. 

Previous studies mainly used qualitative textual analysis [@Sudau:2014] and focused on a specific type of online health forums, such as cancer forums [@Liess:2008], or mental health forums[@Kummervold:2002]. A beneficial next step would be using quantitative methods to examine a broader scope of health topics in a relatively more comprehensive online health forum. However, because of the large volume of information on health forums, traditional research methods used by communication scholars, such as discourse analysis or content analysis, can hardly depict the big map of people's behaviors on online health forums.  

``healthforum`` provides researchers the possibility to investigate the large volume of health information on an online forum. It is an R package that facilitates scholars to scrape online health discussion threads from the health forum *Patient* (<https://patient.info/forums>). *Patient* was rated as one of five best health websites by the Times in the UK [@Porter:2013], and according to Alexa Internet Inc [-@Alexa:2019], *Patient* forum has the highest traffic among all English health forums. In addition, *Patient* forum is an open access website, which means people can read its information without registering. Therefore, the popularity and accessibility make *Patient* forum a good representative case for online health forums.

``healthforum`` can be downloaded from CRAN. It works in the R environment (RCoreTeam, 2018) and is dependent on the rvest package [@Wickham:2016]. R language and APIs are widely used in the social science field. ``healthforum`` thus can be easily adopted by health communication scholars or researchers who are interested in user-generated health information. It has been used by an ongoing research project which examines the influence of text features on the interactivity and likeability of posts.

Using ``healthforum``, researchers can obtain information including the date and text of posts, the counts of likes and replies of posts, the reply relationship between users, the disease category of a post, and user profiles. Plus,  ``healthforum`` also includes a medical glossary dictionary to count the number of medical words used in each post. Using the above information, researchers can do text analysis, text classification, social network analysis, etc. to study people's behavioral traces on the online health forum. 

# Reference
